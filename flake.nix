{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    hosts-blocklists = {
      url = "github:notracking/hosts-blocklists";
      flake = false;
    };
  };

  outputs =
    inputs @ { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , ...
    }: {

      nixosConfigurations.pinto = nixpkgs.lib.nixosSystem rec {
        system = "aarch64-linux";
        modules = [ ./configuration.nix ];
        specialArgs = { inherit (self.packages.${system}) blocked-hosts; };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in
    rec {
      packages = rec {
        sanitize-blocklist = pkgs.haskellPackages.callPackage ./sanitize-blocklist { };

        blocked-hosts = pkgs.runCommand "blocked-hosts" { } ''
          ${sanitize-blocklist} < ${inputs.hosts-blocklists}/dnsmasq/dnsmasq.blacklist.txt > $out
        '';
      };

      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          excludes = [ "hardware-configuration\\.nix" ];
          hooks = {
            nixpkgs-fmt.enable = true;
          };
        };
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.sanitize-blocklist ];
        packages = with pkgs; [
          haskellPackages.haskell-language-server
          ormolu
        ];
        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };
    });
}
