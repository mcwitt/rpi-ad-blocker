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
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , ...
    } @ inputs: {

      nixosConfigurations.rpi3 = nixpkgs.lib.nixosSystem rec {
        system = "aarch64-linux";
        modules = [ self.nixosModules.default ];
        specialArgs = { inherit (self.packages.${system}) blocked-hosts; };
      };

      nixosModules.default = import ./configuration.nix;

    } // flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in
    rec {
      packages = rec {
        default = blocked-hosts;

        sanitize-blocklist = pkgs.haskellPackages.callPackage ./sanitize-blocklist { };

        blocked-hosts = pkgs.runCommand "blocked-hosts" { } ''
          ${sanitize-blocklist} < ${inputs.hosts-blocklists}/dnsmasq/dnsmasq.blacklist.txt > $out
        '';
      };

      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt = {
              enable = true;
              excludes = [ "hardware-configuration\\.nix" ];
            };
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
