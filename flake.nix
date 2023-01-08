{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    hosts-blocklists = {
      url = "github:notracking/hosts-blocklists";
      flake = false;
    };
  };

  outputs = inputs @ { self, nixpkgs, flake-utils, ... }: {

    overlay = final: _: {
      sanitize-blocklist = final.haskellPackages.callPackage ./sanitize-blocklist { };

      blocked-hosts = final.runCommand "blocked-hosts" { } ''
        ${final.sanitize-blocklist} < ${inputs.hosts-blocklists}/dnsmasq/dnsmasq.blacklist.txt > $out
      '';
    };

    nixosConfigurations.pinto = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        { nixpkgs.overlays = [ self.overlay ]; }
        ./configuration.nix
      ];
      specialArgs = { inherit inputs; };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
    in
    rec {
      packages = { inherit (pkgs) blocked-hosts sanitize-blocklist; };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.sanitize-blocklist ];
        packages = with pkgs; [
          haskellPackages.haskell-language-server
          ormolu
        ];
      };
    });
}
