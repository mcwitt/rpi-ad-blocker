{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hosts-blocklists = {
      url = "github:notracking/hosts-blocklists";
      flake = false;
    };
  };

  outputs = inputs @ { self, nixpkgs, ... }: {

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
  } // (
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
    in
    {
      packages.${system} = { inherit (pkgs) blocked-hosts sanitize-blocklist; };
    }
  );
}
