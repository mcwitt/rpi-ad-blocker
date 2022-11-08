{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hosts-blocklists = {
      url = "github:notracking/hosts-blocklists";
      flake = false;
    };
  };

  outputs = inputs @ { nixpkgs, ... }: {
    nixosConfigurations.pinto = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [ ./configuration.nix ];
      specialArgs = { inherit inputs; };
    };
  };
}
