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
    , hosts-blocklists
    } @ inputs: {

      nixosConfigurations.rpi-ad-blocker = nixpkgs.lib.nixosSystem rec {
        system = "aarch64-linux";
        modules = [
          self.nixosModules.default
          ({ pkgs, ... }: {
            environment.systemPackages = [ pkgs.vim ];
            i18n.defaultLocale = "en_US.UTF-8";
            networking.hostName = "rpi3";
            nix.settings.trusted-public-keys = [ "golem:ccFn2QC8Jpctrhlv6Z7SCXYJnvl1eJcvWpLb9tJ/Gck=" ];
            system.stateVersion = "22.11";
            time.timeZone = "America/Los_Angeles";

            users.users.matt = {
              isNormalUser = true;
              extraGroups = [ "wheel" ];
            };
          })
        ];
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
          ${sanitize-blocklist} < ${hosts-blocklists}/dnsmasq/dnsmasq.blacklist.txt > $out
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
      } // packages;

      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.sanitize-blocklist ];
        packages = with pkgs; [
          haskellPackages.haskell-language-server
          ormolu
        ];
        inherit (checks.pre-commit-check) shellHook;
      };
    });
}
