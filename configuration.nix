# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./dnsmasq.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "rpi3";

  time.timeZone = "America/Los_Angeles";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  environment.systemPackages = [ pkgs.vim ];

  services.openssh = {
    enable = true;
    permitRootLogin = "without-password";
  };

  system.stateVersion = "22.05";

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  nix.settings.trusted-public-keys = [ "golem:ccFn2QC8Jpctrhlv6Z7SCXYJnvl1eJcvWpLb9tJ/Gck=" ];
}
