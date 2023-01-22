# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./dnsmasq.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  time.timeZone = "America/Los_Angeles";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

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
}
