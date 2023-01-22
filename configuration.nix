# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{
  imports = [
    ./hardware-configuration.nix
    ./dnsmasq.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  console.useXkbConfig = true;

  services.openssh = {
    enable = true;
    permitRootLogin = "prohibit-password";
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };
}
