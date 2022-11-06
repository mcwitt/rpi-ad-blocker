# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{ lib, pkgs, ... }:

let dnsmasqLog = "/var/log/dnsmasq.log";
in
{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "pinto";

  time.timeZone = "America/Los_Angeles";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  environment.systemPackages = [ pkgs.vim ];

  services.openssh.enable = true;

  system.stateVersion = "22.05";

  services.dnsmasq =
    let
      cloudflare = [
        # https://developers.cloudflare.com/1.1.1.1/setup/linux/
        "1.1.1.1"
        "1.0.0.1"
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];

      google = [
        # https://developers.google.com/speed/public-dns/docs/using#linux
        "8.8.8.8"
        "8.8.4.4"
        "2001:4860:4860::8888"
        "2001:4860:4860::8844"
      ];

    in
    {
      enable = true;
      servers = lib.concatLists [ cloudflare google ];

      extraConfig =
        let hostsUrl = "https://raw.githubusercontent.com/notracking/hosts-blocklists/master/dnsmasq/dnsmasq.blacklist.txt";
        in ''
          domain-needed
          bogus-priv
          no-resolv
          no-poll
          log-queries

          cache-size=10000
          local-ttl=300
          log-facility=${dnsmasqLog}

          conf-file=${builtins.fetchurl hostsUrl}
        '';
    };

  services.logrotate = {
    enable = true;
    settings.dnsmasq = {
      enable = true;
      files = [ dnsmasqLog ];
      frequency = "daily";
    };
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
