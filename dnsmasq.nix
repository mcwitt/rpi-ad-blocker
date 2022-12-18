{ lib, pkgs, ... }:
let logfile = "/var/log/dnsmasq.log";
in
{
  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];

  services.dnsmasq =
    {
      enable = true;

      settings = {
        server =
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
          lib.concatLists [ cloudflare google ];

        domain-needed = true;
        bogus-priv = true;
        no-resolv = true;
        no-poll = true;
        log-queries = true;
        cache-size = 10000;
        local-ttl = 300;
        log-facility = logfile;
        conf-file = toString pkgs.blocked-hosts;
      };
    };

  services.logrotate = {
    enable = true;
    settings.dnsmasq = {
      enable = true;
      files = [ logfile ];
      rotate = 5;
      frequency = "daily";

      # required for dnsmasq to update logfile inode; otherwise will
      # keep writing to archived log
      postrotate = "systemctl restart dnsmasq.service";
    };
  };
}
