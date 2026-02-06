{
  flake.modules.homeManager.zen-browser-search =
    { pkgs, ... }:
    {
      programs.zen-browser.profiles.default.search = {
        force = true;
        default = "google";
        privateDefault = "ddg";
        order = [
          "NixOS Packages"
          "NixOS Options"
          "Home Manager"
          "Noogle"
          "Lean Search Engine"
          "Google Maps"
        ];
        engines =
          let
            nix-icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
          in
          {
            "NixOS Packages" = {
              urls = [
                {
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "type";
                      value = "packages";
                    }
                    {
                      name = "channel";
                      value = "unstable";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              icon = nix-icon;
              definedAliases = [
                "@np"
                "@nixpkgs"
              ];
            };
            "NixOS Options" = {
              urls = [
                {
                  template = "https://search.nixos.org/options";
                  params = [
                    {
                      name = "channel";
                      value = "unstable";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              icon = nix-icon;
              definedAliases = [
                "@no"
                "@nixopts"
              ];
            };
            "Home Manager" = {
              urls = [
                {
                  template = "https://home-manager-options.extranix.com";
                  params = [
                    {
                      name = "release";
                      value = "master";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              icon = nix-icon;
              definedAliases = [
                "@hm"
                "@home"
              ];
            };
            "Noogle" = {
              urls = [ { template = "https://noogle.dev/q?term={searchTerms}"; } ];
              icon = nix-icon;
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [
                "@noogle"
                "@ng"
              ];
            };
            "Lean Search Engine" = {
              urls = [
                {
                  template = "https://leansearch.net";
                  params = [
                    {
                      name = "q";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              definedAliases = [ "@lean" ];
            };
            "Google Maps" = {
              urls = [
                {
                  template = "https://maps.google.com";
                  params = [
                    {
                      name = "q";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              icon = "https://www.google.com/images/branding/product/ico/maps15_bnuw3a_32dp.ico";
              definedAliases = [
                "@maps"
                "@gmaps"
              ];
            };
            "bing".metaData.hidden = true;
            "ebay".metaData.hidden = true;
            "Perplexity".metaData.hidden = true;
          };
      };
    };
}
