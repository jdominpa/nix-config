{
  flake.modules.homeManager.zen-browser-search =
    { pkgs, ... }:
    {
      programs.zen-browser.profiles.default.search = {
        force = true;
        default = "ecosia";
        privateDefault = "ecosia";
        order = [
          "NixOS Packages"
          "NixOS Options"
          "NixOS Wiki"
          "Home Manager"
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
              definedAliases = [
                "@np"
                "@nixpkgs"
              ];
              icon = nix-icon;
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
              definedAliases = [
                "@no"
                "@nixopts"
              ];
              icon = nix-icon;
            };
            "NixOS Wiki" = {
              urls = [
                {
                  template = "https://wiki.nixos.org/w/index.php";
                  params = [
                    {
                      name = "search";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
              definedAliases = [
                "@nw"
                "@nixwiki"
              ];
              icon = nix-icon;
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
              definedAliases = [
                "@hm"
                "@home"
              ];
              icon = nix-icon;
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
              icon = "https://lean-lang.org/static/svg/lean-logo-official-TM-transparent-2400x900.svg";
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
              definedAliases = [
                "@maps"
                "@gmaps"
              ];
              icon = "https://www.google.com/images/branding/product/ico/maps15_bnuw3a_32dp.ico";
            };
            "bing".metaData.hidden = true;
            "ddg".metaData.hidden = true;
            "ebay".metaData.hidden = true;
            "perplexity".metaData.hidden = true;
          };
      };
    };
}
