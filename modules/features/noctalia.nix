{
  config,
  self,
  ...
}:
let
  inherit (config.user) homeDirectory;
in
{
  flake.modules.nixos.noctalia = { pkgs, ... }: {
    programs.noctalia = {
      enable = true;
      package = self.wrappers.noctalia.wrap { inherit pkgs; };
      systemd.enable = true;
    };
  };

  flake.wrappers.noctalia =
    {
      config,
      lib,
      pkgs,
      wlib,
      ...
    }:
    let
      tomlFmt = pkgs.formats.toml { };
    in
    {
      imports = [ wlib.modules.default ];

      options = {
        generatedConfigDirname = lib.mkOption {
          type = lib.types.str;
          default = "${config.binName}";
          description = "Name of the directory which is created as the NOCTALIA_CONFIG_HOME in the wrapper output";
          apply = x: lib.removePrefix "/" (lib.removeSuffix "/" x);
        };
        configDrvOutput = lib.mkOption {
          type = lib.types.str;
          default = config.outputName;
          description = "Name of the derivation output where the generated NOCTALIA_CONFIG_HOME is output to.";
        };
        configPlaceholder = lib.mkOption {
          type = lib.types.str;
          default = "${placeholder config.configDrvOutput}/${config.generatedConfigDirname}";
          readOnly = true;
          description = ''
                The placeholder for the generated config directory.

            Use this inside the module to place files in an ad-hoc manner within it.

            Outside of the module, you should instead use `wrapped-noctalia.generatedConfig` to get the path.
          '';
        };
        settings = lib.mkOption {
          type = wlib.types.structuredValueWith { typeName = "TOML"; };
          default = { };
          description = ''
                Noctalia configuration settings as an attribute set,
            to be written to $NOCTALIA_CONFIG_HOME/noctalia/settings.toml`.
          '';
        };
        colors = lib.mkOption {
          type = wlib.types.structuredValueWith { typeName = "TOML"; };
          default = { };
          description = ''
            Noctalia color configuration as an attribute set
          '';
        };
      };

      config = {
        env = {
          NOCTALIA_CONFIG_HOME = "${placeholder config.configDrvOutput}";
        };
        constructFiles.settings = {
          content = builtins.readFile (
            tomlFmt.generate config.constructFiles.settings.relPath config.settings
          );
          output = lib.mkOverride 0 config.configDrvOutput;
          relPath = lib.mkOverride 0 "noctalia/settings.toml";
        };
        constructFiles.colors = {
          content = builtins.toJSON config.colors;
          output = lib.mkOverride 0 config.configDrvOutput;
          relPath = lib.mkOverride 0 "noctalia/palettes/custom.json";
        };
        package = lib.mkDefault pkgs.noctalia;
        settings = {
          bar = {
            default = {
              center = [
                "workspaces"
              ];
              end = [
                "tray"
                "spacer_0"
                "notifications"
                "keyboard_layout"
                "network"
                "bluetooth"
                "volume"
                "brightness"
                "battery"
                "spacer_1"
                "clock"
              ];
              margin_ends = 0;
              start = [
                "session"
                "launcher"
                "wallpaper"
                "media"
              ];
            };
          };
          idle = {
            behavior = {
              lock = {
                action = "lock";
                enabled = true;
                timeout = 600.0;
              };
              screen-off = {
                action = "screen_off";
                enabled = true;
                timeout = 660.0;
              };
            };
            behavior_order = [
              "lock"
              "screen-off"
            ];
          };
          keybinds = {
            validate = [
              "Return"
              "KP_Enter"
            ];
          };
          location.auto_locate = true;
          nightlight.enabled = true;
          shell = {
            clipboard_history_max_entries = 10;
            greeter_sync.auto_sync = true;
            launch_apps_as_systemd_services = true;
            panel = {
              open_near_click_clipboard = true;
              open_near_click_control_center = true;
            };
            polkit_agent = true;
            screen_corners.enabled = true;
          };
          theme = {
            source = "wallpaper";
            templates.builtin_ids = [
              "gtk3"
              "gtk4"
            ];
          };
          wallpaper.directory = "${homeDirectory.linux}/Imatges/Wallpapers";
          widget = {
            clock = {
              format = "{:%a %d %b %H:%M}";
            };
            media = {
              hide_when_no_media = true;
            };
            notifications = {
              hide_when_no_unread = true;
            };
            spacer_0 = {
              length = 15;
              type = "spacer";
            };
            spacer_1 = {
              length = 15;
              type = "spacer";
            };
          };
        };
      };
    };
}
