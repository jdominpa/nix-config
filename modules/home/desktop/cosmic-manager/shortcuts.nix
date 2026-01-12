{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.cosmic-manager;
  inherit (config.jdp.base) user;
  inherit (config.home-manager.users.${user.name}.lib.cosmic) mkRON;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      wayland.desktopManager.cosmic.shortcuts = [
        # Move window
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Left") ];
            variant = "Move";
          };
          key = "Super+Shift+j";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Down") ];
            variant = "Move";
          };
          key = "Super+Shift+k";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Up") ];
            variant = "Move";
          };
          key = "Super+Shift+l";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Right") ];
            variant = "Move";
          };
          key = "Super+Shift+;";
        }
        # Move window to prev/next workspace
        {
          action = mkRON "enum" "SendToPreviousWorkspace";
          key = "Super+Ctrl+Shift+j";
        }
        {
          action = mkRON "enum" "SendToPreviousWorkspace";
          key = "Super+Ctrl+Shift+l";
        }
        {
          action = mkRON "enum" "SendToNextWorkspace";
          key = "Super+Ctrl+Shift+k";
        }
        {
          action = mkRON "enum" "SendToNextWorkspace";
          key = "Super+Ctrl+Shift+semicolon";
        }
        # Move window to output
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Left") ];
            variant = "SendToOutput";
          };
          key = "Super+Alt+Shift+j";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Down") ];
            variant = "SendToOutput";
          };
          key = "Super+Alt+Shift+k";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Up") ];
            variant = "SendToOutput";
          };
          key = "Super+Alt+Shift+l";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Right") ];
            variant = "SendToOutput";
          };
          key = "Super+Alt+Shift+semicolon";
        }
        # Focus window
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Left") ];
            variant = "Focus";
          };
          key = "Super+j";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Down") ];
            variant = "Focus";
          };
          key = "Super+k";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Up") ];
            variant = "Focus";
          };
          key = "Super+l";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Right") ];
            variant = "Focus";
          };
          key = "Super+;";
        }
        # Focus prev/next workspace
        {
          action = mkRON "enum" "MoveToPreviousWorkspace";
          key = "Super+Ctrl+j";
        }
        {
          action = mkRON "enum" "MoveToPreviousWorkspace";
          key = "Super+Ctrl+l";
        }
        {
          action = mkRON "enum" "MoveToNextWorkspace";
          key = "Super+Ctrl+k";
        }
        {
          action = mkRON "enum" "MoveToNextWorkspace";
          key = "Super+Ctrl+semicolon";
        }
        # Focus output
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Left") ];
            variant = "MoveToOutput";
          };
          key = "Super+Alt+j";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Down") ];
            variant = "MoveToOutput";
          };
          key = "Super+Alt+k";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Up") ];
            variant = "MoveToOutput";
          };
          key = "Super+Alt+l";
        }
        {
          action = mkRON "enum" {
            value = [ (mkRON "enum" "Right") ];
            variant = "MoveToOutput";
          };
          key = "Super+Alt+semicolon";
        }
        # Disabled shortcuts
        {
          action = mkRON "enum" "Disable";
          key = "Super+h";
        }
        {
          action = mkRON "enum" "Disable";
          key = "Super+Shift+h";
        }
        {
          action = mkRON "enum" "Disable";
          key = "Super+Ctrl+h";
        }
        {
          action = mkRON "enum" "Disable";
          key = "Super+Alt+h";
        }
        {
          action = mkRON "enum" "Disable";
          key = "Super+Ctrl+Shift+h";
        }
        {
          action = mkRON "enum" "Disable";
          key = "Super+Alt+Shift+h";
        }
      ];
    };
  };
}
