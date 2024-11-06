{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.plasma.panels = [
      # Application menu
      {
        location = "top";
        alignment = "center";
        lengthMode = "fill";
        hiding = "none";
        floating = false;
        height = 32;
        screen = "all";
        widgets = [
          {
            kickoff = {
              applicationsDisplayMode = "list";
              compactDisplayStyle = false;
              favoritesDisplayMode = "grid";
              sortAlphabetically = true;
              showButtonsFor = {
                custom = [
                  "shutdown"
                  "reboot"
                  "logout"
                  "lock-screen"
                ];
              };
              showActionButtonCaptions = true;
            };
          }
          {
            appMenu = {
              compactView = false;
            };
          }
          {
            panelSpacer = {
              expanding = true;
            };
          }
          {
            pager = {
              general = {
                showWindowOutlines = true;
                showApplicationIconsOnWindowOutlines = true;
                displayedText = "none";
                selectingCurrentVirtualDesktop = "showDesktop";
              };
            };
          }
          {
            panelSpacer = {
              expanding = true;
            };
          }
          {
            systemTray = {
              icons = {
                scaleToFit = true;
                spacing = "small";
              };
              items = {
                hidden = [
                  "org.kde.plasma.brightness"
                ];
              };
              pin = false;
            };
          }
          {
            digitalClock = {
              date = {
                enable = true;
                format = "shortDate";
                position = "belowTime";
              };
              time = {
                format = "24h";
                showSeconds = "onlyInTooltip";
              };
            };
          }
        ];
      }
      # Dock
      {
        location = "bottom";
        alignment = "center";
        lengthMode = "fit";
        hiding = "dodgewindows";
        floating = true;
        height = 50;
        screen = "all";
        widgets = [
          {
            iconTasks = {
              appearance = {
                fill = false;
                highlightWindows = true;
                iconSpacing = "medium";
                indicateAudioStreams = true;
                rows = {
                  multirowView = "never";
                  maximum = null;
                };
                showTooltips = true;
              };
              behavior = {
                grouping = {
                  clickAction = "showPresentWindowsEffect";
                  method = "byProgramName";
                };
                middleClickAction = "newInstance";
                minimizeActiveTaskOnClick = true;
                newTasksAppearOn = "right";
                showTasks = {
                  onlyInCurrentActivity = true;
                  onlyInCurrentDesktop = true;
                  onlyMinimized = false;
                  onlyInCurrentScreen = false;
                };
                sortingMethod = "manually";
                unhideOnAttentionNeeded = true;
                wheel = {
                  ignoreMinimizedTasks = true;
                  switchBetweenTasks = true;
                };
              };
              launchers = [
                "applications:org.kde.dolphin.desktop"
                "preferred://browser"
                "applications:org.kde.konsole.desktop"
                "preferred://text-editor"
              ];
            };
          }
        ];
      }
    ];
  };
}
