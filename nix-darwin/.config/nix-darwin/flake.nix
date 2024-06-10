# Switch with: darwin-rebuild switch --flake ~/.dotfiles/nix-darwin/.config/nix-darwin
{
  description = "Darwin system flake for meain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }:
    let
      configuration = { pkgs, ... }: {
        system.defaults = {
          NSGlobalDomain = {
            "com.apple.mouse.tapBehavior" = 1;
            KeyRepeat = 1;
            "com.apple.swipescrolldirection" = false;
          };

          dock = {
            autohide = false; # autohide dock
            mru-spaces = false; # do not rearrange spaces based on most recent use
            orientation = "right"; # dock on right side
            show-recents = false; # do not show recently closed apps
            tilesize = 10; # smallest icon size
            magnification = true; # magnify icons on hover

            # set hot corners
            wvous-tl-corner = 2;
            wvous-tr-corner = 2;
            wvous-bl-corner = 1;
            wvous-br-corner = 12;

            persistent-apps = [ ];
            # persistent-others = ["~/Desktop" "~/Downloads"];
          };

          finder = {
            # AppleShowAllFiles = true; # show hidden files
            CreateDesktop = false; # do not show icons on desktop
            FXDefaultSearchScope = "SCcf"; # search current folder by default
            ShowPathbar = true; # show path bar
            ShowStatusBar = true; # show status bar
            FXEnableExtensionChangeWarning = false; # do not warn when changing file extensions
            FXPreferredViewStyle = "clmv"; # column view
          };

          menuExtraClock = {
            ShowDate = 1; # show date in menu bar
            ShowSeconds = true; # show seconds in menu bar
          };

          screencapture = {
            location = "~/Documents/Screenshots"; # will have to create it manually
          };

          universalaccess = {
            closeViewScrollWheelToggle = true; # enable zoom with scroll wheel
            reduceMotion = true; # disable animations
            reduceTransparency = true; # disable transparency
          };
        };

        services.nix-daemon.enable = true;
        nix.settings.experimental-features = "nix-command flakes";

        nixpkgs.hostPlatform = "aarch64-darwin";
        system.configurationRevision = self.rev or self.dirtyRev or null;
        system.stateVersion = 4; # for backwards compat

        programs.zsh.enable = true; # default shell on catalina
        environment.systemPackages = [ pkgs.vim ];
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Abins-MacBook-Air
      darwinConfigurations."Machine" = nix-darwin.lib.darwinSystem {
        modules = [
          configuration
          ./modules/homebrew.nix
        ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Machine".pkgs;
    };
}
