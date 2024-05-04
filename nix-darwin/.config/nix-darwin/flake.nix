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
            autohide = true;
            mru-spaces = false;
            orientation = "right";
            show-recents = false;
            tilesize = 10;
            magnification = true;
            wvous-tl-corner = 2;
            wvous-bl-corner = 14;

            persistent-apps = [ "/System/Applications/Utilities/Terminal.app" ];
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
