{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    homeManager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bleeding.url = "nixpkgs/nixos-unstable"; # another experimental commit of nixpkgs
    stable.url = "github:NixOS/nixpkgs/nixos-25.05";

    nur.url = "github:nix-community/NUR";
    personal = {
      url = "path:/Users/meain/dev/src/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    tree-grepper.url = "github:BrianHicks/tree-grepper/82d31cca29d9926ee4db91de88b84729901f93a7";

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      homeManager,
      bleeding,
      stable,
      nur,
      personal,
      emacsOverlay,
      tree-grepper,
      firefox-addons,
    }:
    {
      homeConfigurations =
        let
          # system = "x86_64-linux";
          system = "aarch64-darwin";
          homeConfig = import ./home.nix {
            inherit
              system
              bleeding
              stable
              personal
              tree-grepper
              firefox-addons
              ;
          };
        in
        {
          meain = homeManager.lib.homeManagerConfiguration {
            modules = [ homeConfig ];

            pkgs = import nixpkgs {
              # config.allowBroken = true;
              # config.allowUnfree = true; # resistance is futile
              inherit system;
              overlays = [
                emacsOverlay.overlay
                nur.overlays.default
              ];
            };
          };
        };
    };
}
