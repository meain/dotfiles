{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    homeManager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stable.url = "nixpkgs/nixos-21.11";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    personal = {
      url = "path:/home/meain/dev/src/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, homeManager, stable, nur, personal, emacsOverlay }: {
    homeConfigurations =
      let
        homeConfig = import ./home.nix {
          personal = personal;
          stable = stable;
        };
      in
      {
        meain = homeManager.lib.homeManagerConfiguration {
          modules = [
            homeConfig
          ];

          pkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [ emacsOverlay.overlay nur.overlay ];
          };
        };
      };
  };
}
