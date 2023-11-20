{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    homeManager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bleeding.url = "nixpkgs/nixos-unstable"; # another experimental commit of nixpkgs
    stable.url = "nixpkgs/nixos-23.05";

    nur.url = "github:nix-community/NUR";
    personal = {
      url = "path:/home/meain/dev/src/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    tree-grepper.url = "github:BrianHicks/tree-grepper";
  };

  outputs =
    { self
    , nixpkgs
    , homeManager
    , bleeding
    , stable
    , nur
    , personal
    , emacsOverlay
    , tree-grepper
    }: {
      homeConfigurations =
        let
          homeConfig = import ./home.nix {
            inherit bleeding stable personal tree-grepper;
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
