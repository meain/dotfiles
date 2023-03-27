{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    homeManager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stable.url = "nixpkgs/nixos-21.11";

    pinned-firefox.url = "github:nixos/nixpkgs/c1329a147a5fc2bb49367f6c2cd84bdfeccade43";
    pinned-logseq.url = "github:nixos/nixpkgs/63f96b8e028fa93e63722a41e13d8d35b059a388";

    nur.url = "github:nix-community/NUR";
    personal = {
      url = "path:/home/meain/dev/src/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tree-grepper = {
      url = "github:BrianHicks/tree-grepper";
    };
  };

  outputs =
    { self
    , nixpkgs
    , homeManager
    , stable
    , pinned-logseq
    , pinned-firefox
    , nur
    , personal
    , emacsOverlay
    , tree-grepper
    }: {
      homeConfigurations =
        let
          homeConfig = import ./home.nix {
            inherit stable pinned-firefox pinned-logseq personal tree-grepper;
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
