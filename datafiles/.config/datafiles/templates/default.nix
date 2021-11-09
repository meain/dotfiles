{ nixpkgs ? import <nixpkgs> {  } }:
 
let
  pkgs = with nixpkgs; [ ];
 
in
  nixpkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = pkgs;
  }
