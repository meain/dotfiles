# Contains personal bookmarks
# Actual contents of the file are different, but here is an example
{ utils, ... }:
utils.gh-bookmarks {
  repo = "NixOS/nixpkgs";
  basename = "nixpkgs";
  basekeyword = "nip";
} ++
utils.gh-bookmarks {
  repo = "meain/dotfiles";
  basename = "dotfiles";
  basekeyword = "dot";
}