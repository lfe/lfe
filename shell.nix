let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) stdenv;
in stdenv.mkDerivation {
  name = "lfe";
  buildInputs = with pkgs; [ pandoc ];
}
