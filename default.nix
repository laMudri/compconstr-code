let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) stdenv haskellPackages;

  hask-env = haskellPackages.ghcWithPackages (pkgs:
    with pkgs; [ array dlist pretty optparse-applicative alex happy ]);
in stdenv.mkDerivation {
  name = "stg";
  src = ./.;
  buildInputs = [ hask-env ];
  buildPhase = ''
    cabal build
  '';
  installPhase = "";
}
