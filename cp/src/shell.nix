with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "myenv";
  buildInputs = [ zlib gecode ];
}
