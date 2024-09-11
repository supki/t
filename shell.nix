{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc965
}:
pkgs.mkShell rec {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

  buildInputs = with pkgs; [
    ghc
    stack
    pcre
  ];

  shellHook = ''
    export \
      LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}";
  '';
}
