{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc984
}:
pkgs.mkShell rec {
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
