let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  #haskell-language-server = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8102Binary" ]; };

  ghc8102Binary = pkgs.haskell.compiler.ghc8102Binary.overrideAttrs(old: {
    src = builtins.fetchurl {
      url = "https://downloads.haskell.org/ghc/${old.version}/ghc-${old.version}-x86_64-fedora27-linux.tar.xz";
      sha256 = "jGddqD6bPC9k67QHtfnrssHyGqXXAQIGFP3OZEpULjs=";
    };
  });

  tooling = [
    pkgs.cabal-install
    pkgs.cabal2nix
    ghc8102Binary
    #pkgs.haskell.compiler.ghc8102Binary
    #pkgs.haskell.packages.ghc8102Binary.haskell-language-server
    #haskell-language-server
    pkgs.hlint
    pkgs.stylish-haskell
    pkgs.ormolu
  ];

  consoleDeps =
    [ pkgs.nodejs-12_x
      pkgs.google-cloud-sdk
      pkgs.python
    ];

  serverDeps =
    [ ];

  testDeps = [ ];

  projectDeps = consoleDeps ++ serverDeps ++ testDeps;

in
pkgs.mkShell {
  buildInputs = projectDeps ++ tooling;
}
