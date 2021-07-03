let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  tooling = [
    pkgs.stylish-haskell
    pkgs.haskell.compiler.ghc8104
    pkgs.haskell-language-server
    pkgs.hlint
    pkgs.cabal-install
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
