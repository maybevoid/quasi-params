let
  commit = "34f366a25d711bb2257065dff10e4cd935ac0c8c";
  sha256 = "1a6l7lgrc2zhm1qn1zh9v4n320mkin8z76fqgvlk2cz6ihsr3bf1";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
  }
