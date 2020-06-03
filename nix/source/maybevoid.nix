{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "13e37dbe0975b3b7b0f664b2e1e3e4bed8ee8c70";
  };
in
if useLocal then local-src else remote-src
