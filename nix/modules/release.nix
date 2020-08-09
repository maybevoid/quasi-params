{ nixpkgs ? import ./nixpkgs.nix
, haskell-nix ? import ./haskell-nix.nix
, checkMaterialization ? false
, ghc-version
}:
let
  index-state = "2020-08-07T00:00:00Z";

  project-src = haskell-nix.pkgs.haskell-nix.haskellLib.cleanGit
    { name = "quasi-params-project";
      src = ../..;
    };


  plan-dir = ../plans + "/${ghc-version}";
  materialized = plan-dir + "/plan";
  plan-hash = nixpkgs.lib.removeSuffix "\n"
    (builtins.readFile (plan-dir + "/plan-hash.txt"));

  project = nixpkgs.callPackage ./project.nix
    { inherit
        plan-hash
        haskell-nix
        project-src
        ghc-version
        index-state
        materialized
        checkMaterialization
      ;
    };

  plan = import ./plan.nix
    { inherit
        nixpkgs
        haskell-nix
        project-src
        ghc-version
        index-state
      ;
    };

  shell = project.shellFor
    { packages = hsPkgs:
        [ hsPkgs.quasi-params
          hsPkgs.quasi-params-multi
        ];
    };
in
{ inherit
    plan
    shell
    project
    ghc-version
  ;
}
