{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "quasi-params"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) Soares Ruofei Chen";
      maintainer = "soares.chen@maybevoid.com";
      author = "Soares Chen";
      homepage = "https://github.com/maybevoid/quasi-params";
      url = "";
      synopsis = "Labelled Parameters as Constraints";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        modules = [
          "QuasiParam/Internal"
          "QuasiParam/Injective/Internal"
          "QuasiParam/Multi/Param"
          "QuasiParam/Multi/Cast"
          "QuasiParam/Multi/Entail"
          "QuasiParam"
          "QuasiParam/Dict"
          "QuasiParam/Label"
          "QuasiParam/Tag"
          "QuasiParam/Name"
          "QuasiParam/Injective/Label"
          "QuasiParam/Injective/Tag"
          "QuasiParam/Injective/Name"
          "QuasiParam/Multi"
          ];
        hsSourceDirs = [ "src/lib" ];
        };
      tests = {
        "quasi-params-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."quasi-params" or (errorHandler.buildDepError "quasi-params"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          modules = [
            "Test/QuasiParam/Main"
            "Test/QuasiParam/Basic"
            "Test/QuasiParam/Basic/Name"
            "Test/QuasiParam/Overlap"
            "Test/QuasiParam/Stress"
            "Test/QuasiParam/Multi"
            "Test/QuasiParam/Multi/Item"
            "Test/QuasiParam/Multi/Bifunctor"
            "Test/QuasiParam/Multi/Internal/Item"
            "Test/QuasiParam/Multi/Internal/Entry"
            "Test/QuasiParam/Multi/Internal/Pair"
            "Test/QuasiParam/Multi/Internal/Bifunctor"
            ];
          hsSourceDirs = [ "src/test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../quasi-params; }