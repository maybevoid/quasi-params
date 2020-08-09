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
      specVersion = "3.0";
      identifier = { name = "quasi-params-multi"; version = "0.1.0.0"; };
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
          (hsPkgs."quasi-params" or (errorHandler.buildDepError "quasi-params"))
          ];
        buildable = true;
        modules = [
          "QuasiParam/MultiParam/Module"
          "QuasiParam/MultiParam/Sig"
          ];
        hsSourceDirs = [ "src/lib" ];
        };
      sublibs = {
        "quasi-params-multi-test-item" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."quasi-params" or (errorHandler.buildDepError "quasi-params"))
            ];
          buildable = true;
          modules = [ "Test/QuasiParam/Item/Sig" ];
          hsSourceDirs = [ "src/item" ];
          };
        };
      tests = {
        "quasi-params-multi-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."quasi-params" or (errorHandler.buildDepError "quasi-params"))
            (hsPkgs."quasi-params-multi" or (errorHandler.buildDepError "quasi-params-multi"))
            (hsPkgs."quasi-params-multi-test-item" or (errorHandler.buildDepError "quasi-params-multi-test-item"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          modules = [
            "Test/QuasiParam/Main"
            "Test/QuasiParam/Item"
            "Test/QuasiParam/Item/Internal"
            ];
          hsSourceDirs = [ "src/test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../quasi-params-multi; }