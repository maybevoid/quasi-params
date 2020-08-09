let
  commit = "99ec1661420a1c2cba1e012380dd6344bb640799";
  sha256 = "0xvkfv35rmr4vc6gi06apzm0qp4ljbpm6h5pkmcqx0kyhmfpm3ni";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
  }
