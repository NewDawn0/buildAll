{
  description = "Your awesome flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    utils = {
      url = "github:NewDawn0/nixUtils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, utils, ... }: {
    overlays.default = final: prev: {
      build-all = self.packages.${prev.system}.default;
    };
    packages = utils.lib.eachSystem { } (pkgs:
      let
        build-all = pkgs.haskellPackages.mkDerivation {
          pname = "build-all";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = with pkgs.haskellPackages; [
            aeson
            base
            bytestring
            directory
            filepath
            optparse-applicative
            process
          ];
          homepage = "https://github.com/NewDawn0/buildAll";
          description = "A tool to build all outputs of a Nix flake";
          license = pkgs.lib.licenses.mit;
          mainProgram = "build-all";
        };
      in { default = build-all; });
  };
}
