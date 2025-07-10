{

  inputs = {
    miso.url = "github:dmjio/miso";
  };

  outputs = inputs: 
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.miso.inputs.nixpkgs.legacyPackages.${system};

      in rec {
        devShell = inputs.miso.outputs.devShells.${system}.wasm;

        packages = rec {
          default = miso-audio;

          miso-audio = pkgs.stdenv.mkDerivation {
            name = "miso-audio";
            src = ./.;
            buildInputs = [
              inputs.miso.inputs.ghc-wasm-meta.packages.${system}.all_9_12
              pkgs.cabal-install
              pkgs.git
              pkgs.gnumake
            ];
            preBuild = ''
              HOME=$PWD
            '';
            installPhase = ''
              mkdir $out
              cp -r public $out/
            '';
          };

        };
      }
    );

}

