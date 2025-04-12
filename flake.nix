{
  description = "psql-migrate";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-devtools?ref=lts/ghc-9.8.x";
    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform?ref=lts/ghc-9.8.x";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    inputs@
    { self
    , flake-utils
    , horizon-platform
    , horizon-devtools
    , nixpkgs
    , ...
    }: with builtins; let
      inherit (inputs.nixpkgs) lib;
      onlyHaskell = fs2source
        (fs.union
          (onlyExts-fs [ "cabal" "hs" "project" ] ./.)
          ./LICENSE # horizon requires this file to build
        );
      fs = lib.fileset;
      fs2source = fs': path: fs.toSource { root = path; fileset = fs'; };
      onlyExts-fs = exts: fs.fileFilter (f: foldl' lib.or false (map f.hasExt exts));
      onlyExts = exts: path: fs2source (onlyExts-fs exts path) path;

    in
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
      (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;
        devtools = inputs.horizon-devtools.packages.${system};

        legacyPackages =
          horizon-platform.legacyPackages.${system}.extend
            self.haskell-overlay.${system}.default;
      in
      {
        haskell-overlay.default = final: prev: {
          psql-migrate = final.callCabal2nix "psql-migrate" (onlyHaskell ./.) { };
        };

        packages.default = legacyPackages.psql-migrate;

        devShells.default = legacyPackages.psql-migrate.env.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ (with pkgs; [
            cabal-install
            haskellPackages.cabal-fmt
          ] ++ lib.optionals (system == "x86_64-linux" || system == "aarch64-linux") [
            devtools.ghcid
            devtools.hlint
            devtools.haskell-language-server
          ] ++ [
            nixpkgs-fmt
            stylish-haskell
            devtools.fourmolu
          ]);

          shellHook =
            let
              instructions = ''
                ${write-descriptions general-functions}
              '';

              general-functions = {
                format = {
                  description = "format code";
                  body = ''
                    nix fmt
                    stylish-haskell -ir .
                    cabal-fmt -i *.cabal
                  '';
                };

                shelp = {
                  description = "show this message";
                  body = "echo ${lib.escapeShellArg instructions}";
                };
              };


              write-set = set: f: concatStringsSep "\n" (lib.mapAttrsToList f set);

              write-descriptions = set: ''
                ${write-set set (n: v: "  ${n}\t${v.description}")}'';
            in
            ''
              ${write-set (general-functions)
                  (n: v: ''
                     ${n}() {
                       ${v.body}
                     }
                   '')
              }

              shelp
            '';
        });

        formatter = pkgs.nixpkgs-fmt;
      });
}
