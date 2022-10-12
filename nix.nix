{ pkgs }: let

hpkgs = pkgs.haskellPackages;

ghc = hpkgs.ghcWithPackages (p: with p; [ parsec extra ]);

shell = pkgs.mkShell {
  buildInputs = [ ghc hpkgs.Cabal pkgs.ghcid hpkgs.stylish-haskell ];
  shellHook = ''
    function devt {(
      shopt -s globstar
      ls ./**/*.hs | entr -s '
        clear;
        shopt -s globstar;
        stylish-haskell -i ./**/*.hs &&
        rm -rf ./dist-newstyle &&
        cabal run test
      '
    )}
  '';
};

deriv = hpkgs.callCabal2nix "latuc" ./. { };

in { inherit shell deriv; }
