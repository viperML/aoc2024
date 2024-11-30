with import <nixpkgs> { };
mkShell {
  packages = [
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.fast-tags
    yaml-language-server
    aoc-cli
  ];
}
