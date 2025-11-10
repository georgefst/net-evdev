{ pkgs, ... }: {
  name = "net-evdev";
  compiler-nix-name = "ghc912";
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
}
