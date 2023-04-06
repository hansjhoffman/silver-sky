let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  basePackages = [
    pkgs.esbuild
    pkgs.spago
    pkgs.nodePackages.purs-tidy
    pkgs.nodePackages.purescript-language-server
    pkgs.nixfmt
    pkgs.nodejs-18_x
    pkgs.yarn
  ];

  inputs = basePackages;

  hooks = ''
  '';
in pkgs.mkShell {
  buildInputs = inputs;
  shellHook = hooks;
}
