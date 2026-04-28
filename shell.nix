{ pkgs ? import <nixpkgs> {} }:

let
  r = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      sna
      here
      rmarkdown
      knitr
      numDeriv
    ];
  };
in

pkgs.mkShell {
  packages = with pkgs; [
    r
    quarto
  ];
  shellHook = ''
    export QUARTO_R="${r}/bin/R"
  '';
}