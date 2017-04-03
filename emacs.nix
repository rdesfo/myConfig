{ pkgs ? import <nixpkgs> {} }:

let
    myEmacs = pkgs.emacs;
    emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages( epkgs: 
      (with epkgs.melpaPackages; [ evil org-caldav haskell-mode markdown-mode zerodark-theme ])
  )
