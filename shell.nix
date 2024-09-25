{ pkgs ? import ./nixpkgs.nix }:
(import ./nix.nix { inherit pkgs; }).shell
