{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib; # bring contents pkgs.lib into scope

strings.toUpper "hello"

pkgs.writeText "hello.txt" "Hello dear reader!"
