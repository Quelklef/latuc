let
  rev = "babc25a577c3310cce57c72d5bed70f4c3c3843a";
  fetched = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
in import fetched {}
