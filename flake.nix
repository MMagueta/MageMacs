{
  description = "MageMacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ nixpkgs, utils, ... }:
    {
      overlay.default =
        final: prev: { magemacs = import ./default.nix {pkgs = final;};};
    } //
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in
      rec {

        apps.default = utils.lib.mkApp { drv = packages.default; exePath = "/bin/emacs"; };

        packages.default = import ./default.nix { inherit pkgs;};

      });
}
