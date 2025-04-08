{
  description = "Development environment for Grapesy";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          # This is a minimal development shell configuration that provides the necessary
          # native dependencies for building Grapesy. I'm intentionally avoiding the use
          # of haskell.nix or other more sophisticated Haskell packaging approaches for now
          # because:
          #
          # 1. The snappy-c Haskell package is currently broken in nixpkgs master
          # 2. I want a simple environment that focuses on providing just the native
          #    dependencies needed for successful builds
          # 3. This approach is more accessible to developers who don't use Nix
          #
          # In an ideal world, I would use haskell.nix for a more robust build and
          # packaging solution, but this minimal configuration serves our immediate needs
          # for development and testing.

          buildInputs = with pkgs; [
            # Haskell Development Tools
            cabal-install
            ghc
            haskell-language-server
            (writeScriptBin "haskell-language-server-wrapper" ''
              #!${stdenv.shell}
              exec haskell-language-server "$@"
            '')

            # Native Dependencies
            pkg-config
            protobuf
            snappy
            zlib
          ];
        };
      });
}
