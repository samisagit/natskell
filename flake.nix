{
  description = "A flake to develop Natskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };


  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        hls = pkgs.haskell-language-server.override{supportedGhcVersions = [ "925" ];};
      in {
	defaultPackage = pkgs.hello;

        devShell = pkgs.mkShell{ 
          buildInputs = [
            hls
            pkgs.stack
            pkgs.hlint
            pkgs.stylish-haskell
            pkgs.docker-compose
	    pkgs.zlib
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };
      });
}
