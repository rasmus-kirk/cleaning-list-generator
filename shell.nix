{ pkgs ? import <nixpkgs> {} }:
let
	ghc = pkgs.haskellPackages.ghcWithPackages (p: [p.curl p.megaparsec p.replace-megaparsec ]);
in pkgs.mkShell {
	nativeBuildInputs = with pkgs; [ pandoc random-shuffle shh shh-extras ]
 ;
}


