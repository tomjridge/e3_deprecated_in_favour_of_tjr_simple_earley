{ }:
let 
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchgit = pkgs.fetchgit;
  ocaml=pkgs.ocaml; 
  findlib=pkgs.ocamlPackages.findlib;
in
stdenv.mkDerivation {
      name = "e3";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60e36482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib ];
    
      configurePhase = "true"; 	# Skip configure
  
      postInstall="cp -R _build/src/* $out";
           
      createFindlibDestdir = true;
}