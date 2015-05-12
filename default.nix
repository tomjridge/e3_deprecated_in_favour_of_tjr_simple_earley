{ }:
# nix-build default.nix -A e3.build
# nix-shell default.nix -A e3.build
# nix-shell default.nix -A e3.post_install
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
   mke3 = { ocaml, findlib}:
   rec {
    build= stdenv.mkDerivation {
      name = "e3";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60e36482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib ];
    
      configurePhase = "true"; 	# Skip configure
  
#      installPhase = "true";

       postInstall="cp -a build $out";
           
      createFindlibDestdir = true;
    };

    # get a nix-shell environment with p3 installed via ocamlfind
    post_install = stdenv.mkDerivation {
      name = "post_install";
    
      buildInputs = [ ocaml findlib build ];
  
    };


  };
    
in 
(mke3 { ocaml=pkgs.ocaml_4_02_1; findlib=pkgs.ocamlPackages_4_02_1.findlib; }).build
#  e3_4_01_0 = mke3 { ocaml=pkgs.ocaml_4_01_0; findlib=pkgs.ocamlPackages_4_01_0.findlib; };
