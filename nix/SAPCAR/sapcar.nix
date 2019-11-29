with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "sapcar";

  nativeBuildInputs = [
    autoPatchelfHook # Automatically setup the loader, and do the magic
  ];

  buildInputs = [ glibc gcc-unwrapped ];
  
  src = ../SAPCAR;

  installPhase = ''
    install -m755 -D -t $out/bin SAPCAR
  '';
}
