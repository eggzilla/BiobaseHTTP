{ mkDerivation, aeson, base, BiobaseEnsembl, bytestring, conduit
, either-unwrap, HTTP, http-conduit, hxt, mtl, network, stdenv
, Taxonomy, text, transformers
}:
mkDerivation {
  pname = "BiobaseHTTP";
  version = "1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base BiobaseEnsembl bytestring conduit either-unwrap HTTP
    http-conduit hxt mtl network Taxonomy text transformers
  ];
  homepage = "https://github.com/eggzilla/BiobaseHTTP";
  description = "Libary to interface with the Bioinformatics HTTP services - Entrez Ensembl";
  license = stdenv.lib.licenses.gpl3;
}
