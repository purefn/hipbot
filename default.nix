{ mkDerivation, aeson, base, bifunctors, blaze-builder, bytestring
, either, exceptions, http-client, http-client-tls, http-types, jwt
, lens, mtl, network-uri, postgresql-simple, resource-pool, safe
, stdenv, stm, text, time, transformers, unordered-containers
, utf8-string, wai, wai-lens, webcrank-wai, wreq
}:
mkDerivation {
  pname = "hipbot";
  version = "0.3.0.2";
  src = builtins.filterSource (path: type: baseNameOf path != ".git" && baseNameOf path != "dist") ./.;
  buildDepends = [
    aeson base bifunctors blaze-builder bytestring either exceptions
    http-client http-client-tls http-types jwt lens mtl network-uri
    postgresql-simple resource-pool safe stm text time transformers
    unordered-containers utf8-string wai wai-lens webcrank-wai wreq
  ];
  homepage = "https://github.com/purefn/hipbot";
  description = "A library for building HipChat Bots";
  license = stdenv.lib.licenses.bsd3;
}
