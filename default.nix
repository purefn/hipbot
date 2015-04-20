{ mkDerivation, aeson, base, blaze-builder, bytestring
, clientsession, either, exceptions, http-client, http-types, jwt
, lens, lucid, mtl, network-uri, stdenv, stm, text, transformers
, unordered-containers, utf8-string, wai, wai-extra, wai-lens, warp
, webcrank-wai, wreq
}:
mkDerivation {
  pname = "hipbot";
  version = "0.1";
  src = ./.;
  buildDepends = [
    aeson base blaze-builder bytestring clientsession either exceptions
    http-client http-types jwt lens lucid mtl network-uri stm text
    transformers unordered-containers utf8-string wai wai-extra
    wai-lens warp webcrank-wai wreq
  ];
  homepage = "https://bitbucket.org/rwallace/hipbot";
  description = "A library for building HipChat Bots";
  license = stdenv.lib.licenses.bsd3;
}
