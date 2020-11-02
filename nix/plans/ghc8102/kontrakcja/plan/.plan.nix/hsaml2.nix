{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "hsaml2"; version = "0.1.1"; };
      license = "Apache-2.0";
      copyright = "2016";
      maintainer = "dylan@dylex.net";
      author = "Dylan Simon";
      homepage = "";
      url = "";
      synopsis = "OASIS Security Assertion Markup Language (SAML) V2.0";
      description = "Direct implementation of the SAML XML standard (https://www.oasis-open.org/standards#samlv2.0), along with some related dependencies.  This is currently partial, as the standard is quite extensive, but is sufficient to build a functioning SP and fully validate responses.  The module layout basically follows the standard definition documentation.  Its use still requires a fairly extensive understanding of SAML.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "test/Metadata/metadata-idp.xml"
        "test/Metadata/metadata-nyu.xml"
        "test/Metadata/metadata-osf.xml"
        "test/Metadata/metadata-sp.xml"
        "test/XML/encryption-example.xml"
        "test/XML/noncanonical1.xml"
        "test/XML/noncanonical2.xml"
        "test/XML/noncanonical3.xml"
        "test/XML/noncanonical4.xml"
        "test/XML/noncanonical5.xml"
        "test/XML/noncanonical6.xml"
        "test/XML/signature-example.xml"
        "test/XML/world.txt"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."hxt" or (errorHandler.buildDepError "hxt"))
          (hsPkgs."hxt-charproperties" or (errorHandler.buildDepError "hxt-charproperties"))
          (hsPkgs."hxt-unicode" or (errorHandler.buildDepError "hxt-unicode"))
          (hsPkgs."invertible" or (errorHandler.buildDepError "invertible"))
          (hsPkgs."invertible-hxt" or (errorHandler.buildDepError "invertible-hxt"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libxml-2.0" or (errorHandler.pkgConfDepError "libxml-2.0"))
          ];
        buildable = true;
        modules = [
          "SAML2/Lens"
          "SAML2/XML/LibXML2"
          "SAML2/Bindings/Internal"
          "SAML2"
          "SAML2/XML"
          "SAML2/XML/Schema"
          "SAML2/XML/Schema/Datatypes"
          "SAML2/XML/Canonical"
          "SAML2/XML/ASN1"
          "SAML2/XML/Signature"
          "SAML2/XML/Signature/Types"
          "SAML2/XML/Encryption"
          "SAML2/XML/Types"
          "SAML2/Core"
          "SAML2/Core/Namespaces"
          "SAML2/Core/Datatypes"
          "SAML2/Core/Assertions"
          "SAML2/Core/Protocols"
          "SAML2/Core/Versioning"
          "SAML2/Core/Signature"
          "SAML2/Core/Identifiers"
          "SAML2/Profiles"
          "SAML2/Profiles/ConfirmationMethod"
          "SAML2/Bindings"
          "SAML2/Bindings/General"
          "SAML2/Bindings/Identifiers"
          "SAML2/Bindings/HTTPRedirect"
          "SAML2/Bindings/HTTPPOST"
          "SAML2/Metadata"
          "SAML2/Metadata/Metadata"
          ];
        cSources = [ "SAML2/XML/libxml2_stub.c" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."hsaml2" or (errorHandler.buildDepError "hsaml2"))
            (hsPkgs."hxt" or (errorHandler.buildDepError "hxt"))
            (hsPkgs."hxt-http" or (errorHandler.buildDepError "hxt-http"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          modules = [
            "Bindings/HTTPRedirect"
            "Metadata/Metadata"
            "XML"
            "XML/Canonical"
            "XML/Encryption"
            "XML/Signature"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/0; }