{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, adjunctions, aeson, async, base, bytestring
      , comonad, containers, cryptohash, distributive, ekg, free
      , hashable, hedgehog, hedis, hpack, hspec, hspec-hedgehog, jose
      , jwt, lens, lib, listsafe, monad-logger, MonadRandom, mtl
      , persistent, persistent-postgresql, persistent-template, pipes
      , pipes-aeson, pipes-concurrency, pipes-parse, pretty-simple
      , random, servant, servant-auth, servant-auth-client
      , servant-auth-server, servant-foreign, servant-options
      , servant-server, servant-websockets, split, stm, text, time
      , transformers, utf8-string, vector, wai, wai-cors, wai-extra
      , wai-logger, warp, websockets
      }:
      mkDerivation {
        pname = "poker-server";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          adjunctions aeson async base bytestring comonad containers
          cryptohash distributive ekg free hashable hedis jose jwt lens
          listsafe monad-logger MonadRandom mtl persistent
          persistent-postgresql persistent-template pipes pipes-aeson
          pipes-concurrency pipes-parse pretty-simple random servant
          servant-auth servant-auth-client servant-auth-server
          servant-foreign servant-options servant-server servant-websockets
          split stm text time transformers utf8-string vector wai wai-cors
          wai-extra wai-logger warp websockets
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          adjunctions aeson async base bytestring comonad containers
          cryptohash distributive ekg free hashable hedis jose jwt lens
          listsafe monad-logger MonadRandom mtl persistent
          persistent-postgresql persistent-template pipes pipes-aeson
          pipes-concurrency pipes-parse pretty-simple random servant
          servant-auth servant-auth-client servant-auth-server
          servant-foreign servant-options servant-server servant-websockets
          split stm text time transformers utf8-string vector wai wai-cors
          wai-extra wai-logger warp websockets
        ];
        testHaskellDepends = [
          adjunctions aeson async base bytestring comonad containers
          cryptohash distributive ekg free hashable hedgehog hedis hspec
          hspec-hedgehog jose jwt lens listsafe monad-logger MonadRandom mtl
          persistent persistent-postgresql persistent-template pipes
          pipes-aeson pipes-concurrency pipes-parse pretty-simple random
          servant servant-auth servant-auth-client servant-auth-server
          servant-foreign servant-options servant-server servant-websockets
          split stm text time transformers utf8-string vector wai wai-cors
          wai-extra wai-logger warp websockets
        ];
        prePatch = "hpack";
        homepage = "https://github.com/githubuser/poker-server#readme";
        license = lib.licenses.unlicense;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
