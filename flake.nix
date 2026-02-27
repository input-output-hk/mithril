{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      imports = [inputs.treefmt-nix.flakeModule];

      flake.hydraJobs = builtins.removeAttrs inputs.self.packages ["aarch64-linux"];

      perSystem = {
        pkgs,
        config,
        system,
        self',
        ...
      }: let
        inherit (inputs.nixpkgs) lib;
        
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ (import inputs.rust-overlay) ];
      };

      pkgsMusl =
        if system == "x86_64-linux"
        then pkgs.pkgsCross.musl64
        else null;

      craneLib = inputs.crane.mkLib pkgs;

      craneLibMusl =
        if pkgsMusl != null then
          (inputs.crane.mkLib pkgsMusl).overrideToolchain (p:
            p.rust-bin.stable.latest.default.override {
              targets = [ "x86_64-unknown-linux-musl" ];
            }
          )
        else
          null;

        sourceFilter = orig_path: type: let
          path = builtins.toString orig_path;
          base = builtins.baseNameOf path;
          parentDir = builtins.baseNameOf (builtins.dirOf path);
          matchesSuffix = lib.any (suffix: lib.hasSuffix suffix base) [".rs" ".toml" ".md"];
          isCargoFile = base == "Cargo.lock";
          isCargoConfig = parentDir == ".cargo" && base == "config";
          isOpenApiYaml = base == "openapi.yaml";
          isFakeAggregatorDefaultData = lib.hasInfix "/mithril-aggregator-fake/default_data/" path;
          isTestDataAsset = lib.hasInfix "/test_data/" path;
        in
          type == "directory" || matchesSuffix || isCargoFile || isCargoConfig || isOpenApiYaml
                || isFakeAggregatorDefaultData || isTestDataAsset;

        clean = root:
          lib.cleanSourceWith {
            src = lib.cleanSource (craneLib.path root);
            filter = sourceFilter;
          };

        cleanMusl = root:
          lib.cleanSourceWith {
            src = lib.cleanSource (craneLibMusl.path root);
            filter = sourceFilter;
          };

        buildInputs =
          [
            pkgs.pkg-config
            pkgs.gnum4
            pkgs.openssl
          ]
          ++ lib.optionals (pkgs.stdenv.isDarwin) [
            pkgs.libiconv
          ];

        opensslMusl =
          if pkgsMusl != null then
            pkgsMusl.openssl.override { static = true; }
          else null;

        nativeBuildInputsMusl =
          if pkgsMusl != null then [
            pkgsMusl.stdenv.cc
            pkgsMusl.pkg-config
            pkgsMusl.gnum4
          ]
          else [];

        buildInputsMusl =
          if pkgsMusl != null then [
            opensslMusl
          ]
          else [];

        commonsArgs = {
          pname = "mithril";
          version = "0.0.1";
          src = clean ./.;
          inherit buildInputs;
          CARGO_TERM_VERBOSE = "true";
        };

        commonsArgsMusl =
          if pkgsMusl != null then {
            pname = "mithril";
            version = "0.0.1";
            src = cleanMusl ./.;
            nativeBuildInputs = nativeBuildInputsMusl;
            buildInputs = buildInputsMusl;
            CARGO_TERM_VERBOSE = "true";
            OPENSSL_STATIC = "1";
            OPENSSL_LIB_DIR = "${opensslMusl.out}/lib";
            OPENSSL_INCLUDE_DIR = "${opensslMusl.dev}/include";
          }
          else {};

        preInstallScript = let
          localLibs = "dummy|mithril|stm|multi_sig|size_benches|digester";
        in ''
          shopt -s extglob
          TARGET_DIR="''${CARGO_TARGET_DIR:-target}"
          if [ -n "$CARGO_BUILD_TARGET" ]; then
            PROFILE_DIR="$TARGET_DIR/$CARGO_BUILD_TARGET/release"
          else
            PROFILE_DIR="$TARGET_DIR/release"
          fi

          rm "$PROFILE_DIR/deps/"*@(${localLibs})*
          rm -r "$PROFILE_DIR/"*@(${localLibs})*
          rm -r "$PROFILE_DIR/build/"*@(${localLibs})*
          rm -r "$PROFILE_DIR/.fingerprint/"*@(${localLibs})*
        '';

        buildDeps = craneLibFor: commonArgs: cargoToml: cargoArtifacts:
          (craneLibFor.buildDepsOnly (commonArgs
            // lib.optionalAttrs (cargoToml != null) rec {
              inherit (craneLibFor.crateNameFromCargoToml {inherit cargoToml;}) pname version;
              cargoExtraArgs = "-p ${pname}";
            }))
          .overrideAttrs (_: {
            inherit cargoArtifacts;
            preInstall = preInstallScript;
          });

        buildPackage = cargoToml: baseCargoArtifacts: args:
          craneLib.buildPackage (commonsArgs
            // lib.optionalAttrs (cargoToml != null) rec {
              inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
              cargoExtraArgs = "-p ${pname}";
            }
            // {
              cargoArtifacts = buildDeps craneLib commonsArgs cargoToml baseCargoArtifacts;
              doCheck = false;
            }
            // {
              cargoTestCommand = "RUST_BACKTRACE=1 cargo test --profile release";
            }
            // args);

        buildPackageMusl = cargoToml: baseCargoArtifacts: args:
          craneLibMusl.buildPackage (commonsArgsMusl
            // lib.optionalAttrs (cargoToml != null) rec {
              inherit (craneLibMusl.crateNameFromCargoToml {inherit cargoToml;}) pname version;
              cargoExtraArgs = "-p ${pname}";
            }
            // {
              cargoArtifacts = buildDeps craneLibMusl commonsArgsMusl cargoToml baseCargoArtifacts;
              RUSTFLAGS = "-C target-feature=+crt-static";
              doCheck = false;
            }
            // args);

        # Standard (glibc) builds
        mithril-stm = buildPackage ./mithril-stm/Cargo.toml null {};
        mithril-common = buildPackage ./mithril-common/Cargo.toml mithril-stm.cargoArtifacts { cargoExtraArgs = "-p mithril-common"; };
        mithril = buildPackage null mithril-common.cargoArtifacts {
          doCheck = false;
        };

        # Static (musl) base dependencies
        mithril-stm-musl = if pkgsMusl != null then buildPackageMusl ./mithril-stm/Cargo.toml null {} else null;
        mithril-common-musl = if pkgsMusl != null then buildPackageMusl ./mithril-common/Cargo.toml mithril-stm-musl.cargoArtifacts { cargoExtraArgs = "-p mithril-common"; } else null;
      in {
        packages = {
          default = mithril;
          inherit mithril mithril-stm mithril-common;
          mithril-client = buildPackage ./mithril-client/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-client --features full"; };
          mithril-client-cli = buildPackage ./mithril-client-cli/Cargo.toml mithril.cargoArtifacts {
            pname = "mithril-client";
          };
          mithril-aggregator = buildPackage ./mithril-aggregator/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-aggregator --features rustls-tls,jemallocator"; cargoTestExtraArgs = "--no-default-features"; };
          mithril-signer = buildPackage ./mithril-signer/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-signer --features rustls-tls,jemallocator"; cargoTestExtraArgs = "--no-default-features"; };
          mithril-end-to-end = buildPackage ./mithril-test-lab/mithril-end-to-end/Cargo.toml null {};

        } // lib.optionalAttrs (pkgsMusl != null) {

          mithril-aggregator-static =
            buildPackageMusl ./mithril-aggregator/Cargo.toml mithril-common-musl.cargoArtifacts {
              cargoExtraArgs = "-p mithril-aggregator --features rustls-tls,jemallocator";
            };
          mithril-signer-static =
            buildPackageMusl ./mithril-signer/Cargo.toml mithril-common-musl.cargoArtifacts {
              cargoExtraArgs = "-p mithril-signer --features rustls-tls,jemallocator";
            };
          mithril-client-cli-static =
            buildPackageMusl ./mithril-client-cli/Cargo.toml mithril-common-musl.cargoArtifacts {
              pname = "mithril-client";
              cargoExtraArgs = "-p mithril-client-cli --features rustls-tls";
            };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [self'.packages.mithril-client-cli];

          nativeBuildInputs = [
            pkgs.cargo
            pkgs.rustc
            pkgs.libiconv
            config.treefmt.package
            pkgs.gnumake
          ];

          shellHook = ''
            export RUST_BACKTRACE=1
            ln -sf ${config.treefmt.build.configFile} treefmt.toml
          '';
        };

        formatter = pkgs.writeShellApplication {
          name = "treefmt";
          text = "exec ${config.treefmt.package}/bin/treefmt";
        };

        treefmt = {
          programs.alejandra.enable = true;
          programs.rustfmt.enable = true;
          projectRootFile = "flake.nix";
        };
      };
    };
}
