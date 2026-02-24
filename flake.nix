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
        
      # pkgs =
      #   if system == "x86_64-linux" then
      #     import inputs.nixpkgs {
      #       inherit system;
      #       overlays = [ (import inputs.rust-overlay) ];
      #       crossSystem = {
      #         config = "x86_64-unknown-linux-musl";
      #       };
      #     }
      #   else
      #     import inputs.nixpkgs {
      #       inherit system;
      #       overlays = [ (import inputs.rust-overlay) ];
      #     };
        
      pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ (import inputs.rust-overlay) ];
        };

      pkgsMusl = pkgs.pkgsCross.musl64;

      craneLib =
        if pkgs.stdenv.isLinux then
          (inputs.crane.mkLib pkgs).overrideToolchain (p:
            p.rust-bin.stable.latest.default.override {
              targets = [ "x86_64-unknown-linux-musl" ];
            }
          )
        else
          inputs.crane.mkLib pkgs;

        clean = root:
          lib.cleanSourceWith {
            src = lib.cleanSource (craneLib.path root);
            filter = orig_path: type: let
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
          };

        buildInputs =
          [
            pkgs.gnum4
            pkgsMusl.openssl
          ]
          ++ lib.optionals (pkgs.stdenv.isDarwin) [
            pkgs.libiconv
          ];

        commonsArgs = {
          pname = "mithril";
          version = "0.0.1";
          src = clean ./.;
          inherit buildInputs;
          CARGO_TERM_VERBOSE = "true";
        };

        buildDeps = cargoToml: cargoArtifacts:
          (craneLib.buildDepsOnly (commonsArgs
            // lib.optionalAttrs (cargoToml != null) rec {
              inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
              cargoExtraArgs = "-p ${pname}";
            }))
          .overrideAttrs (_: {
            inherit cargoArtifacts;
            preInstall = let
              # Need to remove dummy builds of local builds so that they can be built again
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
          });

        buildPackage = cargoToml: baseCargoArtifacts: args:
          craneLib.buildPackage (commonsArgs
            // lib.optionalAttrs (cargoToml != null) rec {
              inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
              cargoExtraArgs = "-p ${pname}";
            }
            // {
              cargoArtifacts = buildDeps cargoToml baseCargoArtifacts;
            }
            // lib.optionalAttrs pkgs.stdenv.isLinux {
              CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
              RUSTFLAGS = "-C target-feature=+crt-static";
              
              CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER = 
                "${pkgsMusl.stdenv.cc}/bin/x86_64-unknown-linux-musl-gcc";
              
              PKG_CONFIG_ALLOW_CROSS = "1";
              PKG_CONFIG_ALL_STATIC = "1";
              OPENSSL_STATIC = "1";

              # OPENSSL_DIR = "${pkgs.openssl.dev}";
              # OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
              # OPENSSL_INCLUDE_DIR = "${pkgs.openssl.dev}/include";
            }
            // {
              cargoTestCommand = "RUST_BACKTRACE=1 cargo test --profile release";
            }
            // args);

        mithril-stm = buildPackage ./mithril-stm/Cargo.toml null {};
        mithril-common = buildPackage ./mithril-common/Cargo.toml mithril-stm.cargoArtifacts { cargoExtraArgs = "-p mithril-common"; };
        mithril = buildPackage null mithril-common.cargoArtifacts {
          doCheck = false;
        };
      in {
        packages = {
          default = mithril;
          inherit mithril mithril-stm mithril-common;
          mithril-client = buildPackage ./mithril-client/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-client --features full"; };
          mithril-client-cli = buildPackage ./mithril-client-cli/Cargo.toml mithril.cargoArtifacts {
            pname = "mithril-client";
          };
          mithril-aggregator = buildPackage ./mithril-aggregator/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-aggregator --features bundle_tls"; cargoTestExtraArgs = "--no-default-features"; };
          mithril-signer = buildPackage ./mithril-signer/Cargo.toml mithril.cargoArtifacts { cargoExtraArgs = "-p mithril-signer --features bundle_tls"; cargoTestExtraArgs = "--no-default-features"; };
          mithril-end-to-end = buildPackage ./mithril-test-lab/mithril-end-to-end/Cargo.toml null {};
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
