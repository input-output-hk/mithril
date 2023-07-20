{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
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
        craneLib = inputs.crane.lib.${system};

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
            in
              type == "directory" || matchesSuffix || isCargoFile || isCargoConfig || isOpenApiYaml;
          };

        buildInputs =
          [
            pkgs.gnum4
            pkgs.pkg-config
            pkgs.openssl
          ]
          ++ lib.optional (pkgs.stdenv.isDarwin) [
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
            pkgs.darwin.configdHeaders
            pkgs.darwin.libiconv
          ];

        commonsArgs = {
          pname = "mithril";
          version = "0.0.1";
          src = clean ./.;
          inherit buildInputs;
          CARGO_TERM_VERBOSE = "true";
        };

        buildDeps = cargoToml: cargoArtifacts:
          (craneLib.buildDepsOnly (commonsArgs // lib.optionalAttrs (cargoToml != null) rec {
            inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
            cargoExtraArgs = "-p ${pname}";
          })).overrideAttrs (_: {
            inherit cargoArtifacts;
            # Need to remove mithril-stm and mithril-common dummy builds to avoid later conflicts
            preInstall = ''
              rm ''${CARGO_TARGET_DIR:-target}/release/deps/*mithril*
              rm -r ''${CARGO_TARGET_DIR:-target}/release/*mithril*
              rm -r ''${CARGO_TARGET_DIR:-target}/release/build/*mithril*
              rm -r ''${CARGO_TARGET_DIR:-target}/release/.fingerprint/*mithril*
            '';
          });

        buildPackage = cargoToml: baseCargoArtifacts: args:
          craneLib.buildPackage (commonsArgs // lib.optionalAttrs (cargoToml != null) rec {
            inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
            cargoExtraArgs = "-p ${pname}";
          } // {
            cargoArtifacts = buildDeps cargoToml baseCargoArtifacts;
          } // args);

        mithril-stm-deps = buildDeps ./mithril-stm/Cargo.toml null;

        mithril-common-deps = buildDeps ./mithril-common/Cargo.toml mithril-stm-deps;

        mithril-deps = buildDeps null mithril-common-deps;

        mithril = buildPackage null null {
          cargoArtifacts = mithril-deps;
          doCheck = false;
        };

      in {
        packages = {
          default = mithril;
          inherit mithril;
          mithril-stm = buildPackage ./mithril-stm/Cargo.toml mithril-stm-deps {};
          mithril-common = buildPackage ./mithril-common/Cargo.toml mithril-common-deps {};
          mithril-client = buildPackage ./mithril-client/Cargo.toml mithril-deps {};
          mithril-aggregator = buildPackage ./mithril-aggregator/Cargo.toml mithril-deps {};
          mithril-signer = buildPackage ./mithril-signer/Cargo.toml mithril-deps {};
          mithrildemo = buildPackage ./demo/protocol-demo/Cargo.toml mithril-deps {};
          mithril-end-to-end = buildPackage ./mithril-test-lab/mithril-end-to-end/Cargo.toml mithril-deps {};
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [self'.packages.mithril-client];

          nativeBuildInputs = [
            pkgs.cargo
            pkgs.rustc
            pkgs.libiconv
            config.treefmt.package
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
