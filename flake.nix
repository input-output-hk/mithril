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
            src = lib.cleanSource root;
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

        buildPackage = name: cargoToml:
          craneLib.buildPackage {
            inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
            cargoExtraArgs = "-p ${name}";
            src = clean ./.;
            inherit buildInputs;
          };
      in {
        packages = {
          default = craneLib.buildPackage {
            pname = "mithril";
            version = "0.0.1";
            src = clean ./.;
            doCheck = false; # some tests require cardano-cli
            inherit buildInputs;
          };

          mithril-client = buildPackage "mithril-client" ./mithril-client/Cargo.toml;
          mithril-aggregator = buildPackage "mithril-aggregator" ./mithril-aggregator/Cargo.toml;
          mithril-signer = buildPackage "mithril-signer" ./mithril-signer/Cargo.toml;
          mithrildemo = buildPackage "mithrildemo" ./demo/protocol-demo/Cargo.toml;
          mithril-end-to-end = buildPackage "mithril-end-to-end" ./mithril-test-lab/mithril-end-to-end/Cargo.toml;
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
