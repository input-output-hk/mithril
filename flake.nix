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

        buildPackage = name: cargoToml:
          craneLib.buildPackage {
            inherit (craneLib.crateNameFromCargoToml {inherit cargoToml;}) pname version;
            cargoExtraArgs = "-p ${name}";
            src = clean ./.;
            buildInputs = [
              pkgs.gnum4
              pkgs.pkg-config
              pkgs.openssl
            ];
          };
      in {
        packages = {
          mithril-client = buildPackage "mithril-client" ./mithril-client/Cargo.toml;
          mithril-aggregator = buildPackage "mithril-aggregator" ./mithril-aggregator/Cargo.toml;
          mithril-signer = buildPackage "mithril-signer" ./mithril-signer/Cargo.toml;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [self'.packages.mithril-client];

          nativeBuildInputs = [
            pkgs.cargo
            pkgs.rustc
            config.treefmt.package
          ];

          shellHook = ''
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
