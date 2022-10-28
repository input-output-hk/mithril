import argparse
import glob
import os

import toml


def dir_path(path):
    if os.path.isdir(path):
        return path
    else:
        raise NotADirectoryError(path)


def append_label_to_cargo_toml_version(cargo_toml_path, label: str, dry_run: bool):
    cargo_toml = toml.load(cargo_toml_path)
    print(f"Editing {cargo_toml_path} ...")

    if 'package' not in cargo_toml:
        print("No package section (probably a workspace file), skipping this Cargo.toml")
        return

    new_version = f"{cargo_toml['package']['version'].split('-', 1)[0]}-{label}"
    print(f"{cargo_toml_path} new version: {new_version}")

    if not dry_run:
        cargo_toml['package']['version'] = new_version

        with open(cargo_toml_path, "w") as f:
            toml.dump(cargo_toml, f)


def main(args):
    workdir = os.path.relpath(args.working_dir)
    print(f"Searching for Cargo.toml(s) in '{workdir}' to edit their version label ...")

    cargo_tomls = glob.glob(f"{workdir}/**/Cargo.toml", recursive=True)
    print(f"Cargo.toml(s) found: {cargo_tomls}")

    for cargo_toml in cargo_tomls:
        append_label_to_cargo_toml_version(cargo_toml, args.semver_label, args.dry_run)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        prog="Cargo.toml semver label editor",
        description="Edit all Cargo.toml in the given path and subdirectories by replacing their label with the given"
                    " '--semver-label'.")
    parser.add_argument("-l", "--semver-label", required=True,
                        help="Label to suffix to the Cargo.toml(s) semver version")
    parser.add_argument("-d", "--working-dir", type=dir_path, default="./",
                        help="Directory in which Cargo.toml will be searched")
    parser.add_argument("--dry-run", action="store_true")

    main(parser.parse_args())
