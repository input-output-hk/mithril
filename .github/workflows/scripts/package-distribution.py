import argparse
import os
import platform
import shutil


def dir_path(path):
    if os.path.isdir(path):
        return path
    else:
        raise NotADirectoryError(path)


def build_archive(input_dir, destination_dir, archive_basename):
    input_files = os.listdir(input_dir)
    print(f"packing {archive_basename} with files: {input_files}")

    if platform.system() == "Windows":
        import zipfile

        archive_name = os.path.join(destination_dir, f"{archive_basename}.zip")
        with zipfile.ZipFile(archive_name, mode="x") as archive:
            for filename in input_files:
                archive.write(os.path.join(
                    input_dir, filename), arcname=filename)
    else:
        import tarfile

        archive_name = os.path.join(
            destination_dir, f"{archive_basename}.tar.gz")
        with tarfile.open(archive_name, "x:gz") as archive:
            for filename in input_files:
                archive.add(os.path.join(input_dir, filename),
                            arcname=filename)

    print(f"successfully packed mithril distribution into: {archive_name}")
    return archive_name


def check_archive(archive_path, original_input_dir):
    print(f"checking archive ...")
    test_dir = "./unpack-test"
    shutil.unpack_archive(archive_path, test_dir)
    print("OK ! Check of the archive done")

    shutil.rmtree(test_dir)


def main(args):
    archive_path = build_archive(
        args.input, args.dest, archive_basename=f"mithril-{args.version}-{args.target}")
    check_archive(archive_path, args.input)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        prog="Mithril distribution packager",
        description="Package the files in the given '--input' dir in a .tar.gz (linux, macOs) or .zip (windows)."
    )
    parser.add_argument("--input", type=dir_path,
                        help="input folder which content will be archived", required=True)
    parser.add_argument("--dest", type=dir_path, help="destination folder for the archive, default to current folder",
                        default="./")
    parser.add_argument(
        "--version", help="version of the distribution to package", required=True)
    parser.add_argument(
        "--target", help="target os & architecture of the package", required=True)

    main(parser.parse_args())
