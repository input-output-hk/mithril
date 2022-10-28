import hashlib
import argparse
import os
import platform
import shutil


def sha256sum(path: str):
    h = hashlib.sha256()
    paths = []

    # Note: this won't works if the given path is a directory with a subdirectory
    if os.path.isdir(path):
        for file in os.listdir(path):
            paths.append(os.path.join(path, file))
    else:
        paths.append(path)

    for path in paths:
        with open(path, "rb") as f:
            data = f.read()
            h.update(data)
    return h.hexdigest()


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
                archive.write(os.path.join(input_dir, filename), arcname=filename)
    else:
        import tarfile

        archive_name = os.path.join(destination_dir, f"{archive_basename}.tar.gz")
        with tarfile.open(archive_name, "x:gz") as archive:
            for filename in input_files:
                archive.add(os.path.join(input_dir, filename), arcname=filename)

    print(f"successfully packed mithril distribution into: {archive_name}")
    return archive_name


def check_archive(archive_path, original_input_dir):
    print(f"checking archive ...")
    test_dir = "./unpack-test"
    shutil.unpack_archive(archive_path, test_dir)
    original_checksum = sha256sum(original_input_dir)
    archive_content_checksum = sha256sum(test_dir)
    if original_checksum != archive_content_checksum:
        print(
            f"mithril distribution checksum mismatch: before {original_checksum} != after {archive_content_checksum}"
        )
        exit(1)
    print("OK ! Checksum of the archive files matches original files")

    shutil.rmtree(test_dir)


def compute_sha256_checksum(archive_path):
    print(f"computing archive checksum...")
    archive_checksum = sha256sum(archive_path)
    checksum_filename = f"{archive_path}.sha256"
    with open(checksum_filename, "x") as f:
        f.write(archive_checksum)


def main(args):
    archive_path = build_archive(args.input, args.dest, archive_basename=f"mithril-{args.version}-{args.target}")
    check_archive(archive_path, args.input)
    compute_sha256_checksum(archive_path)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        prog="Mithril distribution packager",
        description="Package the files in the given '--input' dir in a .tar.gz (linux, macOs) or .zip (windows)"
                    " plus add a file with the value the sha256 of the generated package."
    )
    parser.add_argument("--input", type=dir_path, help="input folder which content will be archived", required=True)
    parser.add_argument("--dest", type=dir_path, help="destination folder for the archive, default to current folder",
                        default="./")
    parser.add_argument("--version", help="version of the distribution to package", required=True)
    parser.add_argument("--target", help="target os & architecture of the package", required=True)

    main(parser.parse_args())
