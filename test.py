#!/usr/bin/env python3

# TODO: implementent something like '--clean' to remove all .asm and .txt files from the directory.

from sys import argv, stderr
import os
import subprocess

# We are only handling files in the folder not other folders
def run_folder(folder: str, init: bool):
    passed = 0
    failed = 0

    try:
        files = os.listdir(folder)

    except Exception:
        print(f"{argv[0]}: error: could not open folder '{folder}'", file=stderr)
        exit(1)

    for file in files:
        file = folder + "/" + file
        txt_file = "".join(file.split(".")[:-1]) + ".txt"

        if file.split(".")[-1] != "sbpl": 
            continue

        output = subprocess.check_output(["./hssbpl", "-r", "--silent", "--no-asm", file])
        
        if init: 
            with open(txt_file, "wb") as f:
                f.write(output)
            print(f"successfully initialised: {txt_file}")
            continue

        try:
            with open(txt_file, "rb") as f:
                if output == f.read():
                    print(f"test passed: {file}")
                    passed += 1

                else:
                    print(f"test failed: {file}")
                    failed += 1

        except FileNotFoundError:
            print(f"{argv[0]}: error: could not open file '{txt_file}', this test is likely uninitailised, run with 'init'", file=stderr)
            exit(1)
    
    if not init:
        print()
        print(f"tests passed: {passed}")
        print(f"tests failed: {failed}")

def main():
    INIT = False
    FOLDER = ""

    if len(argv) < 3 or "-f" not in argv:
        print(f"{argv[0]}: error: no folder specified; use -f [folder name]", file=stderr)
        exit(1)

    i = 0

    while i < len(argv):
        arg = argv[i]

        if arg == "init":
            INIT = True

        elif arg == "-f":
            FOLDER = argv[i+1]
            i += 2
            continue

        i += 1

    run_folder(FOLDER, INIT)

if __name__ == "__main__":
    main()
