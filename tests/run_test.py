import argparse
import subprocess
import os
import re
import pathlib
import shutil

from pathlib import Path
from argparse import ArgumentParser
from subprocess import run
from concurrent.futures import ThreadPoolExecutor
from typing import Optional

path_java = "java-src"
expected_stdout = "expected-stdout.txt"
expected_stderr = "expected-stderr.txt"
javac_args = ["-g", "-source", "1.8", "-target", "1.8"]
info = "info.txt"
temp = "build-temp"
converted = "converted.ll"

class TestContext:
    def __init__(self, dx, javac, root, dexdump, path):
        self.root = root
        self.dexdump = dexdump.absolute()
        self.dx = dx.absolute()
        self.javac = javac.absolute()
        self.path = os.path.join(root, path)
        self.temp_path = os.path.join(self.path, temp)
        self.javasrc_path = os.path.join(self.path, path_java)
        self.test_no, self.test_name = path.split('_')


    def comparing(self, src, dst):
        return src in dst


    def compare_expected(self, out, err):
        expect_out = os.path.join(self.path, expected_stdout)
        expect_err = os.path.join(self.path, expected_stderr)
        expect_out = open(expect_out, 'r').read()
        expect_err = open(expect_err, 'r').read()
        o = self.comparing(out, expect_out)
        e = self.comparing(err, expect_err)
        return o and e


    def run(self):
        cmd = []
        cmd += [self.dexdump, "-m", self.temp_path, self.dex]
        cmd += ["-o", os.path.join(self.temp_path, converted)]
        proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out = proc.stdout.decode('ascii')
        err = proc.stderr.decode('ascii')
        res = self.compare_expected(out, err)
        return res, out


    def build_java(self) -> Optional[Path]:
        os.makedirs(self.temp_path, exist_ok=True)
        cmd = []
        cmd += [self.javac]
        cmd += javac_args
        cmd += sorted(Path(self.javasrc_path).glob("**/*.java"))
        cmd += ["-d", self.temp_path]
        subprocess.run(cmd)


    def build_dex(self) -> None:
        cmd = []
        cmd += [self.dx, "--dex"]
        #cmd += sorted(path.glob("**/*.class"))
        self.dex = os.path.join(self.temp_path, self.test_name) + ".dex"
        cmd += ["--output="+self.dex]
        cmd += [self.temp_path]
        subprocess.run(cmd)


    def build(self) -> None:
        self.build_java()
        self.build_dex()


    def clean(self) -> None:
        shutil.rmtree(self.temp_path)


def main() -> None:
    parser = ArgumentParser(description="Tests for dex2ir")
    parser.add_argument("--dx", type=Path, help="dx path")
    parser.add_argument("--javac", type=Path, help="javac path")
    parser.add_argument("--dexdump", type=Path, help="path of dexdump")
    parser.add_argument("--root", type=Path, help="root path")
    args = parser.parse_args()

    p = re.compile('\d*_\w*')
    test_dirs = [ s for s in os.listdir(args.root.absolute()) if p.match(s) ]

    tests: List[TestContext] = []
    for path in test_dirs:
        tests.append(TestContext(args.dx, args.javac, args.root, args.dexdump, path))

    # considering multithreading
    with ThreadPoolExecutor(os.cpu_count()) as pool:
        jobs = {}
        for test in tests:
            jobs[test.test_name] = pool.submit(test.build)
        for test_name, job in jobs.items():
            try:
                job.result()
            except Exception as e:
                raise Exception("Failed to build " + test_name) from e

        for test in tests:
            jobs[test.test_name] = pool.submit(test.run)
        for test_name, job in jobs.items():
            try:
                res, out = job.result()
                if res is not True:
                    print("{} {} Failed".format(test.test_no, test.test_name))
                    print("[stdout] : " + out)

            except Exception as e:
                raise Exception("Failed to run " + test_name) from e

        for test in tests:
            jobs[test.test_name] = pool.submit(test.clean)
        for test_name, job in jobs.items():
            try:
                job.result()
            except Exception as e:
                raise Exception("Failed to clean " + test_name) from e


if __name__ == "__main__":
    main()
