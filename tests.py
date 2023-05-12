#!/usr/bin/env python3

import sys
import argparse
import hashlife
import lifeparsers

def successor_test(args):
    glider = lifeparsers.autoguess_life_file(args.filename[0])
    successor = hashlife.successor(hashlife.construct(glider[0]))
    import pdb
    pdb.set_trace()
    print(glider)

def main(args):
    parser = argparse.ArgumentParser()
    parser.add_argument('test', default="successor")
    parser.add_argument('filename', nargs=1)
    args = parser.parse_args(args)

    if (args.test == "successor"):
        successor_test(args)

    return 0

if __name__=="__main__":
    sys.exit(main(sys.argv[1:]))
