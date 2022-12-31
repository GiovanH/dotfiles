#!/bin/python3
#
# Converts a json formatted argv list to a shell-ready command string.
# Argv lists are frequently reported by debuggers. Pass the json list
# to this script and get a reusable command for manual testing.

import shlex
import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument('list', help="List of command elements, in a json-formatted string.")

args = parser.parse_args()

mylist = json.loads(args.list)
print(' '.join([shlex.quote(s) for s in mylist]))
