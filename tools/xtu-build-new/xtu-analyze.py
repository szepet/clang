#!/usr/bin/env python

import argparse
import multiprocessing

threading_factor = int(multiprocessing.cpu_count() * 1.5)

parser = argparse.ArgumentParser(description='Executes 2nd pass of XTU analysis')
parser.add_argument('-b', required=True, dest='buildlog', metavar='build.json', help='Use a JSON Compilation Database')
parser.add_argument('-g', required=True, dest='buildgraph', metavar='build-graph.json', help='Use a JSON Build Dependency Graph')
parser.add_argument('-p', metavar='preanalyze-dir', dest='xtuindir', help='Use directory for reading preanalyzation data (default=".xtu")', default='.xtu')
parser.add_argument('-o', metavar='output-dir', dest='xtuoutdir' help='Use directory for output analyzation results (default=".xtu-out")', default='.xtu-out')
parser.add_argument('-e', metavar='enabled-checker', nargs='+', dest='enabled_checkers', help='List all enabled checkers')
parser.add_argument('-d', metavar='disabled-checker', nargs='+', dest='disabled_checkers', help='List all disabled checkers')
parser.add_argument('-j', metavar='threads', dest='threads', help='Number of threads used (default=' + str(threading_factor) + ')', default=threading_factor)
parser.add_argument('-v', dest='verbose', action='store_true', help='Verbose output of every command executed')
args = parser.parse_args()


