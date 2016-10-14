#!/usr/bin/env python

import argparse
import multiprocessing

parser = argparse.ArgumentParser(description='Executes 2nd round of XTU analysis')
parser.add_argument('-b', required=True, metavar='build.json', help='Use a JSON Compilation Database')
parser.add_argument('-p', metavar='preanalyze-dir', help='Use directory for reading preanalyzation data (default=".xtu")', default='.xtu')
parser.add_argument('-o', metavar='output-dir', help='Use directory for output analyzation results (default=".xtu-out")', default='.xtu-out')
parser.add_argument('-e', metavar='enabled-checker', nargs='+', help='List all enabled checkers')
parser.add_argument('-d', metavar='disabled-checker', nargs='+', help='List all disabled checkers')
parser.add_argument('-j', metavar='threads', help='Number of threads used (default=' + str(multiprocessing.cpu_count()) + ')', default=multiprocessing.cpu_count())
parser.add_argument('-v', metavar='', help='Verbose output of every command executed')
args = parser.parse_args()


