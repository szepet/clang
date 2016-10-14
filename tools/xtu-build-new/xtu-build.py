#!/usr/bin/env python

import argparse
import multiprocessing

parser = argparse.ArgumentParser(description='Executes 1st pass of XTU analysis')
parser.add_argument('-b', required=True, metavar='build.json', help='Use a JSON Compilation Database')
parser.add_argument('-p', metavar='preanalyze-dir', help='Use directory for reading preanalyzation data (default=".xtu")', default='.xtu')
parser.add_argument('-j', metavar='threads', help='Number of threads used (default=' + str(multiprocessing.cpu_count()) + ')', default=multiprocessing.cpu_count())
parser.add_argument('-v', metavar='', help='Verbose output of every command executed')
args = parser.parse_args()


