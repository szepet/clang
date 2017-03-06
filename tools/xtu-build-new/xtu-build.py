#!/usr/bin/env python

import argparse
import io
import json
import multiprocessing
import os
import re
import signal
import subprocess
import string

threading_factor = int(multiprocessing.cpu_count() * 1.5)
timeout = 86400

parser = argparse.ArgumentParser(
    description='Executes 1st pass of XTU analysis')
parser.add_argument('-b', required=True, dest='buildlog',
                    metavar='build.json',
                    help='Use a JSON Compilation Database')
parser.add_argument('-p', metavar='preanalyze-dir', dest='xtuindir',
                    help='Use directory for generating preanalyzation data '
                         '(default=".xtu")',
                    default='.xtu')
parser.add_argument('-j', metavar='threads', dest='threads',
                    help='Number of threads used (default=' +
                    str(threading_factor) + ')',
                    default=threading_factor)
parser.add_argument('-v', dest='verbose', action='store_true',
                    help='Verbose output of every command executed')
parser.add_argument('--clang-path', metavar='clang-path', dest='clang_path',
                    help='Set path of clang binaries to be used '
                         '(default taken from CLANG_PATH envvar)',
                    default=os.environ.get('CLANG_PATH'))
parser.add_argument('--timeout', metavar='N',
                    help='Timeout for build in seconds (default: %d)' %
                    timeout,
                    default=timeout)
mainargs = parser.parse_args()


def executable_exists(path, exe_name):
    abs_exe_path = os.path.abspath(os.path.join(path, exe_name))
    return os.path.isfile(abs_exe_path) and os.access(abs_exe_path, os.X_OK)


def find_executable_on_arg_path(path, exe_name):
    exists = executable_exists(path, exe_name)
    return path if exists else None


def find_executable_on_env_path(exe_name):
    paths = os.environ['PATH'].split(os.pathsep)
    return next(
        (path for path in paths if executable_exists(path, exe_name)), None)


def check_executable_available(exe_name, arg_path):
    found_path = None
    if arg_path is not None:
        found_path = find_executable_on_arg_path(arg_path, exe_name)
    else:
        found_path = find_executable_on_env_path(exe_name)

    if found_path is None:
        print 'Executable "{}" not found on PATH provided via {}!'.format(
            exe_name,
            'argument' if arg_path is not None else 'environment')
        sys.exit(1)
    elif mainargs.verbose:
        print 'XTU uses {} dir: {}, taken from {}.'.format(
            exe_name,
            found_path,
            'argument' if arg_path is not None else 'environment')
    return found_path


clang_path = check_executable_available('clang', mainargs.clang_path)

buildlog_file = open(mainargs.buildlog, 'r')
buildlog = json.load(buildlog_file)
buildlog_file.close()

src_pattern = re.compile('.*\.(C|c|cc|cpp|cxx|ii|m|mm)$', re.IGNORECASE)
src_2_dir = {}
src_2_cmd = {}
src_order = []
cmd_2_src = {}
cmd_order = []
for step in buildlog:
    if src_pattern.match(step['file']):
        if step['file'] not in src_2_dir:
            src_2_dir[step['file']] = step['directory']
        if step['file'] not in src_2_cmd:
            src_2_cmd[step['file']] = step['command']
            src_order.append(step['file'])
        if step['command'] not in cmd_2_src:
            cmd_2_src[step['command']] = [step['file']]
            cmd_order.append(step['command'])
        else:
            cmd_2_src[step['command']].append(step['file'])


def clear_file(filename):
    try:
        os.remove(filename)
    except OSError:
        pass


def get_command_arguments(cmd):
    had_command = False
    args = []
    for arg in cmd.split():
        if had_command and not src_pattern.match(arg):
            args.append(arg)
        if not had_command and arg.find('=') == -1:
            had_command = True
    return args


def generate_ast(source):
    cmd = src_2_cmd[source]
    args = get_command_arguments(cmd)
    arch_command = os.path.join(clang_path, 'clang-cmdline-arch-extractor') + \
        ' ' + string.join(args, ' ') + ' ' + source
    if mainargs.verbose:
        print arch_command
    arch_output = subprocess.check_output(arch_command, shell=True)
    arch = arch_output[arch_output.rfind('@')+1:].strip()
    ast_path = os.path.abspath(os.path.join(mainargs.xtuindir,
                               os.path.join('/ast/' + arch,
                                            os.path.realpath(source)[1:] +
                                            '.ast')[1:]))
    try:
        os.makedirs(os.path.dirname(ast_path))
    except OSError:
        if os.path.isdir(os.path.dirname(ast_path)):
            pass
        else:
            raise
    dir_command = 'cd ' + src_2_dir[source]
    ast_command = os.path.join(clang_path, 'clang') + ' -emit-ast ' + \
        string.join(args, ' ') + ' -w ' + source + ' -o ' + ast_path
    if mainargs.verbose:
        print dir_command + " && " + ast_command
    subprocess.call(dir_command + " && " + ast_command, shell=True)


def map_functions(command):
    args = get_command_arguments(command)
    sources = cmd_2_src[command]
    dir_command = 'cd ' + src_2_dir[sources[0]]
    funcmap_command = os.path.join(clang_path, 'clang-func-mapping') + \
        ' --xtu-dir ' + os.path.abspath(mainargs.xtuindir) + ' ' + \
        string.join(sources, ' ') + ' -- ' + string.join(args, ' ')
    if mainargs.verbose:
        print funcmap_command
    subprocess.call(dir_command + " && " + funcmap_command, shell=True)

clear_file(os.path.join(mainargs.xtuindir, 'cfg.txt'))
clear_file(os.path.join(mainargs.xtuindir, 'definedFns.txt'))
clear_file(os.path.join(mainargs.xtuindir, 'externalFns.txt'))
clear_file(os.path.join(mainargs.xtuindir, 'externalFnMap.txt'))

original_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
ast_workers = multiprocessing.Pool(processes=int(mainargs.threads))
signal.signal(signal.SIGINT, original_handler)
try:
    res = ast_workers.map_async(generate_ast, src_order)
    # Block with timeout so that signals don't get ignored, python bug 8296
    res.get(mainargs.timeout)
except KeyboardInterrupt:
    ast_workers.terminate()
    ast_workers.join()
    exit(1)
else:
    ast_workers.close()
    ast_workers.join()

original_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
funcmap_workers = multiprocessing.Pool(processes=int(mainargs.threads))
signal.signal(signal.SIGINT, original_handler)
try:
    res = funcmap_workers.map_async(map_functions, cmd_order)
    res.get(mainargs.timeout)
except KeyboardInterrupt:
    funcmap_workers.terminate()
    funcmap_workers.join()
    exit(1)
else:
    funcmap_workers.close()
    funcmap_workers.join()


# Generate externalFnMap.txt

func_2_file = {}
func_2_shortname = {}
extfunc_2_file = {}
func_2_fileset = {}
invalid_func_names = set()

defined_fns_filename = os.path.join(mainargs.xtuindir, 'definedFns.txt')
with open(defined_fns_filename,  'r') as defined_fns_file:
    for line in defined_fns_file:
        mangledname, filename, fnname = line.strip().split(' ')
        if mangledname.startswith('!'):
            mangledname = mangledname[1:]  # main function
        if mangledname not in func_2_file.keys():
            func_2_fileset[mangledname] = set([filename])
        else:
            func_2_fileset[mangledname].add(filename)
        func_2_file[mangledname] = filename
        #if fnname not in func_2_shortname.values():
        #    func_2_shortname[mangledname] = fnname
        #elif mangledname not in func_2_shortname.keys():
        #    invalid_func_names.add(fnname)

extern_fns_filename = os.path.join(mainargs.xtuindir, 'externalFns.txt')
with open(extern_fns_filename,  'r') as extern_fns_file:
    for line in extern_fns_file:
        line = line.strip()
        if line in func_2_file and line not in extfunc_2_file:
            extfunc_2_file[line] = func_2_file[line]

extern_fns_map_filename = os.path.join(mainargs.xtuindir, 'externalFnMap.txt')
with open(extern_fns_map_filename, 'w') as out_file:
    for func, fname in extfunc_2_file.items():
        if len(func_2_fileset[func]) == 1:
#            if func_2_shortname[func] in invalid_func_names:
#                out_file.write('%s %s %s.ast\n' % (func, "__invalid__", fname))
#            else:
             out_file.write('%s %s.ast\n' % (func, fname))


# Build dependency graph

script_dir = os.path.dirname(os.path.realpath(__file__))
graph_script = os.path.join(script_dir, 'lib/xtu-build-graph.py')
graph_script += ' -b ' + mainargs.buildlog + ' -o '
graph_script += os.path.join(mainargs.xtuindir, 'build_dependency.json')
subprocess.call(graph_script, shell=True)
