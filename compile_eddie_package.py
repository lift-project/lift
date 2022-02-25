import argparse
import re
import shutil
from distutils import dir_util, file_util
import errno
import ntpath
import os
from os.path import join
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--java_cmd_log', action="store", dest="java_cmd_log", required=True)
# parser.add_argument('--destination_dir', action="store", dest="destination_dir", required=True)
parser.add_argument('--with_jdk_jars', action="store_true", dest="with_jdk_jars")
# parser.add_argument('--destination_zip', action="store", dest="destination_zip", required=True)
parser.add_argument('--run_label', action="store", dest="run_label", required=True)
parser.add_argument('--par_mappings_csv_path', action="store", dest="par_mappings_csv_path", required=False)
parser.add_argument('--choose_local_size_deterministically', action="store_true", dest="choose_local_size_deterministically", required=False)

args = parser.parse_args()
 
def copy(src, dest):
    try:
        dir_util.copy_tree(src, dest)
    except OSError as e:
        # If the error was caused because the source wasn't a directory
        if e.errno == errno.ENOTDIR:
            file_util.copy_file(src, dest)
        elif e.errno == errno.EEXIST:
            print('Directory already exists')
        else:
            print('Directory not copied. Error: %s' % e)

def execute(cmd, sameline=False):
    print("EXECUTING '" + " ".join(cmd) + "'")
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
    while True:
        output = process.stdout.readline()
        if output == '' and process.poll() is not None:
            break
        if output:
            print(("\033[F" if sameline else "") + output.strip("\n"))

    process.stdout.close()
    process.wait()
    print("\n\n")

    
if args.par_mappings_csv_path is None:
    par_mappings_csv_val = ""
else:
    new_par_mappings_csv_path = join("eddie_package", "src", "test", "resources", ntpath.basename(args.par_mappings_csv_path))

    print("Copying " + args.par_mappings_csv_path + " to " + new_par_mappings_csv_path)
    execute(["rsync", "-a", "--info=progress2", args.par_mappings_csv_path, new_par_mappings_csv_path])

    eddie_par_mappings_csv_path = "/" + join("home", "s1569687", "eddie_package", "src", "test", "resources", ntpath.basename(args.par_mappings_csv_path))

    par_mappings_csv_val = "--par_mappings_csv " + eddie_par_mappings_csv_path

print("Copying eddie_run.scheduler.template.sh to eddie_run.scheduler.sh and...")
print("...replacing RUN_LABEL_VAL_TO_BE_REPLACED to \"" + args.run_label + "\" in " + "eddie_package/eddie_run.scheduler.sh")
# execute(["sed", "-i", "\"s~PAR_MAPPINGS_CSV_VAL_TO_BE_REPLACED~--par_mappings_csv " + new_par_mappings_csv_path + "~g\"", join("eddie_package", "eddie_run.template.sh")])

with open(join("eddie_package", "eddie_run.scheduler.template.sh"), "rt") as fin:
    with open(join("eddie_package", "eddie_run.scheduler.sh"), "wt") as fout:
        for line in fin:
            fout.write(line.replace("RUN_LABEL_VAL_TO_BE_REPLACED", args.run_label))

execute(["chmod", "+x", join("eddie_package", "eddie_run.scheduler.sh")])


print("Copying eddie_run.template.template.sh to eddie_run.template.sh and...")
print("...replacing PAR_MAPPINGS_CSV_VAL_TO_BE_REPLACED to \"" + par_mappings_csv_val + "\" in " + "eddie_package/eddie_run.template.sh")
# execute(["sed", "-i", "\"s~PAR_MAPPINGS_CSV_VAL_TO_BE_REPLACED~--par_mappings_csv " + new_par_mappings_csv_path + "~g\"", join("eddie_package", "eddie_run.template.sh")])

extra_cmd_options = ""
if args.choose_local_size_deterministically:
    extra_cmd_options += "--choose_local_size_deterministically"

with open(join("eddie_package", "eddie_run.template.template.sh"), "rt") as fin:
    with open(join("eddie_package", "eddie_run.template.sh"), "wt") as fout:
        for line in fin:
            fout.write(line
                .replace("PAR_MAPPINGS_CSV_VAL_TO_BE_REPLACED", par_mappings_csv_val)
                .replace("EXTRA_CMD_OPTIONS", extra_cmd_options))

p_full_classpath = re.compile(r" -classpath (?P<full_classpath>\".[^\"]+?\") ")
if args.with_jdk_jars:
    p_a_classpath = re.compile(r"(;|\"|\r|\n)(?P<a_classpath>[^;\"\r\n]*?[^;\"\r\n]*?)(;|\")")
else:
    p_a_classpath = re.compile(r"(;|\"|\r|\n)(?!.*jdk1\.8\.0_241)(?P<a_classpath>[^;\"\r\n]*?[^;\"\r\n]*?)(;|\")")

with open(args.java_cmd_log, "r", newline="") as f:
    m_full_classpath = p_full_classpath.search(f.read())
    if m_full_classpath is None:
        raise IOError
    for m_a_class_path in p_a_classpath.finditer(m_full_classpath.group("full_classpath")):
        path = m_a_class_path.group("a_classpath").replace("\\", "/").replace("C:", "/mnt/c")
        print("Copying " + path + " to " + "eddie_package")
        # copy(path.replace("\\", "/").replace("C:", "/mnt/c"), 
        #     join("eddie_package", ntpath.basename(path)))
        execute(["rsync", "-a", "--info=progress2", path,
            join("eddie_package", "classpath")], sameline=True)
        # print("Copying " + path + " to " + args.destination_zip)
        # execute(["zip", "-0", "-u", args.destination_zip, path.replace("\\", "/").replace("C:", "/mnt/c")])
        # quit()

# Source
execute(["rsync", "-a", "--info=progress2", "/mnt/c/Users/Naums/lift-rewrite-conv/src", "eddie_package"], sameline=True)
execute(["mkdir",  "-p", "eddie_package/lib"], sameline=True)
execute(["rsync", "-a", "--info=progress2", "/mnt/c/Users/Naums/lift-dnnoptimiser-gpgpu/lib/ArithExpr", "eddie_package/lib/"], sameline=True)
execute(["tar", "-czvf", "src-lib.tar.gz", "src", "lib"], sameline=True)

# Logback
os.rename(join("eddie_package", "src", "test", "resources", "logback-test-eddie.xml"),
          join("eddie_package", "src", "test", "resources", "logback-test.xml"))
execute(["rsync", "-a", "--info=progress2", 
    join("eddie_package", "src", "test", "resources", "logback-test.xml"), 
    join("eddie_package", "classpath", "test-classes", "resources", "logback-test.xml")], sameline=True)
execute(["rsync", "-a", "--info=progress2", 
    join("eddie_package", "src", "test", "resources", "logback-test.xml"), 
    join("eddie_package", "classpath", "test-classes", "logback-test.xml")], sameline=True)

# Archive
execute(["zip", "-ur", "eddie_package.zip", "eddie_package"])
