#!/usr/bin/python
import re
import sys

pattern = re.compile("MEDIAN: (.*) ms")

filename = sys.argv[1]

inputFile = open(filename)
outputFile = open(filename + "Parsed", "w")

line = inputFile.readline()

CL_BUILD_PROGRAM_FAILURE = "CL_BUILD_PROGRAM_FAILURE"

conf = ""

while line != "":
    if line == "------------------------------\n":
        conf = inputFile.readline()[:-1]

    if "OpenCL error" in line:
        print "error"
        if CL_BUILD_PROGRAM_FAILURE in line:
            print "Compile error with " + conf
    elif "MEDIAN" in line:
        outputFile.write(conf + "," + pattern.search(line).group(1) + "\n")

    line = inputFile.readline()

inputFile.close()
outputFile.close()
