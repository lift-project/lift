#!/usr/bin/env python
import os
import subprocess
import re
import sys

scriptRoot=os.path.dirname(os.path.realpath(__file__))
projectRoot=os.path.dirname(scriptRoot)

os.chdir(projectRoot)

# SBT now includes colour codes in the output. We need to strip them.
# This solution uses a regex to match and substitute them. Regex taken from
# http://superuser.com/questions/380772/removing-ansi-color-codes-from-text-stream
def remove_colour_codes(string):
	ccrgx = re.compile('\\x1b\[[0-9;]*m')
	return ccrgx.sub('', string)

# Get data from SBT
print("Classpath: ")
classpath = remove_colour_codes(subprocess.check_output(["sbt", "show runtime:fullClasspath"]).decode())
print(classpath)

print("mainClasses: ")
mainClasses = remove_colour_codes(subprocess.check_output(["sbt", "show discoveredMainClasses"]).decode())
print(mainClasses)

print("javaOptions: ")
javaOptions = remove_colour_codes(subprocess.check_output(["sbt", "show javaOptions"]).decode())
print(javaOptions)

# Build a regex to match mainclasses and java options
mcjoRegex = re.compile("\[info\]\s+List\((.*)\)")

# Extract the classes 
mainClassesSections = mcjoRegex.findall(mainClasses)
mainClasses = [ x for x in [x.strip(" ") for x in sum([ x for x in [section.split(",") for section in mainClassesSections]],[])] if x != ""]

print("Discovered mainClasses: ")
for mc in mainClasses: 
	print("\t - " + mc)

requiredMainClasses = [
	"benchmarks.conv.gpgpu20.layeroptimiser.DirectConvManuallyParallelizedConvGenerator",
	"benchmarks.conv.ConvExploration"]

classNotDiscovered = False
for rmc in requiredMainClasses:
	if rmc not in mainClasses:
		print("Required main class \"" + rmc + "\" was not discovered. Something might have went wrong during Lift compilation.")
		classNotDiscovered = True

if classNotDiscovered:
    quit()

# And Java options
javaOptions = mcjoRegex.findall(javaOptions)
print("Discovered javaOptions: ")
for jo in javaOptions: 
	print("\t - " + jo)

# Start building a command to print to a file
command = "#!/bin/bash\n\njava $1 $2 $3 "

# Add java options
command += ' '.join(javaOptions)

command += " -cp "

# Add things from classpath
command += ':'.join(re.compile("Attributed\(([^\)]*)\)").findall(classpath))

generatedScriptsFolder = scriptRoot + "/compiled_scripts/"

# make a folder to store the compiled scripts
subprocess.call(["mkdir", "-p", generatedScriptsFolder])

# for each of the mainclasses, build a runscript
for clazz in requiredMainClasses:
	finalCommand = command + " " + clazz + " $4\n"
	fileName = generatedScriptsFolder + clazz.split(".")[-1]
	f = open(fileName, "w")
	f.write(finalCommand)
	f.close()
	subprocess.call(["chmod", "u+x", fileName])
	print("Created a script for " + clazz)
