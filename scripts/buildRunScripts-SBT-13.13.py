#!/usr/bin/env python
import os
import subprocess
import re
import sys

scriptRoot=os.path.dirname(os.path.realpath(__file__))
projectRoot=os.path.dirname(scriptRoot)

os.chdir(projectRoot)

classpath = subprocess.check_output(["sbt", "show runtime:fullClasspath"])
mainClasses = subprocess.check_output(["sbt", "show discoveredMainClasses"]).split("\n")
javaOptions = subprocess.check_output(["sbt", "show javaOptions"]).split("\n")

command = "#!/bin/bash\n\njava "

print "Classpath: " + (classpath)
print "mainClasses: " + str(mainClasses)
print "javaOptions: " + str(javaOptions)

mcjoRegex = "(.*)"
# mcjoRegex = "List\((.*)\)"
# mcjoRegex = "\[info\] \* (.*)"


mainClasses = map(lambda mc: re.compile(mcjoRegex).search(mainClasses).group(1), mainClasses)
javaOptions = re.compile(mcjoRegex).search(javaOptions).group(1).split(", ")

print mainClasses
print javaOptions

for option in javaOptions:
  command += option + " "

command += "-cp "

for match in re.compile("Attributed\(([^\)]*)\)").finditer(classpath):
	command += match.group(1) + ":"

for clazz in mainClasses:
	finalCommand = command + " " + clazz + " $*\n"
	fileName = scriptRoot + "/compiled_scripts/" + clazz.split(".")[-1]
	f = open(fileName, "w")
	f.write(finalCommand)
	f.close()
	subprocess.call(["chmod", "u+x", fileName])
	print "Created a script for " + clazz
