#!/usr/bin/env python
import os
import subprocess
import re

scriptRoot=os.path.dirname(os.path.realpath(__file__))
projectRoot=os.path.dirname(scriptRoot)

os.chdir(projectRoot)

classpath = subprocess.check_output(["sbt", "show runtime:fullClasspath"])
mainClasses = subprocess.check_output(["sbt", "show discoveredMainClasses"])

command = "#!/bin/bash\ncd \"$( dirname \"${BASH_SOURCE[0]}\" )\"\ncd ..\njava -cp "

mainClasses = re.compile("List\((.*)\)").search(mainClasses).group(1).split(", ")

for match in re.compile("Attributed\(([^\)]*)\)").finditer(classpath):
	command += match.group(1) + ":"

for clazz in mainClasses:
	finalCommand = command + " " + clazz + " $*\n"
	fileName = scriptRoot + "/" + clazz.split(".")[-1]
	f = open(fileName, "w")
	f.write(finalCommand)
	f.close()
	subprocess.call(["chmod", "u+x", fileName])
	print "Created a script for " + clazz
