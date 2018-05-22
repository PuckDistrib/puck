# Puck

This is the official repository of <a href="http://puck.lip6.fr">Puck</a>

How to compile:

Once you've installed <a href="http://www.scala-sbt.org/">sbt</a>,
you can run from the root of the project 'sbt universal:packageZipTarball'
which will produce an a zipped version of the program in puckExtendJ/target/universal/puck-distrib-<current-time>.tgz

alternatively you can run 'sbt printClassPathFile' which will compile the source and generate a file
puckJrrt/target/CLASSPATH that define a bash script that create a CLASSPATH variable with the classpath of the project.
You can use it to run the following command to start puck:   'java -cp $CLASSPATH puck.Front'

