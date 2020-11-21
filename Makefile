.PHONY: all clean
all:
ifeq ($(shell whoami), vagrant)
	sbt "set assemblyJarName := \"cigrid.jar\"" "set assemblyOutputPath in assembly := file(\"$(shell pwd)/cigrid.jar\")" assembly
else
	sbt --batch "set assemblyJarName := \"cigrid.jar\"" "set assemblyOutputPath in assembly := file(\"$(shell pwd)/cigrid.jar\")" assembly < /dev/null
endif


clean:
	sbt clean