#!/bin/sh

# ./pants publish --publish-jar-local=/home/lucas/.m2/repository src/main/scala/lucassonnabend/finiteautomata:finiteautomata

./pants publish.jar --no-dryrun --local=~/.m2/repository src/main/scala/lucassonnabend/finiteautomata:finiteautomata
