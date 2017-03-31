#!/bin/sh

LOCAL_MAVEN="~/.m2/repository"

./pants publish.jar --no-dryrun --local=$LOCAL_MAVEN src/main/scala/lucassonnabend/finiteautomata:finiteautomata
