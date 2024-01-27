#!/bin/bash

##
## Reset the entire Docker state
##
## Don't use this is you are not sure if you might still need any docker
## images, containers, volumes or networks!
##

if [ "$(docker ps -q)" ]
then
  docker kill $(docker ps -q)
fi

if [ "$(docker ps -qa)" ]
then
  docker container rm $(docker ps -qa)
fi

if [ "$(docker image ls -q)" ]
then
  docker image rm $(docker image ls -q)
fi

if [ "$(docker volume ls -q)" ]
then
  docker volume rm $(docker volume ls -q)
fi
