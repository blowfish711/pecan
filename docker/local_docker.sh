#!/bin/bash

help () {
    echo "
This is a script to facilitate PEcAn development using Docker.

ARGUMENTS:

  -t [TAG]   (Required) Set the tag for the container.

  -c         'Clean'.  Remove containers with the given tag before installing.

  -r         'Run'. After building the containers, start them with docker-compose.

  -b [BASE]  'Base' image, if image with TAG doesn't exist. Defaults to 'develop'.

"
}

while getopts t:crb: opt; do
    case $opt in
	t)
	    TAG="${OPTARG}"
	    ;;
	c)
	    CLEAN=yes
	    ;;
	r)
	    RUN=yes
	    ;;
	b)
	    BASE="${OPTARG}"
	    ;;
	\?)
	    echo "Invalid option" >&2
	    help >&2
    esac
done

if [[ ! "$TAG" ]]; then
    echo "TAG (-t) is a required argument." >&2
    help >&2
    exit
fi

DOCKER_WEB="pecan/web:$TAG"
DOCKER_CODE="pecan/executor:$TAG"

if [[ "$CLEAN" ]]; then
    echo "CLEAN was specified."
    echo "Removing old $DOCKER_WEB and $DOCKER_CODE."
    docker image rm $DOCKER_WEB
    docker image rm $DOCKER_CODE
fi

WEB_EXISTS=$(docker image ls $DOCKER_WEB --quiet)
CODE_EXISTS=$(docker image ls $DOCKER_CODE --quiet)

if [[ "$WEB_EXISTS" ]]; then
    WEB_PREFIX=${TAG}
else
    echo "Building web image from version: develop"
    WEB_PREFIX=${BASE}
fi

echo "Creating $DOCKER_WEB"
docker build \
       --tag="$DOCKER_WEB" \
       --file=docker/Dockerfile.local_web \
       --build-arg PECAN_VERSION=${WEB_PREFIX} \
       .

if [[ "$CODE_EXISTS" ]]; then
    CODE_PREFIX=${TAG}
else
    echo "Building code image from version: develop"
    CODE_PREFIX=${BASE}
fi

echo "Creating $DOCKER_CODE"
docker build \
       --tag="$DOCKER_CODE" \
       --file=docker/Dockerfile.local \
       --build-arg PECAN_VERSION=${CODE_PREFIX} \
       .

if [[ "$RUN" ]]; then
    echo "Running docker-compose"
    PECAN_VERSION=${TAG} docker-compose -p pecan up -d
fi
