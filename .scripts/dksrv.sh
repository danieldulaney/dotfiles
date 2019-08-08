#! /usr/bin/env zsh

PORT=${1:-80}
DIR=${2:-$(pwd)}

echo "Serving dir ${DIR} from port ${PORT}"

# Run a server under Docker
docker run --rm \
    --name 'dksrv.sh-server' \
    -v $DIR:/usr/local/apache2/htdocs \
    -p $PORT:80 \
    httpd

