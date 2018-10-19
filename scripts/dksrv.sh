#! /usr/bin/env zsh

# Run a server under Docker
docker run --rm \
    --name 'or-research-server' \
    -v ${1:-$(pwd)}:/usr/local/apache2/htdocs \
    --network=host \
    httpd:2.4-alpine \
    httpd-foreground

