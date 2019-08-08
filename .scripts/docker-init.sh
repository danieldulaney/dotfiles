#! /usr/bin/env sh

echo Container started! Inititalizing...

echo Starting ssh-agent...
eval $(ssh-agent)
echo Done!

echo Adding ssh keys...
ssh-add
echo Done!

echo Starting tmux...

exec tmux

