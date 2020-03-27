#! /usr/bin/env bash

root=$(git rev-parse --show-toplevel)
cd "$root"

echo "Resetting staging area"
git reset

echo "Ensuring we have the latest .dotfiles repo"
git pull

pushd .oh-my-zsh
echo "Explicitly checking out oh-my-zsh master branch"
git checkout master

echo "Pulling the newest oh-my-zsh"
git pull
popd

git add .oh-my-zsh
git commit -m "Updated oh-my-zsh"
git push
