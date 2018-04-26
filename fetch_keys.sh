#! /usr/bin/env bash

echo "$1"

curl "https://github.com/$1.keys" >> ~/.ssh/authorized_keys

