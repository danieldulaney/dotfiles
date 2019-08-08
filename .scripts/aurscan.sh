#! /usr/bin/env bash

aurdir="~/aur"
red="\033[0;31m"
green="\033[0;32m"
yellow="\033[1;33m"
nocolor="\033[0m"

okecho() {
    printf "${green} -> ${nocolor}${1}\n"
}

warnecho() {
    printf "${yellow} ~> ${nocolor}${1}\n"
}

errecho() {
    printf "${red} => ${nocolor}${1}\n"
}

for dir in ~/aur/*/; do
    pushd $dir > /dev/null
    pwd
    git fetch --quiet

    local_branch=$(git branch --show-current)
    remote_branch=$(git for-each-ref --format='%(upstream:short)' "$(git symbolic-ref -q HEAD)")

    if [[ $local_branch != 'master' ]]; then
        warnecho "Local branch is ${local_branch}, not master"
    fi

    if [[ -z $remote_branch ]]; then
        warnecho "No remote branch"
    else
        ahead=$(git rev-list --left-right --count $local_branch...$remote_branch | cut -f1)
        behind=$(git rev-list --left-right --count $local_branch...$remote_branch | cut -f2)

        if (( $ahead > 0 )); then
            warnecho "$local_branch is $ahead ahead"
        fi

        if (( $behind > 0 )); then
            warnecho "$local_branch is $behind behind"
        fi
    fi

    popd > /dev/null
done
