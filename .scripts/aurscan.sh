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
    ok=1
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
            ok=0
        fi

        if (( $behind > 0 )); then
            warnecho "$local_branch is $behind behind"
            ok=0
        fi
    fi

    pkgname=$(echo 'echo $pkgname' | cat PKGBUILD - | bash)
    local_version=$(echo 'echo $pkgver-$pkgrel' | cat PKGBUILD - | bash)
    remote_version=$(curl -Ls 'https://aur.archlinux.org/rpc/?v=5&type=info&arg='"$pkgname" | jq -r '.results[0].Version')

    if [[ $local_version != $remote_version ]]; then
        warnecho "Local version $local_version doesn't match AUR version $remote_version"
        ok=0
    fi

    if [ $ok ]; then
        okecho "Up-to-date"
    fi

    popd > /dev/null
done
