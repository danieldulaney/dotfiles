#! /usr/bin/env bash

alias ll='ls -lh --color'

export PATH="$HOME/.cargo/bin:$PATH"
export GOPATH="$HOME/go"

# Set up prompt
ps1_username() {
    if [ $EUID == 0 ]; then
        echo -e '\e[31m\u\e[0m'
    else
        echo -e '\e[36m\u\e[0m'
    fi
}
ps1_dollar() {
    if [ $EUID == 0 ]; then
        echo -e '\e[32m\$\e[0m'
    else
        echo '\$'
    fi
}
export PS1='['
export PS1+=$(ps1_colored)
export PS1+='\e[36m@\h\e[0m'
export PS1+='] '

export PS1+='\e[35m\w\e[0m'
export PS1+=' '

export PS1+='\n'
export PS1+=$(ps1_dollar)
export PS1+=' '

source ~/.bashrc

