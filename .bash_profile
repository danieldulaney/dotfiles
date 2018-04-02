#! /usr/bin/env bash

# Useful aliases
alias ll='ls -lh --color'
alias l='ls -lh --color'
alias la='ls -lhA --color'

export PATH="$HOME/.cargo/bin:$PATH"

# Set up prompt
ps1_username_color() {
    if [ $EUID == 0 ]; then
        echo -e '\e[31m'
    else
        echo -e '\e[36m'
    fi
}
ps1_dollar_color() {
    if [ $EUID == 0 ]; then
        echo -e '\e[32m'
    else
        echo -e '\e[0m'
    fi
}
in_git_repo() {
    git rev-parse --git-dir > /dev/null 2>&1
}
ps1_branch() {
    branch_name=$(git symbolic-ref HEAD 2> /dev/null)
    if [ $? == 0 ]; then
        branch_name="on $(basename $branch_name)"
    else
        branch_name="at $(git rev-parse --short HEAD)"
    fi

    echo -en "\e[34m$branch_name\e[0m"
}
ps1_git_segment() {
    if in_git_repo ; then
        echo -en ' ('
        echo -en $(ps1_branch)
        echo -en ')'
    fi
}

# Username and hostname
export PS1='['
export PS1+='$(ps1_username_color)'
export PS1+='\u\e[0m'
export PS1+='\e[36m@\h\e[0m'
export PS1+=']'

# Working directory
export PS1+=' '
export PS1+='\e[35m\w\e[0m'

# Git segment
export PS1+='$(ps1_git_segment)'

# Dollar sign on next line
export PS1+='\n$(ps1_dollar_color)'
export PS1+='\$\e[0m'
export PS1+=' '

source ~/.bashrc

