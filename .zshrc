# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.dotfiles/oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="tjkirch"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

# User configuration

# Vim should be the VISUAL editor
export VISUAL=vim
export EDITOR=vim

# Add the .dotfiles scripts folder to the path
export PATH="$HOME/.dotfiles/scripts:$PATH"

# If the SSH_AUTH_SOCK env variable isn't set, set it
if [ ! -v SSH_AUTH_SOCK ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi

# If ~/.cargo exists add $HOME/.cargo/bin to the PATH
if [[ -d $HOME/.cargo ]]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Add $HOME/.local/bin to the PATH
export PATH="$HOME/.local/bin:$PATH"

# Docker aliases
alias dk=docker
# Docker build and run current or previous dir
#dkbr() {
#    if [ "$#" -eq 0 ] ; then
#        location='.'
#        arguments=''
#    elif [ "$#" -eq 1 ] ; then
#        location=${@:1}
#        arguments=''
#    else
#        location="${@:$#}"
#        arguments="${@:1:(($# - 1))}"
#    fi
#
#    container=$(docker build -q "${location}")
#    docker run -it --rm "${arguments}" "${container}"
#}
alias dkbr='docker run -it --rm $(docker build -q .)'
# Docker list all containers
dkpsa() {
    docker ps --all
}

alias dclogs='docker-compose logs -f'
alias dcr='docker-compose restart'
alias dcup='docker-compose up -d'
alias dcdn='docker-compose down'
alias dcmon='docker-compose up -d && docker-compose logs -f'
function dcsh() {
    docker-compose exec "${1}" "${2:-bash}" "${@:3}"
}

# xclip should actually copy things
alias clip='xclip -selection clipboard'

# vi should open vim
alias vi='vim'

neofetch --disable gpu
