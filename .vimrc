syntax enable

set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=4
set smarttab

filetype indent on

set number
set nowrap
set wildmenu

inoremap jk <Esc>

if exists("pathogen#infect")
    execute pathogen#infect()
endif

