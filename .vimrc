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

" Set up the color column
set colorcolumn=80,120
highlight ColorColumn ctermbg=0

silent! call pathogen#infect()

