syntax enable

set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=4
set smarttab

filetype indent on

set mouse=a

set number
set nowrap
set wildmenu

inoremap jk <Esc>

nnoremap <Space> @q

" Set up the color column
set colorcolumn=80,120
highlight ColorColumn ctermbg=0

" Set vimdiff colors
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText   cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red

silent! call pathogen#infect()

