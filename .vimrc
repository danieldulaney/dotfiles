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

set hlsearch

inoremap jk <Esc>

nnoremap <Space> @q

" Web stuff tends to get very indented
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2
autocmd FileType html       setlocal shiftwidth=2 tabstop=2

" Makefiles are a special snowflake
autocmd FileType make       setlocal noexpandtab nosmarttab shiftwidth=8

" Set up the color column
set colorcolumn=80,100,120
highlight ColorColumn ctermbg=0

" Set vimdiff colors
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText   cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red

silent! call pathogen#infect()

" Use flake8 if available
let g:syntastic_python_python_exec='python3'
let g:syntastic_python_checkers=['python']
