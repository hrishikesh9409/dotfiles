set clipboard^=unnamed "This sets the clipboard as the default register. Useful for copy paste from tmux
set number " displays line number
syntax enable "enables syntax highlighting
set mouse=a "sane selection without line numbers
set tabstop=4
set shiftwidth=4

set virtualedit=onemore "cursor goes one more position than the usual
set laststatus=0
set mouse=a "sane selection without line numbers
set tabstop=4
set shiftwidth=4
filetype plugin indent on "identify the kind of filetype automatically
set shortmess=A "prevent vim from giving a warning it the swp file is open 
set cursorline
set encoding=utf8
set ignorecase
set nobackup
set nocompatible "This tells vim not to act like it predecessor vi
set autochdir
set hidden "unsaved buffer wont close when opening a new buffer/file
set relativenumber

" ------------------------------------------------------------------------
set rtp+=~/.config/nvim/autoload/plug.vim
call plug#begin()
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'SirVer/ultisnips'
Plug 'ap/vim-buftabline'
Plug 'morhetz/gruvbox'
Plug 'Valloric/YouCompleteMe'
call plug#end()

" -----------------------------------------------------------------------------------------
" This sets the color scheme
set background=dark
colorscheme gruvbox

" -----------------------------------------------------------------------------------------
" autocomplete 
let g:ycm_global_ycm_extra_conf = '$HOME/dotfiles/nvim/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_auto_trigger = 1
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_autoclose_preview_window_after_insertion = 1
set backspace=indent,eol,start
"let g:ycm_semantic_triggers = { 'cpp': [ 're!.' ] } " gives autocomplete without the need to press C-Space
" autocompelte using omnisharp
 "let g:OmniSharp_server_use_mono = 1

" -----------------------------------------------------------------------------------------
" Nerd Tree file manager
let g:NERDTreeWinSize=60 
map <C-f> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeQuitOnOpen=1 " closes upon opening a file in nerdtree
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '~'

" -------------------------------------------------------------------------------
" highlight a TODO occurrence in bright red
:highlight TODO cterm=italic ctermfg=196 guifg=#ff0000
:match TODO /TODO/

" -------------------------------------------------------------------------------
" terminal and make commands
:imap <c-x><c-x> <Esc>:update<CR>\|<Esc>:!make && make run<CR>  
:nmap <c-x><c-x> :update<CR>\|<Esc>:!make && make run<CR>


" ------------------------------------------------------------------------------
" UltiSnips stuff 
let g:UltiSnipsExpandTrigger = "<nop>"
inoremap <expr> <CR> pumvisible() ? "<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>" : "\<CR>"
let g:UltiSnipsSnippetDirectories = ['$HOME/.config/nvim/UltiSnips', 'UltiSnips'] " dont change the directory; symlink it from dotfiles


" press // for comment using nerd commenter
nmap // <leader>c<space>
vmap // <leader>c<space>


" -----------------------------------------------------------------------------------------
" keyboard shortcuts 
:nmap <c-n> :bnext<CR>
:nmap <c-p> :bprev<CR>
:ab Wq :wq
:ab W :w
:ab WQ :wq
:ab Q :q
:ab Ww :w
:ab wW :w
:ab WW :w
:set guitablabel=%t  " show only the file name an not the path 
:au FocusLost * :wa  " save when focus is lost (not sure if this is working. Test)
:imap vv <Esc>v
:nmap vv <Esc>v
:imap <c-l> <Esc>la
