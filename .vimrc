" General Settings{
    set nocompatible        " Less vi-ey. Still vi-ey enough though
    filetype on             " Allows filetype checking
    set matchpairs=<:>,{:},(:),[:]
    set modelines=0         " Some security thing, I guess
    set encoding=utf-8      " Standardizes text to UTF-8
    set noswapfile          " No more swap file nonsense
    set backspace=indent,eol,start
    set foldlevel=100       " Unfolds everything by default
    set foldmethod=indent   " Folds based on indentation
    set list                " Puts below characters in their places
    set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
    set showcmd             " Shows commands
    set lazyredraw          " Redraw only when necessary
    let mapleader = "\<Space>"   " Sets leader key to spacebar

    " Wildmenu settings{
        set wildmenu                    " Graphical menu of autocomplete options
        set wildmode=list:full
        set wildignore=*.o,*.obj,*~     " stuff to ignore when tab completing
        set wildignore+=*vim/backups*
        set wildignore+=*sass-cache*
        set wildignore+=*DS_Store*
        set wildignore+=vendor/rails/**
        set wildignore+=vendor/cache/**
        set wildignore+=*.gem
        set wildignore+=log/**
        set wildignore+=tmp/**
        set wildignore+=*.png,*.jpg,*.gif
    " }
    if exists("&undodir")
        set undodir=~/.vim/undo
    endif
" }

" Indentation settings{
    set autoindent          " Automatically indents lines
    set smartindent         " Inserts an extra indent in certain cases
    set shiftround          " Rounds indentation to a multiple of <shiftwidth>
    set expandtab           " Inserts <shiftwidth> spaces when tab is pressed
    set smarttab            " Tabs in front of lines are <shiftwidth> spaces
    set shiftwidth=4        " How many spaces constitute one tab
    set tabstop=4           " How many spaces a tab counts for in a file
    set expandtab           " Tabs become spaces
" }

" UI Changes{
    set number              " Puts line number in front of each line
    set numberwidth=1       " Line numbers take less space
    set incsearch           " Starts searching as soon as / is typed
    set hlsearch            " Highlights searched things
    set ignorecase          " All searches are case-insensitive
    set smartcase           " Lowercase searches are case-insensitive
    set mouse=a             " Allows mouse control
    syntax on               " Highlights syntax. It's C21. Jesus.
    filetype indent on      " Smarter indentation based on file type
    set ruler               " Shows cursor all the time
    set nowrap              " Text doesn't wrap at edge of window
    set scrolloff=5         " Margin of 5 lines around edge of screen
    set splitbelow          " Horizontal splits below current window
    set splitright          " Vertical splits to the right of current window
    set textwidth=9999      " Basically no columnal restriction
    set background=dark     " Light backgrounds look awful
    set t_Co=256            " Force 256 colours

    try " colorscheme changing if possible
        colorscheme jack    " If molokai's installed, use it
    catch
        colorscheme elflord " Otherwise, use elflord
    endtry

    try " font changing if possible
        set guifont=Menlo:h11
    catch
        set guifont=Courier\ New:h11
    endtry

    " Status Line Stuff{
        set laststatus=2        " Shows status line at all times
        " Sets status line to show full file path, line no, column no and total lines in file
        set statusline=%F\ %m
    " }
" }

" Abbreviations, Remappings, Spelling{
    " Abbreviations{
        iabbrev ldis ಠ_ಠ
        iabbrev lsad ಥ_ಥ
        iabbrev lhap ಥ‿ಥ
        iabbrev lmis ಠ‿ಠ
        iabbrev (union) ∪
        iabbrev (intersect) ∩
    " }

    " Remappings{
        " 'H' takes you to the beginning of a line, and 'L' to the end
        noremap H 0
        noremap L $

        " 'K' takes you to the top of the doc, and 'J' to the bottom
        noremap J G$
        noremap K 1G

        " 'vv' visually selects a line
        noremap vv 0v$

        " Leader + s puts me in find and replace mode
        noremap <leader>s :%s///g<left><left><left>

        " 'F1' opens help.
        noremap <F1> K

        " <c-'key'> = <c-w> + 'key'
        noremap <C-h> <C-w>h
        noremap <C-j> <C-w>j
        noremap <C-k> <C-w>k
        noremap <C-l> <C-w>l
        
        " '==' aligns whole document
        noremap == 1GvG=

        " Swap : and ;
        noremap : ;
        noremap ; :

        " Leader + m unhighlights search pattern
        noremap <leader>m :nohl <CR>

        " Hitting leader twice toggles fold
        nnoremap <leader><leader> za

        " Leader + various keys has the same effect as ':' + key + enter
        nnoremap <leader>w :up <CR>
        nnoremap <leader>q :q <CR>
        nnoremap <leader>e :e ~
        nnoremap <leader>v :vsplit ~/
        nnoremap <leader>h :split ~/
        nnoremap <leader>x :x <CR>
        nnoremap <leader>Q :q! <CR>

        " Ctrl + arrow keys resize splits
        nnoremap <C-up> 5<C-W>+
        nnoremap <C-down> 5<C-W>-
        nnoremap <C-left> 5<C-W><
        nnoremap <C-right> 5<C-W>>

        " A number n + Enter takes you to line n
        nnoremap <CR> G0

        " Shift + key does the opposite of key
        nnoremap U <C-R>
        nnoremap W b

        " Swapping the behaviour of p and shift-p, as well as autoindenting pasted text
        nnoremap p P=`]<C-o>
        nnoremap P p=`]<C-o>

        " Finally making the arrow kews do something more useful
        nnoremap <Up> {
        nnoremap <Down> }
        nnoremap <Left> b
        nnoremap <Right> w
        inoremap <Up> <Esc>{i
        inoremap <Down> <Esc>}i
        inoremap <Left> <Esc>hbi
        inoremap <Right> <Esc>lwi
        " }

    " Spelling{
        set dictionary=~/usr/share/dict/words
    " }
" }

" FileType-Specific{
    augroup dotfiles " Folding is a useful thing
        au!
        autocmd BufEnter *.vimrc :set foldmethod=marker
        autocmd BufEnter *.vimrc :set foldmarker={,}
        autocmd BufEnter .macos,.aliases,.functions :set syntax=sh
    augroup end " }

    augroup textfiles " {
        au!
        autocmd BufEnter *.txt,*.tex :iabbrev <pi> π
        autocmd BufEnter *.txt,*.tex :iabbrev <lambda> λ
        autocmd BufEnter *.txt,*.tex :iabbrev <delta> δ
        autocmd BufEnter *.txt,*.tex :iabbrev <contained> ϵ
        autocmd BufEnter *.txt,*.tex :setlocal spell spelllang=en_gb
        autocmd BufEnter *.txt :set textwidth=120
    augroup end
    " }
" }
