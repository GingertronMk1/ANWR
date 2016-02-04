"General Settings{{{
    set nocompatible        "Less vi-ey. Still vi-ey enough though
    filetype on             "Allows filetype checking
    set matchpairs=<:>,{:},(:),[:]
    set modelines=0         "Some security thing, I guess
    set encoding=utf-8      "Standardizes text to UTF-8
    set noswapfile          "No more swap file nonsense
    set backspace=indent,eol,start
    set foldlevel=100       "Unfolds everything by default
    set foldmethod=indent   "Folds based on indentation
    set list                "Puts below characters in their places
    set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
    set showcmd             "Shows commands
    set lazyredraw          "Redraw only when necessary
    let mapleader = "\<Space>"  "Sets leader key to spacebar
    "Wildmenu settings{{{
        set wildmenu                    "Graphical menu of autocomplete options
        set wildmode=list:full
        set wildignore=*.o,*.obj,*~     "stuff to ignore when tab completing
        set wildignore+=*vim/backups*
        set wildignore+=*sass-cache*
        set wildignore+=*DS_Store*
        set wildignore+=vendor/rails/**
        set wildignore+=vendor/cache/**
        set wildignore+=*.gem
        set wildignore+=log/**
        set wildignore+=tmp/**
        set wildignore+=*.png,*.jpg,*.gif
    "}}}
"}}}

"Indentation settings{{{
    set autoindent          "Automatically indents lines
    set smartindent         "Inserts an extra indent in certain cases
    set shiftround          "Rounds indentation to a multiple of <shiftwidth>
    set expandtab           "Inserts <shiftwidth> spaces when tab is pressed
    set smarttab            "Tabs in front of lines are <shiftwidth> spaces
    set shiftwidth=4        "How many spaces constitute one tab
    set tabstop=4           "How many spaces a tab counts for in a file
    set expandtab           "Tabs become spaces
"}}}

"UI Changes{{{
    set number              "Puts line number in front of each line
    set numberwidth=1       "Line numbers take less space
    set incsearch           "Starts searching as soon as / is typed
    set hlsearch            "Highlights searched things
    set mouse=a             "Allows mouse control
    syntax on               "Highlights syntax. It's C21. Jesus.
    filetype indent on      "Smarter indentation based on file type
    set ruler               "Shows cursor all the time
    set ignorecase          "All searches are case-insensitive
    set smartcase           "Lowercase searches are case-insensitive
    set nowrap              "Text doesn't wrap at edge of window
    set scrolloff=5         "Margin of 5 lines around edge of screen
    set relativenumber      "Number shows how far a line is from the current
    set splitbelow          "Horizontal splits below current window
    set splitright          "Vertical splits to the right of current window
    set textwidth=9999      "Basically no columnal restriction
    try
        colorscheme molokai "If molokai's installed, use it
    catch
        colorscheme elflord "Otherwise, use elflord
    endtry
    try
        set guifont=Menlo:h11
    catch
        set guifont=Courier\ New:h11
    endtry
    "Statuc Line Stuff{{{
        set laststatus=2        "Shows status line at all times
        "Sets status line to show full file path, line no, column no and total lines in file
        set statusline=%F\ %m
    "}}}
"}}}

"Abbreviations, Remappings{{{
    "Abbreviations{{{
        iabbrev ldis ಠ_ಠ
        iabbrev lsad ಥ_ಥ
        iabbrev lhap ಥ‿ಥ
        iabbrev lmis ಠ‿ಠ
        iabbrev (union) ∪
        iabbrev (intersect) ∩
    "}}}
    "Remappings{{{
        "'H' takes you to the beginning of a line, and 'L' to the end
        noremap H 0
        noremap L $

        "'K' takes you to the top of the doc, and 'J' to the bottom
        noremap J G$
        noremap K 1G

        "'vv' visually selects a line
        noremap vv 0v$

        "Leader + s puts me in find and replace mode
        noremap <leader>s :%s///g<left><left><left>

        "'F1' opens help.
        noremap <F1> K

        "<c-'key'> = <c-w> + 'key'
        noremap <C-h> <C-w>h
        noremap <C-j> <C-w>j
        noremap <C-k> <C-w>k
        noremap <C-l> <C-w>l
        
        "'==' aligns whole document
        noremap == 1GvG=

        "Swap : and ;
        noremap : ;
        noremap ; :

        "Leader + m unhighlights search pattern
        noremap <leader>m :nohl <CR>

        "Hitting leader twice toggles fold
        nnoremap <leader><leader> za

        "Leader + various keys has the same effect as ':' + key + enter
        nnoremap <leader>w :w <CR>
        nnoremap <leader>q :q <CR>
        nnoremap <leader>e :e ~
        nnoremap <leader>v :vsplit ~
        nnoremap <leader>h :split ~

        "Ctrl + arrow keys resize splits
        nnoremap <C-up> 5<C-W>+
        nnoremap <C-down> 5<C-W>-
        nnoremap <C-left> 5<C-W><
        nnoremap <C-right> 5<C-W>>

        "A number n + Enter takes you to line n
        nnoremap <CR> G0

        "Shift + U redoes
        nnoremap U <C-R>

        "Swapping the behaviour of p and shift-p
        nnoremap P p
        nnoremap p P
    "}}}
"}}}

"FileType-Specific{{{
    augroup webfiles "Conveniences for working with web languages{{{
        au!
        autocmd BufEnter *.js,*.html :iabbrev dgei document.getElementById('')<Esc>F'ci'
        autocmd BufEnter *.js,*.html :iabbrev dgec document.getElementsByClassName('')<Esc>F'ci'
        autocmd BufEnter *.js,*.html :iabbrev dget document.getElementsByTagName('')t<Esc>F'ci'
        autocmd BufEnter *.js,*.html :iabbrev dgeiv document.getElementById('').value<Esc>F'ci'
        autocmd BufEnter *.js,*.html :iabbrev dgecv document.getElementsByClassName('').value<Esc>F'ci'
        autocmd BufEnter *.js,*.html :iabbrev dgetv document.getElementsByTagName('').value<Esc>F'ci'
    augroup end "}}}
    augroup vimrc "Conveniences for working with this very file, and others like it{{{
        au!
        autocmd BufEnter *.vimrc :set foldmethod=marker
    augroup end "}}}
    augroup textfiles "Conveniences for working with text files{{{
        au!
        autocmd BufEnter *.txt :setlocal spell spelllang=en_gb
    augroup end "}}}
    augroup cfiles "Conveniences for working with C files{{{
        au!
    augroup end "}}}
"}}}
