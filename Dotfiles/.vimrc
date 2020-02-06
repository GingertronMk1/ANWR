" My garbled mess of a .vimrc
" Roughly, it goes:
" -> General Settings
" -> External File Settings
" -> Wildmenu Settings
" -> Indentation Settings
" -> UI Tweaks
" -> Moving Inside Buffers
" -> Working With Tabs And Splits
" -> Saving And Loading
" -> Other Shortcuts
" -> Autogroups
" -> Skeletons

"------------------------------------------------------------------------------
" General Settings
"------------------------------------------------------------------------------
set nocompatible          " Less vi-ey. Still vi-ey enough though
filetype on               " Allows filetype checking
filetype indent on        " Smarter indentation based on file type
set history=500           " Lots of history
set showmatch             " Explicitly show matched paren pairs
set matchpairs=<:>        " Starting the matchpairs set with angle brackets
set matchpairs+={:}       " Adding braces
set matchpairs+=(:)       " Adding parentheses
set matchpairs+=[:]       " Adding brackets
set mat=1                 " How long to blink on typing the other paren
set modelines=0           " Security option
set encoding=utf-8        " Standardizes text to UTF-8
set backspace=indent,eol,start
set list                  " Puts below characters in their places
set listchars=tab:▸\ ,extends:❯,precedes:❮,trail:·
set showcmd               " Shows commands as I type them
set lazyredraw            " Redraw only when necessary
let mapleader="\<Space>"  " Sets leader key to spacebar
set magic                 " Better regex searching. Also, NEVER BELIEVE IT'S NOT SOOO
set nostartofline         " Stop the cursor going to the start of each new line I go to
set confirm               " Ask to save changes rather than just not letting me do something
set autoread              " Automatically update the file if it's changed by something else
set incsearch             " Starts searching as soon as / is typed
set ignorecase            " All searches are case-insensitive
set smartcase             " Lowercase searches are case-insensitive
set formatoptions=tcrqln  " How vim formats text
set spelllang=en          " Spell language

"------------------------------------------------------------------------------
" External File Settings
"------------------------------------------------------------------------------
set noswapfile                " No more swap file nonsense
set nobackup                  " No more backup file nonsense
set nowritebackup             " Seriously, fuck off with the backups
set backupdir=~/.vim/backups  " Fuck the backup file off somewhere else for now
set viminfo+=n~/.vim/viminfo  " Fuck the viminfo file off somewhere else for now
if exists("&undodir")
  set undodir=~/.vim/undo
endif
"------------------------------------------------------------------------------
" Wildmenu Settings
"------------------------------------------------------------------------------
set wildmenu                      " Graphical menu of autocomplete options
set wildmode=list:longest,full
set wildignore=*.o,*.obj,*~       " stuff to ignore when tab completing
set wildignore+=*vim/backups*     " vim backup stuff
set wildignore+=*DS_Store*        " It's in every folder on a Mac and does sweet FA
set wildignore+=*.png,*.jpg,*.gif " You know, cos I don't want to edit pictures in Vim weirdly enough
set wildignore+=*.aux*.toc,*.log  " The shit that pdflatex pumps out

"------------------------------------------------------------------------------
" Indentation Settings
"------------------------------------------------------------------------------
set autoindent          " Automatically indents lines
set smartindent         " Inserts an extra indent in certain cases
set shiftround          " Rounds indentation to a multiple of <shiftwidth>
set expandtab           " Inserts <shiftwidth> spaces when tab is pressed
set smarttab            " Tabs in front of lines are <shiftwidth> spaces
set shiftwidth=2        " 4 spaces constitute one tab
set tabstop=2           " How many spaces a tab counts for in a file
set softtabstop=2       " Backspace goes back 2 spaces in 'tab' chars
" Better indentation in visual mode
vnoremap > >gv
vnoremap < <gv
" '==' indents whole document
noremap == mi1GvG=`i

"------------------------------------------------------------------------------
" UI Tweaks
"------------------------------------------------------------------------------
set number              " Puts line number in front of each line
set numberwidth=1       " Line numbers take less space
set hlsearch            " Highlights searched things
set mouse=a             " Allows mouse control
syntax on               " Highlights syntax. It's C21. Jesus.
set ruler               " Shows cursor all the time
set cursorline          " Highlight the current line
set wrap                " Text will wrap at edge of window...
set linebreak           " But without linebreaks
set scrolloff=3         " Margin of 5 lines around edge of screen
set splitbelow          " Horizontal splits below current window
set splitright          " Vertical splits to the right of current window
set textwidth=9999      " Basically no columnal restriction
set t_Co=256            " Force 256 colours
try                     " colorscheme changing if possible
  colorscheme jack      " If my bastardised molokai's installed, use it
catch
  colorscheme elflord   " Otherwise, use elflord
endtry
set background=dark     " Light backgrounds look awful
try " font changing if possible
  set guifont=Menlo:h11
catch
  set guifont=Courier\ New:h11
endtry
set laststatus=2        " Shows status line at all times
set cmdheight=2
" The next bit is my statusline broken down into the individual components
set statusline=F:\ %F\            " Current file
set statusline+=%m\               " Modified flag
set statusline+=%=                " Swap to the right for the next bit
set statusline+=L:\ %l\           " Current line
set statusline+=C:\ %c            " Current column

"------------------------------------------------------------------------------
" Moving Inside Buffers
"------------------------------------------------------------------------------
" 'H' takes you to the beginning of a line, and 'L' to the end
noremap H g0
noremap L g$
" 'K' takes you to the top of the doc, and 'J' to the bottom
noremap J G$
noremap K 1G
" A number `n` + Enter takes you to line `n`
nnoremap <CR> G0
" Up and Down arrows now move between paragraphs
nnoremap <Up> {
vnoremap <Up> {
nnoremap <Down> }
vnoremap <Down> }
" Left and right arrows now move between words
nnoremap <Left> b
vnoremap <Left> b
nnoremap <Right> w
vnoremap <Right> w
" Up and Down arrows now move between paragraphs
inoremap <Up> <Esc>{i
inoremap <Down> <Esc>}i
" Left and right arrows now move between words
inoremap <Left> <Esc>bi
inoremap <Right> <Esc>lwi
" h and j work with wrapping
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

"------------------------------------------------------------------------------
" Working With Tabs And Splits
"------------------------------------------------------------------------------
" Creating splits and tabs using leader key
nnoremap <leader>v :vsplit.<CR>
nnoremap <leader>h :split.<CR>
nnoremap <leader>t :tabnew.<CR>

"------------------------------------------------------------------------------
" Saving And Loading
"------------------------------------------------------------------------------
" Leader + various keys has the same effect as ':' + key + enter
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>x :x<CR>
" Leader + shift + various keys does the same thing but forcefully
nnoremap <leader>Q :q!<CR>
nnoremap <leader>W :w!<CR>
" Leader-e puts you into the file tree to edit something else
nnoremap <leader>e :e.<CR>
" ww saves
nnoremap ww :w<CR>
" WW forces a save
nnoremap WW :w!<CR>
" Leader + 'r' reloads the document
nnoremap <leader>r :e<CR>
" wa saves all files
nnoremap wa :wa<CR>

"------------------------------------------------------------------------------
" Other Shortcuts
"------------------------------------------------------------------------------
" 'vv' visually selects a line
noremap vv 0v$
" 'V' selects to the end of the line, mirroring 'D', 'C', etc.
noremap V v$
" Leader + s puts me in find and replace mode
noremap <leader>s :%s///g<left><left><left>
" 'F1' opens help.
noremap <F1> K
" Swap : and ;
noremap : ;
noremap ; :
" Leader + m unhighlights search pattern
noremap <leader>m :nohl<CR>
" Shift + u redoes
nnoremap U <C-R>
" wc now displays a word count at the bottom
nnoremap wc g<C-g>
" <leader>d diffs this
nnoremap <leader>d :diffthis<CR>
" <leader>t trims trailing whitespace
nnoremap <leader>t mt:%s/\s\+$//e<Space>\|<Space>:nohl<CR>`t:delmarks t<CR>:<CR>
" <leader>g adds and commits the file to git
nnoremap <leader>g :!git add % && git commit -m ""<left>
" <leader>p does the last thing and also sets it up to push
nnoremap <leader>p :!git add % && git commit -m "" && git push<left><left><left><left><left><left><left><left><left><left><left><left><left>
" <leader>; adds a semicolon to the end of a line
nnoremap <leader>; A;<Esc>
" <leader>l lesses the file
nnoremap <leader>l !less %

" Auto-close brackets and speech marks
inoremap (  ()<left>
inoremap {  {}<left>
inoremap [  []<left>
inoremap <  <><left>
inoremap '  ''<left>
inoremap "  ""<left>

"------------------------------------------------------------------------------
" Folding
"------------------------------------------------------------------------------

" Fold on indentation
set foldmethod=indent

" Don't fold anything on file load
set foldlevel=1000

" <leader><leader> toggles folds
nnoremap <leader><leader> za

nnoremap <C-w>> 10<C-w>>
nnoremap <C-w>< 10<C-w><
nnoremap <C-w>+ 10<C-w>+
nnoremap <C-w>- 10<C-w>-

"------------------------------------------------------------------------------
" netrw
"------------------------------------------------------------------------------

let g:netrw_banner = 0
let g:netrw_liststyle = 3

"------------------------------------------------------------------------------
" Autogroups
"------------------------------------------------------------------------------
augroup Textfiles "I need that spell check
  au!
  autocmd BufEnter *.tex setlocal spell
  autocmd BufEnter *.md  setlocal spell
  autocmd BufEnter *.txt setlocal spell
augroup END

augroup Oddfiles " Highlighting weird files
  au!
  autocmd BufEnter .aliases      setlocal syntax=sh
  autocmd BufEnter .bash_profile setlocal syntax=sh
  autocmd BufEnter .bashrc       setlocal syntax=sh
  autocmd BufEnter .exports      setlocal syntax=sh
  autocmd BufEnter .functions    setlocal syntax=sh
  autocmd BufEnter .prompt       setlocal syntax=sh
  autocmd BufEnter *.twig        setlocal syntax=html
  autocmd BufEnter .zshrc        setlocal syntax=sh
  autocmd BufEnter *.twig        setlocal syntax=html
augroup END

augroup Commentary " <leader>c should comment a line
  au!
  autocmd BufEnter *.html   nnoremap <leader>c mcI<!--<Esc>A--><Esc>`c:delmarks c<CR>:<CR>
  autocmd BufEnter *.php    nnoremap <leader>c mcI<!--<Esc>A--><Esc>`c:delmarks c<CR>:<CR>
  autocmd BufEnter *.css    nnoremap <leader>c mcI/*<Esc>A*/<Esc>   `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.scss   nnoremap <leader>c mcI//<Esc>           `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.c      nnoremap <leader>c mcI//<Esc>           `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.cpp    nnoremap <leader>c mcI//<Esc>           `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.hs     nnoremap <leader>c mcI--<Esc>           `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.lhs    nnoremap <leader>c mcg0a--<Esc>         `c:delmarks c<CR>:<CR>
  autocmd BufEnter *.vimrc  nnoremap <leader>c mcI"<Esc>            `c:delmarks c<CR>:<CR>
augroup END

augroup FileSpecificExpansions
  au!
  " <? will autocomplete to a full php statement in .php files"
  autocmd BufEnter *.php        inoremap <buffer> <? <?php ; ?><left><left><left><left>
  autocmd BufEnter *.html.twig  inoremap <buffer> {% {%  %}<left><left><left>
  autocmd BufEnter *.html.twig  inoremap <buffer> {{ {{  }}<left><left><left>
augroup END

"------------------------------------------------------------------------------
" Skeletons
"------------------------------------------------------------------------------
augroup Skel
  au!
  :autocmd BufNewFile *.c                           0r ~/.vim/skeleton.c
  :autocmd BufNewFile *.h                           0r ~/.vim/skeleton.h
  :autocmd BufNewFile *.hs                          0r ~/.vim/skeleton.hs
  :autocmd BufNewFile *.lhs                         0r ~/.vim/skeleton.lhs
  :autocmd BufNewFile *.tex                         0r ~/.vim/skeleton.tex
  :autocmd BufNewFile *.html                        0r ~/.vim/skeleton.html
  :autocmd BufNewFile *.php                         0r ~/.vim/skeleton.html
  :autocmd BufNewFile */history-project/_shows/*.md 0r ~/.vim/_skeleton.md
augroup END
