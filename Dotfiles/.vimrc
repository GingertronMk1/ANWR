" My garbled mess of a .vimrc
" Roughly, it goes:
" -> General Settings
" -> Wildmenu Settings
" -> External File Settings
" -> Indentation Settings
" -> UI Tweaks
" -> Moving Inside Buffers
" -> Working With Tabs And Splits
" -> Saving And Loading
" -> Other Shortcuts
" -> Filetype-Specific


"------------------------------------------------------------------------------
" General Settings
"------------------------------------------------------------------------------
set nocompatible        " Less vi-ey. Still vi-ey enough though
filetype on             " Allows filetype checking
filetype plugin on      " Probs does nowt for me but oh well
filetype indent on      " Smarter indentation based on file type
set history=500         " Lots of history
set showmatch           " Explicitly show matched paren pairs
" These are those matched pairs
set matchpairs=<:>,{:},(:),[:]
set mat=1               " How long to blink on typing the other paren
set modelines=0         " Security option
set encoding=utf-8      " Standardizes text to UTF-8
set backspace=indent,eol,start
set list                " Puts below characters in their places
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,trail:·
set showcmd             " Shows commands as I type them
set lazyredraw          " Redraw only when necessary
let mapleader="\<Space>"   " Sets leader key to spacebar
set magic               " Better regex searching. Also, NEVER BELIEVE IT'S NOT SOOO
set nostartofline       " Stop the cursor going to the start of each new line I go to
set confirm             " Ask to save changes rather than just not letting me do something
set incsearch           " Starts searching as soon as / is typed
set ignorecase          " All searches are case-insensitive
set smartcase           " Lowercase searches are case-insensitive

"------------------------------------------------------------------------------
" External File Settings
"------------------------------------------------------------------------------
set noswapfile          " No more swap file nonsense
set nobackup            " No more backup file nonsense
set nowritebackup       " Seriously, fuck off with the backups
set backupdir=~/.vim/backups  " Fuck the backup file off somewhere else for now
set viminfo+=n~/.vim/viminfo  " Fuck the viminfo file off somewhere else for now
if exists("&undodir")
  set undodir=~/.vim/undo
endif
"------------------------------------------------------------------------------
" Wildmenu Settings
"------------------------------------------------------------------------------
set wildmenu                    " Graphical menu of autocomplete options
set wildmode=list:longest,full
set wildignore=*.o,*.obj,*~     " stuff to ignore when tab completing
set wildignore+=*vim/backups*   " vim backup stuff
set wildignore+=*DS_Store*      " It's in every folder on a Mac and does sweet FA
set wildignore+=*.png,*.jpg,*.gif " You know, cos I don't want to edit pictures in Vim weirdly enough
set wildignore+=*.toc,*.log       " The shit that pdflatex pumps out

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
noremap == 1GvG=

"------------------------------------------------------------------------------
" UI Tweaks
"------------------------------------------------------------------------------
set number              " Puts line number in front of each line
set numberwidth=1       " Line numbers take less space
set hlsearch            " Highlights searched things
set mouse=a             " Allows mouse control
syntax on               " Highlights syntax. It's C21. Jesus.
set ruler               " Shows cursor all the time
set nowrap              " Text doesn't wrap at edge of window
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
set statusline+=WD:\ %{getcwd()}  " Current dir
set statusline+=%=                " Swap to the right for the next bit
set statusline+=L:\ %l\           " Current line
set statusline+=C:\ %c            " Current column

"------------------------------------------------------------------------------
" Moving Inside Buffers
"------------------------------------------------------------------------------
" 'H' takes you to the beginning of a line, and 'L' to the end
noremap H 0
noremap L $
" 'K' takes you to the top of the doc, and 'J' to the bottom
noremap J G$
noremap K 1G
" A number `n` + Enter takes you to line `n`
nnoremap <CR> G0
" Up and Down arrows now move between paragraphs
nnoremap <Up> {
nnoremap <Down> }
" Left and right arrows now move between words
nnoremap <Left> b
nnoremap <Right> w

"------------------------------------------------------------------------------
" Working With Tabs And Splits
"------------------------------------------------------------------------------
" <c-'key'> = <c-w> + 'key'
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" Tab navigation and creation
inoremap <C-t>l <Esc>:tabn<CR>
inoremap <C-t>h <Esc>:tabp<CR>
nnoremap <C-t>l :tabp<CR>
nnoremap <C-t>h :tabp<CR>
" Creating splits and tabs using leader key
nnoremap <leader>v :vsplit.<CR>
nnoremap <leader>h :split.<CR>
nnoremap <leader>t :tabnew.<CR>
" Ctrl + arrow keys resize vsplits
nnoremap <C-left> 5<C-w><
nnoremap <C-right> 5<C-w>>

"------------------------------------------------------------------------------
" Saving And Loading
"------------------------------------------------------------------------------
" Leader + various keys has the same effect as ':' + key + enter
nnoremap <leader>w :up<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>x :x<CR>
" Leader + shift + various keys does the same thing but forcefully
nnoremap <leader>Q :q!<CR>
nnoremap <leader>W :up!<CR>
" Leader-e puts you into the file tree to edit something else
nnoremap <leader>e :e.<CR>
" ww saves
nnoremap ww :up<CR>
" WW forces a save
nnoremap WW :up!<CR>
" Leader + 'r' reloads the document
nnoremap <leader>r :e<CR>

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
noremap <leader>m :nohl <CR>
" Shift + u redoes
nnoremap U <C-R>
" wc now displays a word count at the bottom
nnoremap wc g<C-g>
" <leader>d diffs this
nnoremap <leader>d :diffthis<CR>

"------------------------------------------------------------------------------
" Filetype-Specific
"------------------------------------------------------------------------------
autocmd FileType conf :set syntax=sh
augroup markdown
  au!
  autocmd BufEnter *.md :nnoremap <Leader>i ciw*<C-r>"*<Esc>
  autocmd BufEnter *.md :nnoremap <Leader>b ciw**<C-r>"**<Esc>
  autocmd BufEnter *.md :nnoremap <Leader>u ciw__<C-r>"__<Esc>
  autocmd BufEnter *.md :nnoremap <Leader>f ciw`<C-r>"`<Esc>
augroup latex
  au!
  autocmd BufEnter *.tex :nnoremap <Leader>i ciw\textit{<C-r>"}<Esc>
  autocmd BufEnter *.tex :nnoremap <Leader>b ciw\textbf{<C-r>"}<Esc>
  autocmd BufEnter *.tex :nnoremap <Leader>u ciw\underline{<C-r>"}<Esc>
  autocmd BufEnter *.tex :nnoremap <Leader>f ciw\verb\|<C-r>"\|<Esc>
  autocmd BufEnter *.tex :command Comp !pdflatex %
  autocmd BufEnter *.tex :setlocal textwidth=1024
