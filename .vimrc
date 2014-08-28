set nocompatible        "Removes vi compatibility.
set autoindent          "Automatically indents lines to the same level as the previous line.
set smartindent         "Inserts an extra indent in certain cases.
set shiftwidth=4        "How many spaces constitute one tab .
set tabstop=4           "How many spaces a tab counts for in a file.
set expandtab           "Inserts <shiftwidth> spaces when tab is pressed.
set smarttab            "Tabs in front of lines are <shiftwidth> spaces, and backspacing a tab deletes 4 spaces.
set shiftround          "Rounds indentation to a multiple of <shiftwidth>.
set number              "Puts line number in front of each line.
set numberwidth=1       "Line numbers take less space.
set incsearch           "Starts searching as soon as / is typed.
set hlsearch            "Highlights searched things.
set mouse=a             "Allows mouse control.
syntax enable           "Highlights ALL THE SYNTAX
filetype indent on      "Smarter indentation based on file type.
set ruler               "Shows cursor all the time.
set ignorecase          "All searches are case-insensitive. See below.
set smartcase           "Lowercase searches aren't case-sensitive.
set textwidth=0         "Eliminates textwidth restrictions
set wrap                "Text wraps at edge of window
set encoding=utf-8      "Standardizes text to UTF-8
set noswapfile          "No more swap file nonsense
iabbrev ldis ಠ_ಠ
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
nnoremap <Space> za
"H goes to beginning of line, L to end
nnoremap H 0
nnoremap L $
set background=dark
set foldlevelstart=0
