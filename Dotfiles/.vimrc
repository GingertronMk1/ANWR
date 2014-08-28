set nocompatible        "Removes vi compatibility.
"Indentation 	settings{{{1-----------------------------------------------------
    set autoindent          "Automatically indents lines to the same level as the previous line.
    set smartindent         "Inserts an extra indent in certain cases.
    set shiftround          "Rounds indentation to a multiple of <shiftwidth>.
    set expandtab           "Inserts <shiftwidth> spaces when tab is pressed.
    set smarttab            "Tabs in front of lines are <shiftwidth> spaces, and backspacing a tab deletes 4 spaces.
    set shiftwidth=4        "How many spaces constitute one tab.
    set tabstop=4           "How many spaces a tab counts for in a file.
"UI Changes{{{1---------------------------------------------------------------
    set background=dark     "Dark background.
    set number              "Puts line number in front of each line.
    set numberwidth=1       "Line numbers take less space.
    set incsearch           "Starts searching as soon as / is typed.
    set hlsearch            "Highlights searched things.
    set mouse=a             "Allows mouse control.
    syntax on               "Highlights ALL THE SYNTAX.
    filetype indent on      "Smarter indentation based on file type.
    set ruler               "Shows cursor all the time.
    set ignorecase          "All searches are case-insensitive. See below.
    set smartcase           "Lowercase searches aren't case-sensitive.
    set textwidth=0         "Eliminates textwidth restrictions.
    set wrap                "Text wraps at edge of window.
    set encoding=utf-8      "Standardizes text to UTF-8.
    set noswapfile          "No more swap file nonsense.
    set modeline            "Iunno.
    set scrolloff=5         "Margin of 5 lines around edge of screen.
"Abbreviations and Remappings{{{1---------------------------------------------
    iabbrev ldis ಠ_ಠ
    noremap <Space> za	
    noremap H 0
    noremap L $
    noremap J G
    noremap K 1G
    noremap vv vg_
    noremap ,s :%s///<left><left>
    set matchpairs=<:>,{:},(:),[:]
"Folding{{{1------------------------------------------------------------------
    set foldlevel=0
    set foldlevelstart=50
    set foldmethod=indent
