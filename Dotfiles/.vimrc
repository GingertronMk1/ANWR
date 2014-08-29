"General Settings{{{--------------------------------------------------------
    set nocompatible        "Removes vi compatibility.
    "Characters form pairs.
    set matchpairs=<:>,{:},(:),[:]
"}}}
"Indentation settings{{{----------------------------------------------------
    set autoindent          "Automatically indents lines to the same level as the previous line.
    set smartindent         "Inserts an extra indent in certain cases.
    set shiftround          "Rounds indentation to a multiple of <shiftwidth>.
    set expandtab           "Inserts <shiftwidth> spaces when tab is pressed.
    set smarttab            "Tabs in front of lines are <shiftwidth> spaces, and backspacing a tab deletes 4 spaces.
    set shiftwidth=4        "How many spaces constitute one tab.
    set tabstop=4           "How many spaces a tab counts for in a file.
"}}}
"UI Changes{{{--------------------------------------------------------------
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
"}}}
"Abbreviations, Remappings{{{---------------------------
    "Abbreviations{{{-------------------------------------------------------
        "Abbreviates 'ಠ_ಠ' to the look of disapproval.
        iabbrev ldis ಠ_ಠ
    "}}}
    "Remappings{{{----------------------------------------------------------
        "Causes space to toggle a fold.
        noremap <Space> za
        "'H' takes you to the beginning of a line, and 'L' to the end.
        noremap H 0
        noremap L $
        "'K' takes you to the top of the doc, and 'J' to the bottom.
        noremap J G
        noremap K 1G
        "'vv' visually selects a line bar leading indentation.
        noremap vv vg_
        "',s' puts me in substitute mode.
        noremap ,s :%s///g<left><left><left>
        "'F1' opens help.
        noremap <F1> K
    "}}}--------------------------------------------------------------------
"}}}------------------------------------------------------------------------
"Folding{{{-----------------------------------------------------------------
    set foldlevel=0
    set foldlevelstart=5
    highlight Folded guibg=#000030 guifg=#FFFFFF
"}}}
"FileType-Specific{{{-------------------------------------------------------
    ".vimrc files get folded on {{{/}}}, others based on indentation
    augroup filetype_folding
        autocmd!
        autocmd FileType vim setlocal foldmethod=marker
        autocmd FileType !vim setlocal foldmethod=indent
    augroup END
    "In JS and HTML files, 'dgeid' expands to 'document.getElementById('').
    augroup web_abbreviations
        autocmd FileType javascript :iabbrev dgeid document.getElementById('')<left><left>
        autocmd FileType html  :iabbrev dgeid document.getElementById('')<left><left>
    augroup END
"}}}

