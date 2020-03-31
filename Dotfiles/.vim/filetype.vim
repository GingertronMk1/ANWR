" my filetype file
if exists("did_load_filetypes")
    finish
endif
augroup filetypedetect
    au! BufEnter *.vue       setfiletype html
    au! BufEnter *.blade.php setfiletype php
    au! BufEnter *.html.twig setfiletype html
augroup END
