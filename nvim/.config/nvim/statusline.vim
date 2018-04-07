function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? '' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

set statusline=
set statusline+=\ %t
set statusline+=%#LineNr#
set statusline+=%m
set statusline+=\ %q
set statusline+=%=
set statusline+=%#ALEWarningSign#
set statusline+=%{LinterStatus()}
set statusline+=%#LineNr#
set statusline+=\ %l:%c
set statusline+=\ %p%%
set statusline+=\ 

