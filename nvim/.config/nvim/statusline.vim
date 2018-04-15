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

set statusline=                                        " Reset status line
set statusline+=%#StatusLineNC#                        " Faded
set statusline+=\=\                                    " Opening
set statusline+=%{&readonly?':':!&modifiable?':':''}   " Non modifiable
set statusline+=%*                                     " Reset color
set statusline+=%t                                     " File name
set statusline+=%#StatusLineNC#                        " Faded
set statusline+=%{&modified?'+':''}                    " Modified
set statusline+=\ %q                                   " Quickfix, LocList etc
set statusline+=%=                                     " Split
set statusline+=%#ALEWarningSign#                      " Warning color
set statusline+=%{LinterStatus()}                      " ALE errors and warns
set statusline+=%#StatusLineNC#                        " Faded
set statusline+=\ %l:%c                                " Line number and column
set statusline+=\ %p%%\ \=                             " Percentage

