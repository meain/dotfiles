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

function TinyFilePath()
  let l:path = expand('%')

  if l:path ==? ''
    return '[SPECIAL]'
  endif

  let l:splits = split(l:path, '/')
  let l:no_of_splits = len(l:splits)
  if l:no_of_splits > 1
    let l:folder = l:splits[l:no_of_splits - 2]
    let l:file = l:splits[l:no_of_splits - 1]
    return l:folder . '/' . l:file
  else
    return l:path
  endif
endfunction

function! NearestMethodOrFunction() abort
  return get(b:, 'vista_nearest_method_or_function', '')
endfunction

set statusline=                                        " Reset status line
set statusline+=%*                                     " Reset color
set statusline+=%{&readonly?':':!&modifiable?':':''}   " Non modifiable
set statusline+=\ \ %{TinyFilePath()}\                 " File name
set statusline+=%{&modified?'+':''}                    " Modified
set statusline+=\ %q                                   " Quickfix, LocList etc
set statusline+=%#Normal#                        " Faded
set statusline+=\ %{NearestMethodOrFunction()}
set statusline+=%=                                     " Split
set statusline+=%#ALEWarningSign#                      " Warning color
set statusline+=%{LinterStatus()}                      " ALE errors and warns
set statusline+=%#Normal#                        " Faded
set statusline+=\ %l:%c                                " Line number and column
set statusline+=\ %y                                   " Filetype
set statusline+=\                                      " Blank

