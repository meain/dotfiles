" function! LinterStatus() abort
"   try
"     let l:counts = ale#statusline#Count(bufnr(''))
"     let l:all_errors = l:counts.error + l:counts.style_error
"     let l:all_non_errors = l:counts.total - l:all_errors
"
"     if l:counts.total == 0
"       let l:diagnosticsDict = LanguageClient#statusLineDiagnosticsCounts()
"       let l:errors = get(l:diagnosticsDict,'E',0)
"       let l:warnings = get(l:diagnosticsDict,'W',0)
"       return l:errors + l:warnings == 0 ? '' : printf(
"           \   '%dW %dE',
"           \   l:warnings,
"           \   l:errors
"           \)
"     endif
"
"     return l:counts.total == 0 ? '' : printf(
"           \   '%dW %dE',
"           \   all_non_errors,
"           \   all_errors
"           \)
"   catch
"     return ''
"   endtry
" endfunction

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

function! TreesitterStatus() abort
  if get(g:, 'enable_treesitter_status', 0) ==# 0
    return ' '
  endif
  let ts = nvim_treesitter#statusline(30)
  if ts ==# 'null'
    return ''
  else
    return ts
  endif
endfunction

function! LSPStatus() abort
  let l:counts = luaeval("require'lsp-status'.diagnostics()")
  if len(l:counts) ==# 0
    return printf('-')
  endif
  let l:errors = l:counts.errors
  let l:warnings = l:counts.warnings
  " let l:hints = l:counts.hints
  " let l:info = l:counts.info

  return l:errors + l:warnings == 0 ? '!' : printf(
      \   '%dW %dE',
      \   l:warnings,
      \   l:errors
      \)
endfunction

set statusline=                                           " Reset status line
set statusline+=%*                                        " Reset color
set statusline+=%{&readonly?':':!&modifiable?':':''}      " Non modifiable
set statusline+=\ \ %{TinyFilePath()}\                    " File name
set statusline+=%{&modified?'+':''}                       " Modified
set statusline+=\ %q                                      " Quickfix, LocList etc
set statusline+=%#Comment#                                " Faded
set statusline+=\ %{TreesitterStatus()}                   " Treesitter path
set statusline+=%#Normal#                                 " Normal
" set statusline+=\ %{NearestMethodOrFunction()}            " Vista method
set statusline+=%=                                        " Split
set statusline+=%#ALEWarningSign#                         " Warning color
" set statusline+=QF:%{len(getqflist())}\                   " Number of items in qflist
" set statusline+=%{LinterStatus()}                         " ALE errors and warns
set statusline+=%{LSPStatus()}
set statusline+=%#Normal#                                 " Faded
set statusline+=\ %l:%c                                   " Line number and column
set statusline+=\ %P                                      " Percentage of file
set statusline+=\ %L                                      " Total number of lines
set statusline+=\ %y                                      " Filetype
set statusline+=\                                         " Blank
