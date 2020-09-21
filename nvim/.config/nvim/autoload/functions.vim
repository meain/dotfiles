" Toggle quickfix
function! functions#QuickfixToggle()
    let nr = winnr('$')
    cwindow
    let nr2 = winnr('$')
    if nr == nr2
        cclose
    endif
endfunction

" Zoom in and out of windows
function! functions#ZoomToggle() abort
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    else
        let t:zoom_winrestcmd = winrestcmd()
        resize
        vertical resize
        let t:zoomed = 1
    endif
endfunction

" Save session and quit all buffers (for use with viml command)
function! functions#MinimizeIfZoomed()
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    endif
endfunction

" Strip trailing whitespaces
function! functions#StripTrailingWhitespace()
    if !&binary && &filetype !=# 'diff'
      normal! mz
      %s/\s\+$//e
      normal! `z
    endif
endfunction

" Better marks
function! functions#Marks()
    marks abcdefghijklmnopqrstuvwxyz.
    echo 'Jump to mark: '
    let mark=nr2char(getchar())
    redraw
    execute 'normal! `'.mark
endfunction

" Githuv link copy
function! functions#GHOpen(open) abort
    let l:git_origin = system("git config --get remote.origin.url|sed 's|git@github.com:|https://github.com/|;s|.git$||'")
    let l:git_branch = system('git rev-parse --abbrev-ref HEAD')
    let l:current_file = expand('%')
    let l:line_start = getpos("'<")[1]
    let l:line_end = getpos("'>")[1]
    let l:current_line = line('.')
    if l:line_start != 0
      let l:url = trim(l:git_origin) . '/blob/' . trim(l:git_branch) . '/' . trim(l:current_file) . '#L' . trim(l:line_start) . '-#L' . trim(l:line_end)
    else
      let l:url = trim(l:git_origin) . '/blob/' . trim(l:git_branch) . '/' . trim(l:current_file) . '#L' . trim(l:current_line)
    endif
    echo l:url
    " We can open this thing in firefox if `open` just did not replace # with %23
    call system("echo '". l:url . "' | pbcopy")
    if a:open
      call system("open '". l:url . "'")
    endif
endfunction

" Create a floating buffer
function! functions#Floater(...)
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')
  if a:0 >= 4
    let height = a:1
    let width = a:2
    let horizontal = a:3
    let vertical = a:4
  elseif a:0 == 2
    let height = a:1
    let width = a:2
    let horizontal = float2nr((&columns - width) / 2)
    let vertical = 0
  else
    let height = float2nr(10)
    let width = float2nr(80)
    let horizontal = float2nr((&columns - width) / 2)
    let vertical = 0
  endif

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'anchor': 'NW',
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
  if a:0 == 5
    execute 'set winblend='.a:5
  endif
endfunction

" Auto add note header
function! functions#AddNoteHeader()
  let l:note_title = $NOTE_TITLE
  if len(l:note_title) == 0
    let l:note_title = substitute(substitute(trim(expand('%:t')), '.md$', '', ''), '_', ' ', 'g')
  endif
  call setline(1, '---')
  call setline(2, 'title: '.l:note_title)
  call setline(3, 'created: '.trim(system('date')))
  call setline(4, '---')
  call setline(5, '')
  call setline(6, '<++>')
  normal! ggjf:llv$
endfunction

" Close unused buffers
function! functions#CloseHiddenBuffers()
  let open_buffers = []

  for i in range(tabpagenr('$'))
    call extend(open_buffers, tabpagebuflist(i + 1))
  endfor

  for num in range(1, bufnr('$') + 1)
    if buflisted(num) && index(open_buffers, num) == -1
      exec 'bdelete '.num
    endif
  endfor
endfunction

" Find highlight group of char under the cursor
function! functions#ShowHightlightGroup()
  if !exists('*synstack')
    return
  endif
  let name = synIDattr(synID(line('.'),col('.'),1),'name')
  let trans = synIDattr(synID(line('.'),col('.'),0),'name')
  let lo = synIDattr(synIDtrans(synID(line('.'),col('.'),1)),'name')
  echo 'hi[' . name . '] trans[' . trans .'] lo[' . lo . ']'
endfunc

" Remove space after bracket on joining
function! functions#Join()
  let previous_last_char = getline('.')[col('$')-2]
  normal! J
  let current_char = getline('.')[col('.')-1]
  if previous_last_char =~# '(\|[\' && current_char ==# ' '
    normal! x
  endif
endfunction

" For use with vv
function! functions#OpenLastFile()
  let last = filter(copy(v:oldfiles), 'filereadable(v:val)')
  if !empty(last)
    execute 'edit' fnameescape(last[0])
  endif
endfunction

" Search for css defenittion
function! functions#CSSSearchForClassDef()
  setlocal iskeyword+=-
  let word = expand('<cword>')
  setlocal iskeyword-=-
  execute ':Find .' . word
endfunction

" Json format
function! functions#JSONFormat() range
  execute '%!fixmalformedjson | jq'
  set filetype=json
endfunction

" Startpage
function! functions#StartPage(force)
  if !(argc() == 0 && &filetype ==# '' && line2byte('$') == -1) && !a:force
    return
  endif
  let l:oldfiles = v:oldfiles
  let g:cur_dir = getcwd()
  let g:cur_dir_len = len(getcwd()) + 1
  function! Ffn(idx, val)
    return a:val =~# g:cur_dir && filereadable(a:val) && !(a:val =~# '\.git')
  endfunction
  function! NameCleanUp(idx, val)
    return a:val[g:cur_dir_len:]
  endfunction
  function! FileOpen()
    let l:filename = split(getline('.'), ' ')
    if len(l:filename) == 1
      if l:filename[0] ==# '__'
          FZF
      elseif l:filename[0] ==# '##'
          Dirvish
      else
        silent exec 'e '. l:filename[0]
      endif
    endif
  endfunction

  let l:filterd_files = filter(l:oldfiles, function('Ffn'))
  let l:cleaned_files = map(l:filterd_files, function('NameCleanUp'))
  enew
  setlocal
      \ bufhidden=wipe
      \ buftype=nofile
      \ nobuflisted
      \ nocursorcolumn
      \ cursorline
      \ nolist
      \ nonumber
      \ noswapfile
      \ norelativenumber
  call append('^', '##')
  call append('^', l:oldfiles[:10])
  call append('^', '__')
  normal! Gddgg
  nnoremap <buffer><silent> e :enew<CR>
  nnoremap <buffer><silent> i :enew <bar> startinsert<CR>
  nnoremap <buffer><silent> o :enew <bar> startinsert<CR>
  nnoremap <buffer><silent> v :OpenLastFile<CR>
  nnoremap <buffer><silent> <enter> :call FileOpen()<CR>
  setlocal
    \ nomodifiable
    \ nomodified
  try
    IndentLinesDisable
  endtry
endfunction

" Markdown preview
function! functions#MarkdownPreview()
  call jobstart('pandocmarkdownpreview '.expand('%'))
  echo 'Generating preview...'
endfunction

" Copy all matching string
function! functions#CopyMatches(reg)
  let hits = []
  %s//\=len(add(hits, submatch(0))) ? submatch(0) : ''/gne
  let reg = empty(a:reg) ? '+' : a:reg
  execute 'let @'.reg.' = join(hits, "\n") . "\n"'
endfunction
