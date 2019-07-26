function! MoveCurrentFile()
  let old_file = expand('%')
  let new_file = input('New location: ', old_file, 'file')
  if (new_file == '' || new_file == old_file) | return | endif
  let alternate_buffer = @#
  if buflisted(new_file) | exec 'bd! ' . new_file | endif
  call system('mkdir -p $(dirname ' . shellescape(new_file) . ')')
  call system('mv ' . shellescape(old_file) . ' ' . shellescape(new_file))
  exec ':edit! ' . new_file
  exec 'bd! ' . old_file
  if bufexists(alternate_buffer) | let @# = alternate_buffer | endif
endfunction
command! Move :call MoveCurrentFile()<cr>

function! RenameCurrentFile()
  let old_name = expand('%:t')
  let new_name = input('New name: ', old_name, 'file')
  if (new_name == '' || new_name == old_name) | return | endif
  let dir = expand('%:h')
  let old_file = expand('%')
  let new_file = (dir == '.') ? (new_name) : (dir . '/' . new_name)
  if new_file == old_file | return | endif
  let alternate_buffer = @#
  if buflisted(new_file) | exec 'bd! ' . new_file | endif
  call system('mv ' . shellescape(old_file) . ' ' . shellescape(new_file))
  exec ':edit! ' . new_file
  exec 'bd! ' . old_file
  if bufexists(alternate_buffer) | let @# = alternate_buffer | endif
endfunction
command! Rename :call RenameCurrentFile()<cr>

function! DuplicateCurrentFile()
  let old_file = expand('%')
  let new_file = input('Duplicate as: ', old_file, 'file')
  if (new_file == '' || new_file == old_file) | return | endif
  if buflisted(new_file) | exec 'bd! ' . new_file | endif
  exec ':saveas! ' . new_file
endfunction
command! Duplicate :call DuplicateCurrentFile()<cr>

function! CopyCurrentFileAbsolutePath(...)
  let linenr = ''
  if a:0 | let linenr = ':' . line('.') | endif
  let @+ = expand('%:p') . linenr
endfunction
command! Filepath :call CopyCurrentFileAbsolutePath()

function! CreateNewFileInCurrentDir()
  let path = expand('%:h')
  if path == '.'
    let path = ''
  endif
  if path != ''
    let path .= '/'
  endif
  let new_file = input('New file: ', path, 'file')
  if new_file != '' && new_file != path
    exec ':e ' . new_file
    w
    call feedkeys(":file\<cr>")
  endif
endfunction
command! CreateNewFileInCurrentDir :call CreateNewFileInCurrentDir()
