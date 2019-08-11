function! OpenCurrentFileInGithub(...)
  let local_branch = a:0
  let file_dir = expand('%:h')
  let git_root = system('cd ' . file_dir . '; git rev-parse --show-toplevel | tr -d "\n"')

  let file_path = substitute(expand('%:p'), git_root . '/', '', '')
  let branch = 'master'
  if local_branch | let branch = system('git symbolic-ref --short -q HEAD | tr -d "\n"') | endif
  let git_remote = system('cd ' . file_dir . '; git remote get-url origin | tr -d "\n"')

  let url = git_remote . '/blob/' . branch . '/' . file_path

  let first_line = getpos("'<")[1]
  let url .= '#L' . first_line
  let last_line = getpos("'>")[1]
  if last_line != first_line | let url .= '-L' . last_line | endif
  call system('echo ' . url . ' | pbcopy')
  call system('open ' . url)
endfunction

command! OpenCurrentFileInGithub :echom "<leader>og in visual mode"<cr>

nnoremap <silent> <leader>og :echom "Github link only in visual mode"<cr>
vnoremap <silent> <leader>og :<c-u>call OpenCurrentFileInGithub()<cr>
