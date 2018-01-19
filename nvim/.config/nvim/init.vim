"                            Plugin Management                         "
"                    ==============================                    "

call plug#begin('~/.vim/plugged')

" Visual enhancements
Plug 'kshenoy/vim-signature'                                                                  " Show marks
Plug 'itchyny/lightline.vim'                                                                  " Statusline plugin
Plug 'flazz/vim-colorschemes'                                                                 " Vim colorscheme
Plug 'Yggdroot/indentLine'                                                                    " Show indent
Plug 'mhinz/vim-signify'                                                                      " Git diff icons in gutter
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }                                          " Hyper focus editing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }                                                    " Centerify
Plug 'easymotion/vim-easymotion'

" Added functinality
Plug 'mhinz/vim-startify'                                                                     " A fancy start page for vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }                             " Fzf
Plug 'junegunn/fzf.vim'                                                                       " Fzf for vim
Plug 'christoomey/vim-tmux-navigator'                                                         " Seamless navigation between vim and tmux
Plug 'suan/vim-instant-markdown'                                                              " View markdown in browser while editing
Plug 'ervandew/supertab'                                                                      " Autocomplete on tab
Plug 'zirrostig/vim-schlepp'                                                                  " Better drag visuals
Plug 'vim-scripts/restore_view.vim'                                                           " Restore file pointer
Plug 'jsfaint/gen_tags.vim'                                                                   " Better tags usage
Plug 'tpope/vim-dispatch'                                                                     " Async tasks in vim
Plug 'airblade/vim-rooter'                                                                    " Change directory to project root
Plug 'editorconfig/editorconfig-vim'                                                          " Editorconfig
Plug 'mattn/gist-vim', { 'on': 'Gist' }                                                       " Push current buffer as gist
Plug 'rizzatti/dash.vim', { 'on': 'Dash' }                                                    " Search Dash docs
Plug 'davidbeckingsale/writegood.vim', { 'on': ['WritegoodEnable', 'WritegoodToggle'] }       " Better writing mode
Plug 'mattn/emmet-vim'                                                                        " Emmet
Plug 'alvan/vim-closetag'                                                                     " Automatically add closing tag
Plug 'jreybert/vimagit', { 'on': 'Magit' }                                                    " Even better git interface for vim
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less',
  \ 'scss', 'json', 'graphql', 'markdown'] }

" Code editing enhacements
Plug 'tpope/vim-sleuth'                                                                       " Automatic indentation setting
Plug 'tpope/vim-vinegar'                                                                      " Better netrw
Plug 'tpope/vim-fugitive'                                                                     " Git stuff from within vim
Plug 'majutsushi/tagbar', { 'on' : 'Tagbar' }                                                 " Class/module browser
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }                                            " Undo tree
Plug 'scrooloose/nerdtree', { 'on': ['NERDTree', 'NERDTreeToggle'] }                          " Nerdtree

" Morph code
Plug 'tpope/vim-surround'                                                                     " Surround
Plug 'jiangmiao/auto-pairs'                                                                   " Autopair
Plug 'tomtom/tcomment_vim'                                                                    " Code commenting
Plug 'junegunn/vim-easy-align'                                                                " Some prettification

" Language helpers
Plug 'sheerun/vim-polyglot'                                                                   " Multiple language support
Plug 'othree/yajs.vim', { 'for': ['javascript', 'javascript.jsx', 'typescript'] }             " JS support
Plug 'leafgarland/typescript-vim', { 'for': ['javascript', 'javascript.jsx', 'typescript'] }  " Typescript supprt
Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx', 'typescript'] }    " Inproved JSX syntax
Plug 'tpope/vim-markdown', { 'for': ['md', 'markdown'] }                                      " Better markdown support
Plug 'davidhalter/jedi-vim', { 'for': ['python'] }                                            " Python helper
Plug 'fatih/vim-go', { 'for': ['go'] }                                                        " Golang helper
Plug 'sebdah/vim-delve', { 'for': 'go' }                                                      " Debugger for go
Plug 'racer-rust/vim-racer', { 'for': 'rust' }                                                " Rust support
Plug 'mhartington/nvim-typescript', { 'for': ['javascript', 'javascript.jsx', 'typescript'] } " Typescript completion
Plug 'tmhedberg/matchit', { 'for': ['html','xml', 'tex'] }                                    " Match tags for html, xml latex etc
Plug 'raimon49/requirements.txt.vim', { 'for': 'requirements' }                               " Requirements file
Plug 'bpietravalle/vim-bolt' , { 'for': ['bolt'] }                                            " Bolt file syntax (firebase)

" Language enhacements
Plug '~/Documents/Projects/vim-jsontogo' , { 'for': ['go'] }                                  " Convert JSON to Go struct

" Linting / Checking
Plug 'w0rp/ale'                                                                               " Neomake - linting and stuf
Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' }                                         " Beautify code

" Autocomplete
Plug 'roxma/nvim-completion-manager'                                                          " Better autocompletion
Plug 'calebeby/ncm-css', { 'for': 'css' }                                                     " CSS Completion
Plug 'roxma/nvim-cm-tern',  {'do': 'npm install'}                                             " Completion for js
Plug 'Shougo/neco-vim', { 'for': 'vim' }                                                      " Vimscript autocomplete
Plug 'roxma/ncm-github'                                                                       " Github completion
Plug 'roxma/nvim-cm-racer', { 'for': ['rs', 'rust'] }                                         " Rust autocompletion
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }                       " nvim language client (nvim-completion-manager)

" Snippets
Plug 'SirVer/ultisnips'                                                                       " Snippet manager
Plug 'honza/vim-snippets'                                                                     " More snippets
Plug 'epilande/vim-es2015-snippets', { 'for': ['javascript', 'typescript'] }                  " ES2015 snippets
Plug 'epilande/vim-react-snippets', { 'for': ['javascript', 'typescript', 'javascript.jsx'] } " React snippets

" Dependencies
Plug 'vim-scripts/mru.vim'                                                                    " Save recently used files (v)
Plug 'radenling/vim-dispatch-neovim'                                                          " Neovim support for vim-dispatch

" Others
Plug 'wakatime/vim-wakatime'                                                                  " Wakatime
Plug 'junegunn/vim-github-dashboard', { 'on': ['GHA', 'GHD']}                                 " Github dashboard

call plug#end()





"                           Editor Settings                            "
"                    ==============================                    "


" Set encoding to utf8
set encoding=utf8

" Make autocomplete for filenames work
set path+=**

" Turn on line numbers
set number

" Highlight cursor line (slows down)
set nocursorline

" Hidden startup messages
set shortmess=atI

" Auto read and write
set autowrite
set autoread

" Confirm before quit without save
set confirm

" Disable wrapping
set nowrap

" Set hidden
set hidden

" Better backup, swap and undos storage
set directory=~/.vim/dirs/tmp     " directory to place swap files in
set backupdir=~/.vim/dirs/backups " where to put backup files
set backup                        " make backup files
set undodir=~/.vim/dirs/undodir   " undo directory
set undofile                      " persistent undos - undo after you re-open the file

" Allow mouse
set mouse=a

" Incremental search
set incsearch

" Incremental substitution
set inccommand=split

" Highlighted search results
set hlsearch

" Smart search
set ignorecase

" Allow plugins by file type (required for plugins!)
filetype plugin on
filetype indent on

" Redraw only when essential
set lazyredraw

" Just sync some lines of a large file
set synmaxcol=200
syntax sync minlines=256

" Make backspace great again
set backspace=2

" Set split direction
set splitbelow
set splitright

" Default intent to 4 spaces ( auto switch based on type in code section )
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Don't show --INSERT-- at bottom
set noshowmode

" Show invisibles
set list
set listchars=tab:\ \ ,eol:¬,trail:⋅
autocmd ColorScheme * highlight NonText ctermfg=238 ctermbg=235

" Show line breaks
set showbreak=↪

" When scrolling, keep cursor 5 lines away from screen border
set scrolloff=5

" Autocompletion of files and commands behaves like zsh
set wildmenu
set wildmode=full

" Setting up ignores
set wildignore+=*/tmp/*,*.so,*.pyc,*.png,*.jpg,*.gif,*.jpeg,*.ico,*.pdf
set wildignore+=*.wav,*.mp4,*.mp3
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*
set wildignore+=*.swp,*~,._*
set wildignore+=_pycache_,.DS_Store,.vscode,.localized

" Indent based folding
set foldmethod=indent
set foldignore=
set foldlevelstart=10
set foldnestmax=10

" Setting colorscheme
set background=dark
autocmd ColorScheme janah highlight Normal ctermbg=235
colorscheme janah

" Use italics for some text
hi htmlArg gui=italic
hi Comment gui=italic
hi Type    gui=italic
hi htmlArg cterm=italic
hi Comment cterm=italic
hi Type    cterm=italic

" Set up leader keys
let mapleader = "\<Space>"
let maplocalleader = "\|"

" Smart colorcolumn
highlight ColorColumn ctermbg=154 ctermfg=0 guibg=#474747
autocmd BufEnter * call matchadd('ColorColumn', '\%160v', 100)

" Better coloring for errors
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=11 guifg=#ffff00

" Don't you f'in touch my cursor
set guicursor=

" Completefunc
autocmd BufEnter * set completefunc=cm#_completefunc

" Oh-my-zsh themes are shell.
autocmd BufNewFile,BufRead *.zsh-theme set filetype=sh

" Resize panes whenever containing window resized.
autocmd VimResized * wincmd =

" Make sure it is javascript
augroup filetype javascript syntax=javascript





"                                Commands                              "
"                    ==============================                    "

" Save as sudo
ca w!! w !sudo tee "%"

" I am too lazy to take my hands from shift
command! WQ wq
command! Wq wq
command! W w

" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.cpp,*.c,*.java,*.go,*.rs,*.ts,*.cljs,*.clj :%s/\s\+$//e

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap





"                               Key remaps                             "
"                    ==============================                    "


" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

"Get back to where you were easily
nnoremap gg mpgg
nnoremap G mpG
nnoremap / mp/

" Quick command mode (second binding to get find next)
nnoremap ; :
nnoremap ' ;

" Use the clipboard for copy and paste
nnoremap y "+y
nnoremap Y "+y$
nnoremap p "+p`]
nnoremap P "+P`]
vnoremap y "+y
vnoremap Y "+Y
vnoremap p "+p
vnoremap P "+P

" Easier indentation - does dot loose selection
vnoremap > >gv
vnoremap < <gv

" Navigaiion
nnoremap <Down> V10<c-y><esc>
nnoremap <Up> V10<c-e><esc>

" Easy buffer switching
nnoremap <silent><Leader><Tab> :enew\|:Startify<cr>

" Terminal split jump
tnoremap <m-h> <C-\><C-N><C-w>h
tnoremap <m-j> <C-\><C-N><C-w>j
tnoremap <m-k> <C-\><C-N><C-w>k
tnoremap <m-l> <C-\><C-N><C-w>l

" Terminal mode esc remap
tnoremap <Leader><Esc> <C-\><C-n>

" Copy entire file content
nnoremap yp mzggVG"+y`z

" Quick suspend
nnoremap <silent><M-Enter> <c-z>

" Vim docs
au FileType vim nmap K :help <c-r><c-w><cr>

" Don't change Enter in all buffers
autocmd FileType help nnoremap <buffer> <Enter> <Enter>
autocmd FileType vim-plug nnoremap <buffer> <Enter> <Enter>





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <silent><leader><leader> :w<cr>
nnoremap <silent><Leader>q :bd<cr>
nnoremap <silent><Leader>w :w<cr>

" Split like a boss
nnoremap <silent><Leader>v :vsplit\|:Startify<cr>
nnoremap <silent><Leader>h :sp\|:Startify<cr>

" Clear search highlight
nnoremap <silent><Leader>/ :nohls<CR>

" Easy tag navigation
nnoremap <silent><Leader>; <C-]>
nnoremap <silent><Leader>' <C-o>

" Quick fold and unfold
nnoremap <silent><Leader><esc> :normal!za<cr>

" Quick switch tabs
nnoremap <silent><Leader>m :tabn<cr>
nnoremap <silent><Leader>n :tabp<cr>

" Only current buffer
nnoremap <silent><leader>o :only<cr>





"                                Code                                  "
"                    ==============================                    "


" Spell checking
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.org setlocal spell
autocmd BufRead,BufNewFile *.txt setlocal spell
autocmd FileType gitcommit setlocal spell
autocmd FileType help setlocal nospell
set complete+=kspell
function! FixLastSpellingError()
    normal! mz[s1z=`z
endfunction
nnoremap Z :call FixLastSpellingError()<cr>
inoremap <c-z> :call FixLastSpellingError()<cr>

" Get output of shell command in vim window
function! SplitRunCommand()
    call inputsave()
    let l:cmd = input('Command: ')
    new
    setlocal buftype=nofile
    call append(0, systemlist(l:cmd))
    call inputrestore()
endfunction
nnoremap ,s :call SplitRunCommand()<cr>

" Google search from within vim
function! GoogleSearch(arg)
py << EOF
import vim
def frame_querry():
    term = vim.eval("a:arg")
    vim.command("vsp")
    vim.command("term googler --count 5 " + term)
frame_querry()
EOF
endfunction
command! -nargs=1 Google call GoogleSearch(<f-args>)
nnoremap <silent><leader>s :Google <c-r><c-w><cr>
vnoremap <leader>s y:Google <c-r>"<cr>

" Toggle quickfix
function! QuickfixToggle()
    let nr = winnr("$")
    cwindow
    let nr2 = winnr("$")
    if nr == nr2
        cclose
    endif
endfunction
nnoremap <silent> <leader>i :call QuickfixToggle()<cr>

" Zoom in and out of windows
function! s:ZoomToggle() abort
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
command! ZoomToggle call s:ZoomToggle()
nnoremap <silent> _ :ZoomToggle<CR> \| :normal! 0<cr>

" Save session and quit all buffers (for use with viml command)
function! MinimizeIfZoomed()
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    endif
endfunction
nnoremap <c-q> :call MinimizeIfZoomed() \|:SSave zzz \| :qa<cr>y

" Strip trailing whitespaces
function! StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
      normal mz
      %s/\s\+$//e
      normal `z
    endif
endfunction
command! StripTrailingWhitespace :call StripTrailingWhitespace()

" Better marks
function! Marks()
    marks abcdefghijklmnopqrstuvwxyz.
    echo 'Jump to mark: '
    let mark=nr2char(getchar())
    redraw
    execute 'normal! `'.mark
endfunction
command! Marks call Marks()
nnoremap <silent>`` :call Marks()<cr>

" Json format
function! JSONFormat() range
  execute "%!python -m json.tool"
endfunction
command! -range JSONFormat <line1>,<line2>call JSONFormat()

" Scratch buffer
function! ScratchOpen()
  execute "topleft new __scratch__"
  if filereadable("/tmp/.scratchfile")
    execute " %! cat /tmp/.scratchfile"
  endif
  redraw
  execute "resize 15"
  execute "set ft=scratch"
  setlocal bufhidden=hide
  setlocal buftype=nofile
  setlocal foldcolumn=0
  setlocal nofoldenable
  setlocal nonumber
  setlocal noswapfile
  setlocal winfixheight
  setlocal winfixwidth
endfunction
function! ScratchToggle()
  let l:bufNr = bufnr("$")
  let l:name = "__scratch__"
  while l:bufNr > 0
    if buflisted(l:bufNr)
      if (matchstr(bufname(l:bufNr), l:name."$") == l:name )
        execute "w /tmp/.scratchfile"
        execute "bd ".l:bufNr
        return
      endif
    endif
    let l:bufNr = l:bufNr-1
  endwhile
  call ScratchOpen()
endfunction
nnoremap <silent><leader>u :call ScratchToggle()<cr>





"                            Plugin settings                           "
"                    ==============================                    "

" Fzf fuzzy search
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_tags_command = 'ctags -R'
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
command! -bang History call fzf#vim#history( {'options': ['--query', '!.git/ !.vim/ ', '--no-sort']}, <bang>0)
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
nnoremap <silent><Enter> :FZF<cr>
nnoremap <silent> <leader><Enter> :History<cr>
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
nnoremap ,e :FZF<cr>
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)
nnoremap <leader>c :Commands<CR>
nnoremap <leader>b :Buffers<cr>
let g:fzf_layout = { 'down': '~40%' }  " Default fzf layout
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Jedi python
let g:jedi#use_splits_not_buffers = "bottom"
let g:jedi#goto_command = "<leader>d"
let g:jedi#goto_assignments_command = ""
let g:jedi#goto_definitions_command = ""
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = ""
let g:jedi#completions_command = ""
let g:jedi#rename_command = "<leader>r"
let g:jedi#max_doc_height = 30

" MRU
let MRU_Max_Entries = 200
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Auto_Close = 1
let MRU_Max_Menu_Entries = 10

" nvim-completion-engine
set shortmess+=c   " supress the annoying completion messages
" autocmd CompleteDone * pclose
" set completeopt+=noinsert
" set completeopt-=preview

" Fugitive
nnoremap <silent><leader>g :Gstatus\|normal!gg7j<cr>
command! Gl normal! :!git vhm<cr>

" Startify
nnoremap ,l :Startify<cr>
highlight StartifyBracket ctermfg=240
highlight StartifyFooter  ctermfg=240
highlight StartifyHeader  ctermfg=114
highlight StartifyNumber  ctermfg=215
highlight StartifyPath    ctermfg=245
highlight StartifySlash   ctermfg=240
highlight StartifySpecial ctermfg=240
let g:startify_skiplist = [
       \ '\.png',
       \ '\.jpeg',
       \ ]

" Drag Visuals
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight
vmap D <Plug>SchleppDup

" Limelight
let g:limelight_conceal_ctermfg=0

" Ale
let g:ale_sign_column_always = 1
let g:ale_sign_error = ':x'
let g:ale_sign_warning = ':!'
highlight ALEErrorSign ctermfg=196
highlight ALEWarningSign ctermfg=226
autocmd User ALELint call lightline#update()
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'python': ['flake8']
\}
let g:ale_virtualenv_dir_names = ['~/.virtual_envs']
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%severity%][%linter%] %s '
let g:ale_lint_on_enter = 0

" Vim-Tmux navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <m-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <m-j> :TmuxNavigateDown<cr>
nnoremap <silent> <m-k> :TmuxNavigateUp<cr>
nnoremap <silent> <m-l> :TmuxNavigateRight<cr>
nnoremap <silent> <m-/> :TmuxNavigatePrevious<cr>

" Vim Indentline
let g:indentLine_enabled = 1
let g:vim_json_syntax_conceal = 0
augroup TerminalStuff
  au!
  autocmd TermOpen * setlocal nonumber norelativenumber
  autocmd TermOpen * IndentLinesDisable
augroup END

" Easy allign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Signfy
highlight SignifySignAdd    cterm=bold ctermbg=240  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=240  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=240  ctermfg=227

" Lightline
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'right': [ [ 'percent' ],
      \              [ 'linter_warnings', 'linter_errors', 'linter_ok'] ],
      \   'left': [ [ 'mode', 'paste' ],
      \             ['filename', 'modified', 'cutpoint' ] ]
      \ },
      \   'tabline': {
      \     'left': [ [ 'tabs' ] ],
      \     'right': [ [ 'close' ] ]
      \   },
      \   'tab': {
      \     'active': [ 'tabnum', 'filename', 'modified' ],
      \     'inactive': [ 'tabnum', 'filename', 'modified' ]
      \ },
      \ 'component': {
      \   'cutpoint': '%<'
      \ },
      \ 'component_function': {
      \   'gitbranch': 'GitBranch',
      \   'percent': 'LinePercent',
      \ },
      \ 'component_expand': {
      \   'linter_warnings': 'LightlineLinterWarnings',
      \   'linter_errors': 'LightlineLinterErrors',
      \   'linter_ok': 'LightlineLinterOK'
      \ },
      \ 'component_type': {
      \   'readonly': 'error',
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error'
      \ },
      \'mode_map': {
      \   'n': '! ', 'i': '! ', 'R': '! ', 'v': '! ', 'V': '| ', "\<C-v>": ': ',
      \   'c': '! ', 's': 'se', 'S': 'sl', "\<C-s>": 'sl', 't': '! '
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' },
      \ }

function! GitBranch() abort
    return strlen(fugitive#head()) ? ' ' . fugitive#head() : ''
endfunction

function! LinePercent()
    return line('.') * 100 / line('$') . '%'
endfunction

function! LightlineLinterWarnings() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf(' %d', all_non_errors)
endfunction

function! LightlineLinterErrors() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  return l:counts.total == 0 ? '' : printf(' %d', all_errors)
endfunction

function! LightlineLinterOK() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  return l:counts.total == 0 ? '' : ''
endfunction

" Sleuth auto indent
let g:sleuth_automatic = 1

" Polyglot
let g:polyglot_disabled = ['markdown', 'md', 'latex', 'tex', 'julia']

" Markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']

" Gentags
let g:gen_tags#blacklist = ['$HOME']

" Nvim completion manager
" let g:cm_matcher = {'module': 'cm_matchers.abbrev_matcher'}
let g:cm_matcher = {'module': 'cm_matchers.fuzzy_matcher', 'case': 'smartcase'}

" Jplus
nmap J <Plug>(jplus)
vmap J <Plug>(jplus)
nmap <Leader>J <Plug>(jplus-getchar)
vmap <Leader>J <Plug>(jplus-getchar)
let g:jplus#config = {
\	"_" : {
\		"delimiter_format" : ' %d '
\	}
\}

" Vim Router
let g:rooter_use_lcd = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
let g:rooter_patterns = ['Rakefile', 'Makefile', 'package.json', '.git/', '.vscode']

" Vim go
let g:go_fmt_autosave = 0
let g:go_fmt_fail_silently = 1
let g:go_list_type = "quickfix"
let g:go_fmt_command = "goimports"
let g:go_addtags_transform = "camelcase"
autocmd BufEnter *.go nnoremap <leader>d :GoDef<cr>
autocmd BufEnter *.go nnoremap <leader>r :GoRun<cr>
autocmd BufEnter *.go nnoremap <leader>a :GoBuild<cr>
autocmd BufEnter *.go nnoremap <leader>t :GoTest<cr>

" Goyo
function! s:goyo_enter()
  silent !tmux set status off
  silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  silent !tmux set status on
  silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
endfunction
autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" Nvim typescript
autocmd BufEnter *.ts,*.js,*.tsc nnoremap <leader>d :TSDef<cr>
autocmd BufEnter *.ts,*.js,*.tsc nnoremap K :TSDoc<cr>

" Racer
au FileType rust nmap <silent><leader>d <Plug>(rust-def)
au FileType rust nmap K <Plug>(rust-doc)
au FileType rust nmap <silent><leader>a :Dispatch cargo build<cr>
au FileType rust nmap <silent><leader>r :Start cargo run<cr>

" Autoformat
nnoremap <silent>,, :Autoformat<cr>
au FileType go nnoremap <silent>,, :GoFmt<cr>
au FileType javascript,typescript,css,less,scss,json,graphql,markdown nnoremap <silent>,, :Prettier<cr>
au FileType elm nnoremap <silent>,, :ElmFormat<cr>

" JSX Typescript
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx
hi xmlTagName guifg=#87d787 ctermfg=114
hi xmlTag guifg=#87d787 ctermfg=114
hi xmlEndTag guifg=#87d787 ctermfg=114

" Emmet
let user_emmet_expandabbr_key = '<m-i>'
let g:user_emmet_settings = {
\  'javascript.jsx' : {
\      'extends' : 'jsx',
\  },
\}

" CloseTag
let g:closetag_filenames = '*.html,*.xhtml,*.xml,*.js,*.html.erb,*.md'

" Prettier
let g:prettier#config#print_width = 100
let g:prettier#config#tab_width = 2
let g:prettier#config#use_tabs = 'false'
let g:prettier#config#semi = 'false'
let g:prettier#config#single_quote = 'true'
let g:prettier#config#bracket_spacing = 'true'
let g:prettier#config#jsx_bracket_same_line = 'true'
let g:prettier#config#trailing_comma = 'es5'

" SuperTab
let g:SuperTabDefaultCompletionType = "<c-n>"

" Language server protocol
let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'go': ['go-langserver'],
    \ 'css': ['css-languageserver', '--stdio']
    \ }
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> ge :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

" Github Dashboard
let g:github_dashboard = { 'username': 'meain' , 'password': $GITHUB_DASHBOARD_VIM_TOKEN }

" Elm
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_setup_keybindings = 1
autocmd BufEnter *.elm nnoremap <leader>d :ElmShowDocs<cr>
autocmd BufEnter *.elm nnoremap <leader>r :ElmMake<cr>
autocmd BufEnter *.elm nnoremap <leader>a :ElmMakeMain<cr>
autocmd BufEnter *.elm nnoremap <leader>t :ElmTest<cr>

" Easymotion
let g:EasyMotion_do_mapping=0
nmap  <Leader>e <Plug>(easymotion-bd-w)
nmap  <Tab> <Plug>(easymotion-bd-w)
