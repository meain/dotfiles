"                            Plugin Management                         "
"                    ==============================                    "

let g:javascript_filetypes = ['javascript', 'javascript.jsx', 'typescript', 'typescript.tsx']
call plug#begin('~/.local/share/nvim/plugged')

" Visual enhancements
Plug 'mhinz/vim-janah'                                                                         " Janah colorscheme
Plug 'Yggdroot/indentLine'                                                                     " Show indent
Plug 'mhinz/vim-signify'                                                                       " Git diff icons in gutter
" Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }                                           " Hyper focus editing
" Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }                                                     " Centerify
Plug 'ap/vim-css-color', { 'for': 'css' }                                                      " Show colors
" Plug 'yuttie/comfortable-motion.vim'                                                           " Scroll up and down
Plug 'machakann/vim-highlightedyank'                                                           " Visually highlight yanked region

" Added functinality
Plug 'meain/vim-startify'                                                                      " A fancy start page for vim (slow)
Plug '~/Documents/Projects/others/done/_vim/vim-googler', { 'on': 'Google' }                   " Google from within vim
Plug 'justinmk/vim-sneak'                                                                      " Quickly sneak arround
Plug 'wincent/loupe'                                                                           " Better search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }                              " Fzf
Plug 'junegunn/fzf.vim'                                                                        " Fzf for vim
Plug 'jremmen/vim-ripgrep', { 'on': 'Rg' }                                                     " Rg with quickfix list
Plug 'christoomey/vim-tmux-navigator'                                                          " Seamless navigation between vim and tmux
Plug 'suan/vim-instant-markdown', { 'for': [ 'md', 'markdown' ] }                              " View markdown in browser while editing
Plug 'ervandew/supertab'                                                                       " Autocomplete on tab
Plug 'zirrostig/vim-schlepp'                                                                   " Better drag visuals
" Plug 'xtal8/traces.vim'                                                                        " Interactive subsititue
Plug 'vim-scripts/restore_view.vim'                                                            " Restore file pointer
Plug 'tpope/vim-dispatch', { 'on': 'Dispatch' }                                                " Async tasks in vim
Plug 'airblade/vim-rooter'                                                                     " Change directory to project root
Plug 'editorconfig/editorconfig-vim'                                                           " Editorconfig
" Plug 'mattn/gist-vim', { 'on': 'Gist' }                                                        " Push current buffer as gist
" Plug 'rizzatti/dash.vim', { 'on': 'Dash' }                                                     " Search Dash docs
" Plug 'davidbeckingsale/writegood.vim', { 'on': ['WritegoodEnable', 'WritegoodToggle'] }        " Better writing mode
Plug 'mattn/emmet-vim', { 'for': ['html', 'css'] + g:javascript_filetypes }                    " Emmet
" Plug 'jreybert/vimagit', { 'on': [ 'Magit', 'MagitOnly' ] }                                    " Even better git interface for vim
" Plug 'metakirby5/codi.vim', { 'on': 'Codi' }                                                   " Live code preview
Plug 'rhysd/committia.vim'                                                                     " Better COMMIT_EDITMSG editing
Plug 'Galooshi/vim-import-js', {'for': g:javascript_filetypes, 'do': 'npm i -g import-js'}     " Easier imports for javascript

" Code editing enhacements
Plug 'tpope/vim-sleuth'                                                                        " Automatic indentation setting
Plug 'tpope/vim-fugitive', { 'on': [ 'Gstatus', 'Gblame' ] }                                   " Git stuff from within vim
Plug 'majutsushi/tagbar', { 'on' : 'Tagbar' }                                                  " Class/module browser
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }                                             " Undo tree
Plug 'tpope/vim-vinegar'                                                                       " Simpler file browser
Plug 'scrooloose/nerdtree', { 'on': [ 'NERDTree', 'NERDTreeFind', 'NERDTreeToggle' ] }         " Nerdtree
" Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': [ 'NERDTree', 'NERDTreeFind', 'NERDTreeToggle' ] } " Git sign for nerdtree
Plug 'meain/vim-automkdir'                                                                     " Automatically create parent dirs
Plug 'yardnsm/vim-import-cost', { 'do': 'npm install', 'for': g:javascript_filetypes }         " See import cost of javascript deps

" Morph code
Plug 'tpope/vim-surround'                                                                      " Surround
Plug 'jiangmiao/auto-pairs'                                                                    " Autopair
Plug 'tomtom/tcomment_vim'                                                                     " Code commenting
Plug 'junegunn/vim-easy-align'                                                                 " Some prettification

" Language helpers
" Plug 'sheerun/vim-polyglot'                                                                  " Multiple language support (slow)
" Plug 'jonsmithers/experimental-lit-html-vim'                                                 " lit-html highlight
Plug 'elzr/vim-json', { 'for': 'json' }                                                        " Json syntax highlight
" Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }                    " Javascript syntax highlight
Plug 'HerringtonDarkholme/yats.vim', { 'for': [ 'typescript', 'typescript.tsx' ] }             " Typescript syntax highlight
" lug 'leafgarland/typescript-vim', { 'for': [ 'typescript', 'typescript.tsx' ] }              " Typescript syntax highlight
Plug 'peitalin/vim-jsx-typescript', { 'for': [ 'typescript', 'typescript.tsx' ] }              " Typescript JSX syntax highlight
Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx'] }                   " Inproved JSX syntax
Plug 'tpope/vim-markdown', { 'for': ['md', 'markdown'] }                                       " Better markdown support
Plug 'cespare/vim-toml', { 'for': 'toml' }
" Plug 'fatih/vim-go', { 'for': ['go'] }                                                         " Golang helper
" Plug 'sebdah/vim-delve', { 'for': 'go' }                                                       " Debugger for go
" Plug 'racer-rust/vim-racer', { 'for': 'rust' }                                                 " Rust support
" Plug 'numirias/semshi', { 'for': 'python', 'do': ':UpdateRemotePlugins' }                      " Better syntax highlight for python
" Plug 'raimon49/requirements.txt.vim', { 'for': 'requirements' }                              " Requirements file

" Language enhacements
Plug '~/Documents/Projects/projects/vim-jsontogo' , { 'for': ['go'] }                          " Convert JSON to Go struct

" Linting / Checking
Plug 'w0rp/ale'                                                                                " Linter formatter and more
" Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' }                                          " Beautify code

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }                                  " Competion framework
" Plug 'tbodt/deoplete-tabnine', { 'do': './install.sh' }
" Plug 'Shougo/echodoc.vim'                                                                      " Show signature
Plug 'Shougo/neco-vim', { 'for': 'vim' }                                                       " Completion for viml
Plug 'sebastianmarkow/deoplete-rust', { 'for': ['rs', 'rust'] }                                " Rust autocompletion (slow)
" Plug 'wellle/tmux-complete.vim'                                                              " Tmux completion
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }           " Language client

" Snippets
Plug 'SirVer/ultisnips'                                                                        " Snippet manager

" Dependencies
Plug 'vim-scripts/mru.vim'                                                                     " Save recently used files (for v)
Plug 'radenling/vim-dispatch-neovim', { 'on': 'Dispatch' }                                     " Neovim support for vim-dispatch
" Plug 'mattn/webapi-vim'                                                                        " Web calls
Plug 'tpope/vim-repeat'                                                                        " Repeat plugins with .

" Others
" Plug 'sedm0784/vim-you-autocorrect', { 'on': 'EnableAutocorrect' }                             " Autocorrect like in phones
" Plug 'junegunn/vim-online-thesaurus', { 'on': 'OnlineThesaurusCurrentWord' }                   " Online thesaurus
" Plug 'kristijanhusak/vim-carbon-now-sh', { 'on': 'CarbonNowSh' }                               " Polaroid for code
" Plug 'wakatime/vim-wakatime'                                                                   " Wakatime (slow)
" Plug 'junegunn/vim-github-dashboard', { 'on': ['GHA', 'GHD']}                                  " Github dashboard
" Plug 'vim-scripts/loremipsum', { 'on': 'Loremipsum' }                                          " Loremipsum
" Plug 'sotte/presenting.vim', { 'on': 'PresentingStart' }                                       " Presetation in vim
" Plug 'junegunn/vader.vim', {'for': 'vader'}                                                    " Vimscript testing framework
Plug '~/Documents/Projects/projects/vim-colorswitch', { 'do': 'UpdateRemotePlugins' }

call plug#end()





"                           Editor Settings                            "
"                    ==============================                    "


" Set encoding to utf8
set encoding=utf8

" Make autocomplete for filenames work
set path+=**

" Turn on line numbers
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
augroup END

" Highlight cursor line (slows down)
set nocursorline

" Show statusline
set laststatus=2

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
set backup                        " make backup files
set undofile                      " persistent undos - undo after you re-open the file
set directory=~/.vim/dirs/tmp     " directory to place swap files in
set backupdir=~/.vim/dirs/backups " where to put backup files
set undodir=~/.vim/dirs/undodir   " undo directory

" Allow mouse
set mouse=a

" Incremental search
set incsearch

" Incremental substitution ( now using traces.vim)
set inccommand=nosplit

" Highlighted search results
set hlsearch

" Smart search
set ignorecase

" Do not put two spaces after .
set nojoinspaces

" show partial commands
set showcmd

" Allow plugins by file type (required for plugins!)
filetype plugin on
filetype indent on

" Redraw only when essential
set lazyredraw

" Just sync some lines of a large file
set synmaxcol=400
syntax sync minlines=256

" Make backspace great again
set backspace=2
" Set split direction
set splitbelow
set splitright

" no jumping arround
set noshowmatch

" Set updatetime
set updatetime=2000

" Tabs config
set expandtab
set smarttab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround

" Don't show --INSERT-- at bottom
set noshowmode

" Set textwidth
set textwidth=120

" Show invisibles
set list
set listchars=tab:\ \ ,eol:¬,trail:⋅

" Show line breaks
set showbreak=…

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
set wildignore+=.cache,node_modules,package-lock.json,yarn.lock,dist,.git

" Setting up python
let g:python_host_prog = '/Users/meain/.pyenv/versions/2.7.11/bin/python'
let g:python3_host_prog = '/Users/meain/.pyenv/versions/3.6.6/bin/python'

" Fill chars
set fillchars=vert:\|
set fillchars+=fold:-

" Indent based folding
set foldlevel=0
set foldmethod=indent
" set foldmethod=manual
set foldignore=
set foldlevelstart=10
set foldnestmax=10

" ColorScheme change ( redblack )
autocmd ColorScheme redblack highlight Normal guibg=#000000 ctermbg=0
autocmd ColorScheme redblack highlight SignColumn guibg=#000000 ctermbg=0
autocmd ColorScheme redblack highlight LineNr guibg=#000000 ctermbg=0
autocmd ColorScheme redblack highlight VertSplit guibg=#000000 ctermbg=0
autocmd ColorScheme redblack highlight StatusLine guifg=#dadada ctermfg=253 ctermbg=NONE guibg=NONE gui=NONE cterm=NONE
autocmd ColorScheme redblack highlight StatusLineNC guifg=#878787 ctermfg=102 ctermbg=NONE guibg=NONE gui=NONE cterm=NONE
autocmd ColorScheme redblack highlight StartifyFile guifg=#eeeeee ctermfg=255 ctermbg=NONE gui=NONE cterm=NONE
autocmd ColorScheme redblack highlight SignifySignAdd    cterm=bold gui=bold  ctermfg=119 guifg=#87ff5f
autocmd ColorScheme redblack highlight SignifySignDelete cterm=bold gui=bold  ctermfg=167 guifg=#d75f5f
autocmd ColorScheme redblack highlight SignifySignChange cterm=bold gui=bold  ctermfg=227 guifg=#ffff5f
autocmd ColorScheme redblack highlight ColorColumn ctermbg=154 guibg=#212121
autocmd ColorScheme redblack highlight CursorLine ctermbg=154 guibg=#212121
autocmd ColorScheme redblack highlight CursorLineNr ctermfg=red guifg=red ctermbg=NONE guibg=NONE
autocmd ColorScheme redblack highlight WildMenu ctermfg=red guifg=red ctermbg=NONE guibg=NONE

" ColorScheme change ( janah )
autocmd ColorScheme janah highlight Normal ctermbg=234 guibg=#1f1f1f
autocmd ColorScheme janah highlight SignColumn ctermbg=234 guibg=#1f1f1f
autocmd ColorScheme janah highlight LineNr ctermbg=234 guibg=#1f1f1f
autocmd ColorScheme janah highlight VertSplit ctermbg=234 guibg=#1f1f1f
autocmd ColorScheme janah highlight StatusLineNC guifg=#878787 ctermfg=102 ctermbg=NONE guibg=NONE gui=NONE cterm=NONE
autocmd ColorScheme janah highlight StatusLine guifg=#878787 ctermfg=102 guibg=#f5f5f5 ctermfg=253 gui=NONE cterm=NONE
autocmd ColorScheme janah highlight SignifySignAdd    cterm=bold gui=bold  ctermfg=119 guifg=#87ff5f guibg=#1f1f1f
autocmd ColorScheme janah highlight SignifySignDelete cterm=bold gui=bold  ctermfg=167 guifg=#d75f5f guibg=#1f1f1f
autocmd ColorScheme janah highlight SignifySignChange cterm=bold gui=bold  ctermfg=227 guifg=#ffff5f guibg=#1f1f1f
autocmd ColorScheme janah highlight ColorColumn ctermbg=154 ctermfg=0 guibg=#474747 guifg=#ffffff
autocmd ColorScheme janah highlight CursorLine ctermbg=154 guibg=#474747
autocmd ColorScheme janah highlight CursorLineNr ctermbg=NONE guibg=NONE guifg=#df005f ctermfg=161
autocmd ColorScheme janah highlight WildMenu guifg=#df005f ctermfg=161 guibg=NONE ctermbg=NONE gui=bold cterm=bold

" ColorScheme change (pencil:light)
autocmd ColorScheme pencil highlight SignifySignAdd    cterm=bold guifg=#00ff00
autocmd ColorScheme pencil highlight SignifySignDelete cterm=bold guifg=#ff0000
autocmd ColorScheme pencil highlight SignifySignChange cterm=bold guifg=#0000ff

" Setting colorscheme
" janah, base16-greyscale, redblack, seoul256, gruvbox, grb256,
" tomorrow-night, zenburn, base16-classic-light, base16-classic-dark
" light:pencil
set termguicolors
set background=dark
colorscheme janah

" Colors common
highlight NonText ctermfg=238 guifg=#444444

" Use italics for some text
highlight htmlArg gui=italic
highlight htmlArg cterm=italic
highlight Comment gui=italic
highlight Comment cterm=italic
" highlight Type    gui=italic
" highlight Type    cterm=italic

" Highlight git conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Set up leader keys
let mapleader = ' '
let maplocalleader = '\|'

" Smart colorcolumn
augroup custom_colorcolum
  autocmd!
  autocmd BufEnter * call matchadd('ColorColumn', '\%160v', 100)
augroup end

" Better coloring for errors
highlight clear SpellBad
highlight SpellBad cterm=underline gui=underline ctermfg=11 guifg=#ffff00

" Don't you f'in touch my cursor
set guicursor=

" Completefunc
augroup custom_completefunc
  autocmd!
  autocmd BufEnter * set completefunc=cm#_completefunc
augroup end

" Oh-my-zsh themes are shell.
augroup custom_zsh_theme
  autocmd!
  autocmd BufNewFile,BufRead *.zsh-theme set filetype=sh
augroup end

" Resize panes whenever containing window resized.
augroup custom_vimresize
  autocmd!
  autocmd VimResized * wincmd =
augroup end

" Make sure it is javascript
augroup Filetype javascript syntax=javascript

" Make vim edit crontab
autocmd filetype crontab setlocal nobackup nowritebackup

" Remove whitespace at save
autocmd BufWritePre *.css,*.js,*.html,*.cpp,*.c,*.java,*.go,*.rs,*.ts,*.cljs,*.clj :%s/\s\+$//e

" Trun of syntax hilighting if huge
autocmd BufReadPre *
      \ if getfsize(expand("%")) > 10000000 | syntax off | endif
autocmd BufReadPre *.min.* syntax off





"                             Abbreviations                            "
"                    ==============================                    "

iabbr cosnt const
iabbr imprt import
iabbr imoprt import




"                                Commands                              "
"                    ==============================                    "

" Save as sudo
ca w!! w !sudo tee "%"

" I am too lazy to take my hands from shift
command! WQ wq
command! Wq wq
command! W w

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap

" vertical help
ca h vert h





"                               Key remaps                             "
"                    ==============================                    "


" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Easier line navigatin
nnoremap H g^
nnoremap L g$

" Eaier quit and no ex
nnoremap <silent> q :q<cr>
nnoremap Q q

"Get back to where you were easily
nnoremap gg mpgg
nnoremap G mpG
nnoremap / mp/

" Quick command mode (second binding to get find next)
nnoremap ; :
vnoremap ; :
nnoremap ' ;
vnoremap ' ;

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

" Easy buffer switching
nnoremap <silent><Leader><Tab> :tabnew\|:Startify<cr>

" Terminal split jump
tnoremap <m-h> <C-\><C-N><C-w>h
tnoremap <m-j> <C-\><C-N><C-w>j
tnoremap <m-k> <C-\><C-N><C-w>k
tnoremap <m-l> <C-\><C-N><C-w>l

" Terminal mode esc remap
tnoremap <Leader><Esc> <C-\><C-n>

" Copy entire file content
nnoremap yp mzggVG"+y`z

" Vim docs
au FileType vim nmap K :help <c-r><c-w><cr>

" Don't change Enter in all buffers
autocmd FileType help nnoremap <buffer> <Enter> <Enter>
autocmd FileType vim-plug nnoremap <buffer> <Enter> <Enter>

" Go back and fwd smoother
nnoremap <silent>\ <c-o>
nnoremap <silent>~ <c-i>

" Easier arrow keys
inoremap <silent> <m-h> <left>
inoremap <silent> <m-j> <down>
inoremap <silent> <m-k> <up>
inoremap <silent> <m-l> <right>

" Shady remaps
autocmd FileType c,cpp,css inoremap ;; <Esc>A;
autocmd FileType go inoremap ;; :=

" Visual star
vnoremap * "sy/<c-r>s<CR>

" Source current line
vnoremap <silent><leader>S y:execute @@<cr>
nnoremap <silent><leader>S ^vg_y:execute @@<cr>

" vv to select line
nnoremap vv ^vg_





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <silent><leader><leader> :w<cr>
nnoremap <silent><Leader>q :q<cr>
nnoremap <silent><Leader>w :w<cr>

" Split like a boss
nnoremap <silent><Leader>v :vsplit \| :Startify<cr>
nnoremap <silent><Leader>h :split \| :Startify<cr>

" Clear search highlight
nnoremap <silent><Leader>/ :nohls<CR>

" Easy tag navigation
nnoremap <silent><Leader>' <C-]>

" Quick fold and unfold
nnoremap <silent><Leader><esc> :normal!za<cr>

" Quick suspend
nnoremap <silent><leader>z <c-z>

" Quick switch tabs
nnoremap <silent><Leader>k :tabn<cr>
nnoremap <silent><Leader>j :tabp<cr>
nnoremap <silent><s-right> :tabm +1<cr>
nnoremap <silent><s-left> :tabm -1<cr>

" Only current buffer
nnoremap <silent><leader>o :only<cr>

" Quickly copy current line without extra spaces or newline
nnoremap <silent><leader>y ^yg_

" New tab
nnoremap <silent><leader>t :tabnew \| :Startify<cr>

" Easier switching of quickfix list items
nnoremap <silent><leader>n :cnext<cr>
nnoremap <silent><leader>m :cprev<cr>

" Change from class to className
autocmd FileType javascript nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
autocmd FileType javascript.jsx nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
autocmd FileType typescript nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
autocmd FileType typescript.tsx nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l

" quick search and replace
nnoremap <leader>r :%s/

" Hack until QuickPrint is a thing
autocmd FileType javascript.jsx vnoremap <silent><leader>p "pyoconsole.log("<esc>"ppa", <esc>"ppa)<esc>
autocmd FileType javascript.jsx nnoremap <silent><leader>p viw"pyoconsole.log("<esc>"ppa", <esc>"ppa)<esc>
autocmd FileType javascript vnoremap <silent><leader>p "pyoconsole.log("<esc>"ppa", <esc>"ppa)<esc>
autocmd FileType javascript nnoremap <silent><leader>p viw"pyoconsole.log("<esc>"ppa", <esc>"ppa)<esc>
autocmd FileType python vnoremap <silent><leader>p "pyoprint("<esc>"ppa", <esc>"ppa)<esc>
autocmd FileType python nnoremap <silent><leader>p viw"pyoprint("<esc>"ppa", <esc>"ppa)<esc>





"                             Autocommands                             "
"                    ==============================                    "

augroup improved_autoread
  autocmd!
  autocmd FocusGained,BufEnter * silent! checktime
augroup end





"                                Code                                  "
"                    ==============================                    "


" Spell checking
augroup custom_spellcheck
  autocmd!
  autocmd BufRead,BufNewFile *.md setlocal spell
  autocmd BufRead,BufNewFile *.org setlocal spell
  autocmd BufRead,BufNewFile *.txt setlocal spell
  autocmd FileType gitcommit setlocal spell
  autocmd FileType help setlocal nospell
augroup end
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

" Toggle quickfix
function! QuickfixToggle()
    let nr = winnr('$')
    cwindow
    let nr2 = winnr('$')
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
nnoremap <silent> `` :ZoomToggle<CR> \| :normal! 0<cr>

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
      normal! mz
      %s/\s\+$//e
      normal! `z
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
nnoremap <silent>_ :call Marks()<cr>

" Json format
function! JSONFormat() range
  execute '%!python -m json.tool'
endfunction
command! -range JSONFormat <line1>,<line2>call JSONFormat()

" Generate Gitignore
function! GitignoreGenerate()
    call inputsave()
    echo 'Auto generate .gitignore (y/n): '
    let l:yesno = nr2char(getchar())
    call inputrestore()
    if l:yesno ==? 'y'
      call inputsave()
      let l:lang = input('Language: ')
      echo 'Fetching .gitignore for ' . l:lang
      execute '%!curl -L -s https://www.gitignore.io/api/linux,macos,windows,' . l:lang . ' | sed "1,3d;$d" | sed "$d" | sed "$d"'
      call inputrestore()
    else
      echo ''
    endif
    redraw
endfunction
command! GitignoreGenerate :call GitignoreGenerate()
augroup GitIgnore
  au!
  autocmd BufNewFile .gitignore call GitignoreGenerate()
augroup end

" Scratch buffer
function! ScratchOpen()
  execute 'topleft new __scratch__'
  if filereadable('/tmp/.scratchfile')
    execute ' %! cat /tmp/.scratchfile'
  endif
  redraw
  execute 'resize 15'
  execute 'set ft=scratch'
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
  let l:bufNr = bufnr('$')
  let l:name = '__scratch__'
  while l:bufNr > 0
    if buflisted(l:bufNr)
      if (matchstr(bufname(l:bufNr), l:name.'$') == l:name )
        execute 'w /tmp/.scratchfile'
        execute 'bd '.l:bufNr
        return
      endif
    endif
    let l:bufNr = l:bufNr-1
  endwhile
  call ScratchOpen()
endfunction
nnoremap <silent><leader>u :call ScratchToggle()<cr>

" Close unused buffers
function! s:CloseHiddenBuffers()
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
command! Cleanup call s:CloseHiddenBuffers()

" Find highlight group of char under the cursor
function! ShowHightlightGroup()
  if !exists('*synstack')
    return
  endif
  let name = synIDattr(synID(line('.'),col('.'),1),'name')
  let trans = synIDattr(synID(line('.'),col('.'),0),'name')
  let lo = synIDattr(synIDtrans(synID(line('.'),col('.'),1)),'name')
  echo 'hi[' . name . '] trans[' . trans .'] lo[' . lo . ']'
endfunc
nnoremap <silent><leader>gh :call ShowHightlightGroup()<cr>

" Remove space after bracket on joining
function! Join()
  let previous_last_char = getline('.')[col('$')-2]
  normal! J
  let current_char = getline('.')[col('.')-1]
  if previous_last_char =~ '(\|[\' && current_char == ' '
    normal! x
  endif
endfunction
nnoremap <silent>J :call Join()<CR>

" Quickly print a variable
" function! QuickPrint()
"   let print_format = 
"         \{ 'javascript.jsx': 'console.log(";;;;", ;;;;)', 
"         \'javascript': 'console.log(";;;;", ;;;;)',
"         \'python': 'print(";;;", ;;;)'}
"   let current_language = 'javascript.jsx'
"   let value_to_print = @*
" endfunction
" vnoremap <silent><leader>p :call QuickPrint()<cr>


" Stratr profiling
function! Profile()
  profile start profile.log
  profile func *
  profile file *
endfunction





"                            Plugin settings                           "
"                    ==============================                    "

" Fzf fuzzy search
let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_tags_command = 'ctags -R'
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
command! -bang History call fzf#vim#history({'options': ['--query', '!.git/ !.vim/ ', '--no-sort', '--preview', 'cat {}']}, <bang>0)
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -bang Open call fzf#run({'source': 'rg --files --hidden --follow --glob "!.git/*"', 'sink': 'e', 'down': '40%', 'options': '--preview "cat {}"'})
nnoremap <silent><Enter> :FZF<cr>
nnoremap <silent> <leader><Enter> :History<cr>
command! -bang -nargs=* Find
      \ call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>),
      \ 1, <bang>0)
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Find<cr>
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'Identifier', 'Normal', 'Normal'],
  \ 'bg+':     ['bg', 'Normal', 'Normal'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'WildMenu'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
let g:fzf_layout = { 'down': '~40%' }  " Default fzf layout

" MRU
let MRU_Max_Entries = 200
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Auto_Close = 1
let MRU_Max_Menu_Entries = 10

" Supress completion messages
set shortmess+=c

" Fugitive
nnoremap <silent><leader>g :Gstatus\|normal!gg7j<cr>
command! Gl normal! :!git vhm<cr>

" Startify
function! s:list_commits()
  let git = 'git -C ' . getcwd()
  let commits = systemlist(git . ' log --oneline | head -n5')
  let git = 'G' . git[1:]
  return map(commits, '{"line": matchstr(v:val, "\\s\\zs.*"), "cmd": "'. git .' show ". matchstr(v:val, "^\\x\\+") }')
endfunction
nnoremap <silent>,l :Startify<cr>
augroup custom_startify
  autocmd!
  autocmd User Startified setlocal cursorline
augroup end
highlight StartifyBracket ctermfg=240 guifg=#585858
highlight StartifyFooter  ctermfg=240 guifg=#585858
highlight StartifyHeader  ctermfg=114 guifg=#87d787
highlight StartifyNumber  ctermfg=215 guifg=#ffaf5f
highlight StartifyPath    ctermfg=245 guifg=#8a8a8a
highlight StartifySlash   ctermfg=240 guifg=#585858
highlight StartifySpecial ctermfg=240 guifg=#585858
let g:startify_files_number = 5
let g:startify_change_to_dir = 0
let g:startify_custom_header = [':meain - neovim','']
let g:startify_relative_path = 1
let g:startify_use_env = 1
let g:startify_session_persistence = 1
let g:startify_custom_indices = ['v','a', 'd', 'g', 'h', 'l', 'o', 'p', 'r', 't', 'n', 'm', 'b']
let g:startify_fortune_use_unicode = 1
let g:startify_list_order = ['dir', 'files',  'sessions']
let g:startify_session_before_save = [
    \ 'echo "Cleaning up before saving.."'
    \ ]
let g:startify_skiplist = [
    \ '\.png',
    \ '\.jpeg',
    \ 'COMMIT_EDITMSG',
    \ escape(fnamemodify(resolve($VIMRUNTIME), ':p'), '\') .'doc',
    \ 'bundle/.*/doc',
    \ '\.vimgolf',
    \ '^/tmp',
    \ ]
let g:startify_lists = [
\  { 'type': 'dir',		  'header': [ 'Files '. getcwd() ] },
\  { 'type': function('s:list_commits'), 'header': [ 'Recent Commits' ] },
\  { 'type': 'sessions',  'header': [ 'Sessions' ]		 },
\  { 'type': 'bookmarks', 'header': [ 'Bookmarks' ]		 },
\  { 'type': 'commands',  'header': [ 'Commands' ]		 },
\ ]
let g:startify_commands = [
\	{ 'up': [ 'Update Plugins', ':PlugUpdate' ] },
\	{ 'ug': [ 'Upgrade Plugin Manager', ':PlugUpgrade' ] },
\	{ 'q': [ 'Delete buffer', ':bd' ] },
\ ]
let g:startify_bookmarks = [
	\ { 'c': '~/.dotfiles/nvim/.config/nvim/init.vim' },
	\ { 'z': '~/.dotfiles/zsh/.zshrc' }
\ ]

" Drag Visuals
let g:Schlepp#reindent = 1
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight
vmap D <Plug>SchleppDup

" Limelight
let g:limelight_conceal_ctermfg=0

" Ale
nnoremap <silent>,, :ALEFix<cr>
let g:ale_sign_column_always = 1
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '⚠'
" let g:ale_virtualtext_cursor = 1
highlight ALEErrorSign ctermfg=196 guifg=#ff0000 ctermbg=234 guibg=#1f1f1f
highlight ALEWarningSign ctermfg=226 guifg=#ffff00 ctermbg=234 guibg=#1f1f1f
let g:ale_statusline_format = ['✖ %d', '⚠ %d', '⬥ ok']
let g:ale_linters = {
\  'javascript': ['flow', 'eslint'],
\  'typescript' : ['tslint', 'eslint'],
\  'typescript.tsx' : ['tslint', 'eslint'],
\  'python': ['pycodestyle', 'pyflakes'],
\  'html': ['tidy'],
\  'css': ['stylelint'],
\  'bash': ['shellcheck'],
\  'zsh': ['shellcheck']
\}
let g:ale_virtualenv_dir_names = ['~/.virtual_envs']
let g:ale_echo_msg_error_str = '✖'
let g:ale_echo_msg_warning_str = '⚠'
let g:ale_echo_msg_format = '%severity% %linter%: %s '
let g:ale_lint_on_enter = 0
let g:ale_fixers = {
\  'javascript' : ['prettier'],
\  'typescript' : ['prettier'],
\  'json' : ['fixjson'],
\  'python' : ['black'],
\  'rust': ['rustfmt'],
\  'go' : ['goimports'],
\  'css' : ['prettier'],
\  'elm' : ['elm-format'],
\  'markdown' : ['prettier'],
\  'html' : ['prettier'],
\  'yaml': ['prettier'],
\  'sh': ['shfmt']
\}
let g:ale_javascript_prettier_use_local_config = 1

" Vim-Tmux navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <m-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <m-j> :TmuxNavigateDown<cr>
nnoremap <silent> <m-k> :TmuxNavigateUp<cr>
nnoremap <silent> <m-l> :TmuxNavigateRight<cr>
nnoremap <silent> <m-/> :TmuxNavigatePrevious<cr>

" Vim Indentline
let g:indentLine_enabled = 1
let g:indentLine_fileTypeExclude = ['startify', 'help']
augroup TerminalStuff
  au!
  autocmd TermOpen * setlocal nonumber norelativenumber
  autocmd TermOpen * IndentLinesDisable
augroup END

" Easy allign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Signfy
let g:signify_sign_add               = 'ǁ'
let g:signify_sign_delete            = 'ǁ'
let g:signify_sign_change            = 'ǁ'

" Sleuth auto indent
let g:sleuth_automatic = 1

" Vim-json
let g:vim_json_syntax_conceal = 0

" Polyglot
" let g:polyglot_disabled = ['markdown', 'md', 'latex', 'tex', 'jsx', 'typescript']

" Markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']

" Gentags
let g:gen_tags#blacklist = ['$HOME']

" Vim Router
let g:rooter_use_lcd = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
let g:rooter_patterns = ['Rakefile', 'Makefile', 'package.json', '.git/', '.vscode']

" Vim go
let g:go_fmt_autosave = 0
let g:go_fmt_fail_silently = 1
let g:go_list_type = 'quickfix'
let g:go_fmt_command = 'goimports'
let g:go_addtags_transform = 'camelcase'
augroup custom_go
  autocmd!
  autocmd BufEnter *.go nnoremap <leader>d :GoDef<cr>
  autocmd BufEnter *.go nnoremap <leader>r :GoRun<cr>
  autocmd BufEnter *.go nnoremap <leader>a :GoBuild<cr>
  autocmd BufEnter *.go nnoremap <leader>t :GoTest<cr>
augroup end

" Goyo
" function! s:goyo_enter()
"   silent !tmux set status off
"   silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
"   set noshowmode
"   set noshowcmd
"   set scrolloff=999
"   Limelight
" endfunction
"
" function! s:goyo_leave()
"   silent !tmux set status on
"   silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
"   set showmode
"   set showcmd
"   set scrolloff=5
"   Limelight!
" endfunction
" autocmd! User GoyoEnter nested call <SID>goyo_enter()
" autocmd! User GoyoLeave nested call <SID>goyo_leave()

" Racer
let g:deoplete#sources#rust#racer_binary='/Users/meain/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path='/Users/meain/Documents/Projects/others/clones/rust/src'
augroup custom_rust
  autocmd!
  " au FileType rust nnoremap <silent><leader>d <Plug>(rust-def)
  " au FileType rust nnoremap K <Plug>(rust-doc)
  au FileType rust nnoremap <silent><leader>a :Dispatch cargo build<cr>
  au FileType rust nnoremap <silent><leader>r :Start cargo run<cr>
augroup end

" JSX Typescript
augroup custom_jsx
  autocmd!
  autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx
augroup end
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

" SuperTab
let g:SuperTabDefaultCompletionType = '<c-n>'

" Language server protocol
let g:LanguageClient_autoStart = 1
let g:LanguageClient_useVirtualText = 0
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls'],
    \ 'javascript': ['flow-language-server', '--stdio'],
    \ 'javascript.jsx': ['flow-language-server', '--stdio'],
    \ 'typescript': ['typescript-language-server', '--stdio'],
    \ 'go': ['go-langserver'],
    \ 'css': ['css-languageserver', '--stdio'],
    \ 'python': ['pyls'],
    \ 'sh': ['bash-language-server', 'start'],
    \ 'zsh': ['bash-language-server', 'start'],
    \ 'dockerfile': ['docker-langserver', '--stdio']
    \ }


let g:LanguageClient_diagnosticsDisplay = {
      \1: {
        \'name': 'Error',
        \'texthl': 'ALEError',
        \'signText': '✖',
        \'signTexthl': 'ALEErrorSign',
        \'virtualTexthl': 'Special',
      \},
      \2: {
        \'name': 'Warning',
        \'texthl': 'ALEWarning',
        \'signText': '⚠',
        \'signTexthl': 'ALEWarningSign',
        \'virtualTexthl': 'Todo',
      \},
      \3: {
        \'name': 'Information',
        \'texthl': 'ALEInfo',
        \'signText': '∴',
        \'signTexthl': 'ALEInfoSign',
        \'virtualTexthl': 'Todo',
      \},
      \4: {
        \'name': 'Hint',
        \'texthl': 'ALEInfo',
        \'signText': '➤',
        \'signTexthl': 'ALEInfoSign',
        \'virtualTexthl': 'Todo',
      \},
      \}
nnoremap <silent><leader>l :call LanguageClient_contextMenu()<CR>
nnoremap <silent> gh :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gx :call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>

" Github Dashboard
let g:github_dashboard = { 'username': 'meain' , 'password': $GITHUB_DASHBOARD_VIM_TOKEN }

" Elm
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_setup_keybindings = 1
augroup custom_elm
  autocmd!
  autocmd BufEnter *.elm nnoremap <leader>d :ElmShowDocs<cr>
  autocmd BufEnter *.elm nnoremap <leader>r :ElmMake<cr>
  autocmd BufEnter *.elm nnoremap <leader>a :ElmMakeMain<cr>
  autocmd BufEnter *.elm nnoremap <leader>t :ElmTest<cr>
augroup end

" NerdTree
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=1
let NERDTreeMinimalUI=1
let NERDTreeHijackNetrw=0
let NERDTreeRespectWildIgnore=1
let NERDTreeStatusline = '         File Browser'
nnoremap <silent><Tab> :NERDTreeToggle<cr>
augroup custom_nerdtree
  autocmd!
  autocmd FileType nerdtree nnoremap <silent><buffer> <Tab> :NERDTreeToggle<cr>
augroup end
let g:NERDTreeIndicatorMapCustom = {
    \ 'Modified'  : '!',
    \ 'Staged'    : '|',
    \ 'Untracked' : '-',
    \ 'Renamed'   : '➜',
    \ 'Unmerged'  : '═',
    \ 'Deleted'   : '✖',
    \ 'Dirty'     : '/',
    \ 'Clean'     : '',
    \ 'Ignored'   : '☒',
    \ 'Unknown'   : '?'
    \ }

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" Echodoc
let g:echodoc#enable_at_startup = 1

" Indentline
let g:indentLine_color_term = 236
let g:indentLine_color_gui = '#303030'

" Ultisnips
let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips', 'UltiSnips']

" Magit
" nnoremap <silent><bs> :Magit<cr>
" augroup custom_magit
"   autocmd!
"   autocmd FileType magit nnoremap <silent><buffer> <bs> :q<cr>
" augroup end

" Sneak
map s <Plug>Sneak_s
map S <Plug>Sneak_S
let g:sneak#label = 1

" AutoPair
let g:AutoPairsShortcutToggle='<c-p>'
let g:AutoPairsMultilineClose = 0

" Vim Plug
command! LoadAllPlugins call plug#load(keys(g:plugs))

" Googler
nnoremap <silent><leader>s :Google <c-r><c-w><cr>
vnoremap <leader>s y:Google <c-r>"<cr>

" Comfortable motion
" let g:comfortable_motion_no_default_key_mappings = 1
" nnoremap <silent> <Up> :call comfortable_motion#flick(100)<CR>
" nnoremap <silent> <Down> :call comfortable_motion#flick(-100)<CR>

" Vim instant markdown
let g:instant_markdown_autostart = 0

" Rg
let g:rg_highlight = 1
let g:rg_command = 'rg --vimgrep --ignore-case --hidden --follow'

" Dash
" nnoremap <silent><leader>; :Dash<cr>

"Loupe
let g:LoupeVeryMagic=0

" Vim javascript
let g:javascript_plugin_flow = 1

" Semshi
let g:semshi#excluded_buffers = ['*']
hi semshiLocal           ctermfg=209 guifg=#ff875f
hi semshiGlobal          ctermfg=214 guifg=#ffaf00
hi semshiImported        ctermfg=214 guifg=#ffaf00 cterm=bold gui=bold
hi semshiParameter       ctermfg=75  guifg=#5fafff
hi semshiParameterUnused ctermfg=117 guifg=#87d7ff cterm=underline gui=underline
hi semshiFree            ctermfg=218 guifg=#ffafd7
hi semshiBuiltin         ctermfg=207 guifg=#ff5fff
hi semshiAttribute       ctermfg=49  guifg=#00ffaf
hi semshiSelf            ctermfg=249 guifg=#b2b2b2
hi semshiUnresolved      ctermfg=226 guifg=#ffff00 cterm=underline gui=underline
hi semshiSelected        ctermfg=231 guifg=#ffffff ctermbg=161 guibg=#d7005f

hi semshiErrorSign       ctermfg=231 guifg=#ffffff ctermbg=160 guibg=#d70000
hi semshiErrorChar       ctermfg=231 guifg=#ffffff ctermbg=160 guibg=#d70000
sign define semshiError text=E> texthl=semshiErrorSign

" vim-import-cost
nnoremap <silent><leader>a :ImportJSFix<cr>
let g:import_cost_virtualtext = 1
let g:import_cost_virtualtext_prefix = ' '

" vim-jsx-typescript
hi tsxTagName ctermfg=150 guifg=#afdf87
hi tsxCloseString ctermfg=216 guifg=#ffaf87
hi tsxCloseTag ctermfg=216 guifg=#ffaf87
hi tsxAttributeBraces ctermfg=181 guifg=#dfafaf
hi tsxEqual ctermfg=216 guifg=#ffaf87
hi tsxAttrib ctermfg=115 guifg=#87dfaf
augroup custom_tsx
  autocmd!
  autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.tsx
augroup end


" HighlightedYank
let g:highlightedyank_highlight_duration = 200
highlight HighlightedyankRegion cterm=underline gui=underline


" Color Swap
autocmd FileType css nnoremap <buffer><leader>c :ColorSwap<CR>




"                             Source External                          "
"                    ==============================                    "

" File manipulations
source ~/.config/nvim/filemanip.vim

" Git stuff
source ~/.config/nvim/gitutils.vim

" Statusline
source ~/.config/nvim/statusline.vim
