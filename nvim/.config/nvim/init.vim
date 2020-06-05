"                            Plugin Management                         "
"                    ==============================                    "

let g:javascript_filetypes = ['javascript', 'javascript.jsx', 'typescript', 'typescript.tsx']
let g:schlepp_triggers = ['<Plug>SchleppUp', '<Plug>SchleppDown', '<Plug>SchleppLeft', '<Plug>SchleppRight', '<Plug>SchleppDup']
call plug#begin('~/.local/share/nvim/plugged')

" Visual enhancements
Plug 'mhinz/vim-janah'                                                                         " Janah colorscheme
Plug 'cormacrelf/vim-colors-github'                                                            " Light colorschrme
Plug 'logico/typewriter-vim'                                                                   " Minimal light colorscheme
Plug 'Yggdroot/indentLine'                                                                     " Show indent
Plug 'mhinz/vim-signify', { 'on': 'LazyLoadPlugins' }                                          " Git diff icons in gutter
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }                                           " Hyper focus editing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }                                                     " Centerify
Plug 'norcalli/nvim-colorizer.lua'                                                             " Highlight color values

" Added functinality
Plug '~/Documents/Projects/others/done/_vim/vim-googler', { 'on': ['LazyLoadPlugins', 'Searcher'] } " Search from within vim
Plug '/usr/local/opt/fzf'                                                                      " Fzf
Plug 'junegunn/fzf.vim'                                                                        " Fzf for vim
Plug 'christoomey/vim-tmux-navigator'                                                          " Seamless navigation between vim and tmux
Plug 'suan/vim-instant-markdown', { 'for': [ 'md', 'markdown' ] }                              " View markdown in browser while editing
Plug 'ervandew/supertab'                                                                       " Autocomplete on tab
Plug 'zirrostig/vim-schlepp', { 'on': g:schlepp_triggers }                                     " Better drag visuals
Plug 'vim-scripts/restore_view.vim'                                                            " Restore file pointer
Plug 'tpope/vim-dispatch', { 'on': [ 'Dispatch', 'Start' ] }                                   " Async tasks in vim
Plug 'airblade/vim-rooter'                                                                     " Change directory to project root
Plug 'editorconfig/editorconfig-vim'                                                           " Editorconfig
Plug 'mattn/emmet-vim', { 'for': ['html', 'css'] + g:javascript_filetypes }                    " Emmet
Plug 'AndrewRadev/splitjoin.vim'                                                               " Split and join lines
Plug 'shinglyu/vim-codespell', { 'on': ['CodeSpell'] }                                         " Spellchecking in code
Plug 'wellle/targets.vim'                                                                      " Adds quite a few text objects

" Code editing enhacements
Plug 'tpope/vim-sleuth'                                                                        " Automatic indentation setting
Plug 'tpope/vim-fugitive', { 'on': [ 'Gstatus', 'Gblame', 'Gdiff' ] }                          " Git stuff from within vim
Plug 'liuchengxu/vista.vim', { 'on': 'Vista' }                                                 " Tags viewer
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }                                             " Undo tree
Plug 'justinmk/vim-dirvish', { 'on': ['Dirvish', 'LazyLoadPlugins'] }                          " File browser
Plug 'scrooloose/nerdtree', { 'on': [ 'NERDTree', 'NERDTreeFind', 'NERDTreeToggle' ] }         " Nerdtree
Plug 'meain/vim-automkdir'                                                                     " Automatically create parent dirs

" Morph code
Plug 'tpope/vim-surround', { 'on': 'LazyLoadPlugins' }                                         " Surround
Plug 'jiangmiao/auto-pairs'                                                                    " Autopair
Plug 'tomtom/tcomment_vim'                                                                     " Code commenting
Plug 'AndrewRadev/tagalong.vim', { 'on': 'LazyLoadPlugins' }                                   " Automatically change closing tag

" Language helpers
Plug 'elzr/vim-json', { 'for': 'json' }                                                        " Json syntax highlight
Plug 'chr4/nginx.vim', { 'for': 'nginx' }                                                      " Nginx config file syntax
Plug 'HerringtonDarkholme/yats.vim', { 'for': [ 'typescript', 'typescript.tsx' ] }             " Typescript syntax highlight
Plug 'peitalin/vim-jsx-typescript', { 'for': [ 'typescript', 'typescript.tsx' ] }              " Typescript JSX syntax highlight
Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx'] }                   " Inproved JSX syntax
Plug 'tpope/vim-markdown', { 'for': ['md', 'markdown'] }                                       " Better markdown support
Plug 'cespare/vim-toml', { 'for': 'toml' }                                                     " Toml highlight
Plug 'raimon49/requirements.txt.vim', { 'for': 'requirements' }                                " Requirements file
Plug 'jxnblk/vim-mdx-js', { 'for': 'mdx' }                                                     " MDX stuff

" Language enhacements
Plug '~/Documents/Projects/projects/vim-jsontogo' , { 'for': ['go'] }                          " Convert JSON to Go struct

" Linting / Checking
Plug 'w0rp/ale', { 'on': 'LazyLoadPlugins' }                                                   " Linter formatter and more

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }                                  " Competion framework
Plug 'Shougo/echodoc.vim', { 'on': 'LazyLoadPlugins' }                                         " Show signature
Plug 'Shougo/neco-vim', { 'for': 'vim' }                                                       " Completion for viml
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }           " Language client
Plug '~/Documents/Projects/projects/deoplete-notmuch', { 'for': 'mail' }                       " Email autocomplete

" Testing
Plug 'christoomey/vim-tmux-runner', { 'on': ['TestNearest', 'TestFile', 'VtrAttachToPane'] }   " Easy switching between vim and tmux
Plug 'janko/vim-test', { 'on': ['TestNearest', 'TestFile', 'TestLast', 'TestSuite'] }          " Quicky run test from vim

" Dependencies
Plug 'tpope/vim-repeat', { 'on': 'LazyLoadPlugins' }                                           " Repeat plugins with .
Plug 'radenling/vim-dispatch-neovim', { 'on': ['Dispatch', 'Start'] }                          " Neovim support for vim-dispatch

" Others
Plug 'dstein64/vim-startuptime', { 'on': 'StartupTime' }                                       " Measure startuptime
Plug 'rhysd/git-messenger.vim', { 'on': 'GitMessenger' }                                       " Show git commit
Plug 'kristijanhusak/vim-carbon-now-sh', { 'on': 'CarbonNowSh' }                               " CarbonNow
Plug 'davidbeckingsale/writegood.vim', { 'on': ['WritegoodEnable', 'WritegoodToggle'] }        " Better writing mode
Plug '~/Documents/Projects/projects/vim-colorswitch', { 'do': 'UpdateRemotePlugins' }          " Cycle between color types
Plug '~/Documents/Projects/projects/vim-package-info', { 'on': 'LazyLoadPlugins' }             " Show infor about packages
Plug '~/Documents/Projects/projects/vim-printer', { 'on': 'LazyLoadPlugins' }                  " Quick debug print
Plug '~/Documents/Projects/projects/vim-mtodo', { 'for': 'mtodo' }                             " Custom Todo

call plug#end()





"                           Editor Settings                            "
"                    ==============================                    "


" Set encoding to utf8
set encoding=utf8

" set scriptencoding
scriptencoding utf-8

" Make autocomplete for filenames work
set path+=**

" Turn on line numbers
set nonumber norelativenumber

" I don't need syntax
syntax off

" Highlight cursor line (slows down)
set nocursorline

" Show statusline
set laststatus=2

" Hidden startup messages
set shortmess=atIAc

" Auto read and write
set autowrite
set autoread

" Confirm before quit without save
set confirm

" Disable wrapping
set nowrap

" Set hidden
set hidden

" No newline when using $
set selection=old

" Always show signcolumn
set signcolumn=yes

" Better backup, swap and undos storage
set backup                        " make backup files
set undofile                      " persistent undos - undo after you re-open the file
set directory=~/.cache/vim/dirs/tmp     " directory to place swap files in
set backupdir=~/.cache/vim/dirs/backups " where to put backup files
set undodir=~/.cache/vim/dirs/undodir   " undo directory

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
set smartcase

" Better emoji support
set noemoji

" Do not put two spaces after .
set nojoinspaces

" show partial commands
set noshowcmd

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
set listchars=tab:\ \ ,eol:¬,trail:⋅,nbsp:ø

" Show line breaks
set showbreak=…

" When scrolling, keep cursor 5 lines away from screen border
set scrolloff=5

" Autocompletion of files and commands behaves like zsh
set wildmenu
set wildmode=full
set wildoptions-=pum

" Autocompletion setings
set completeopt+=noselect
set completeopt+=noinsert
set completeopt-=preview

" Setting up ignores
set wildignore+=*.so,*.pyc,*.png,*.jpg,*.gif,*.jpeg,*.ico,*.pdf
set wildignore+=*.wav,*.mp4,*.mp3
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*
set wildignore+=*.swp,*~,._*
set wildignore+=_pycache_,.DS_Store,.vscode,.localized
set wildignore+=.cache,node_modules,package-lock.json,yarn.lock,dist,.git
set wildignore+=.vimruncmd

" Netrw
let g:netrw_banner = 0

" Vimdiff
set diffopt+=vertical,context:3

" Setting up python
let g:python_host_prog = $PYENV_ROOT . '/versions/2.7.11/bin/python'
let g:python3_host_prog = $PYENV_ROOT . '/versions/3.6.6/bin/python'

" Fill chars
set fillchars=vert:\|
set fillchars+=fold:\ 

" Indent based folding
set foldlevel=0
set foldmethod=indent
" set foldmethod=manual
set foldignore=
set foldlevelstart=10
set foldnestmax=10

augroup colorscheme_github
  autocmd!
  autocmd ColorScheme github highlight Normal ctermbg=231 guibg=#ffffff
  autocmd ColorScheme github highlight SignColumn ctermbg=231 guibg=#ffffff
  autocmd ColorScheme github highlight LineNr ctermbg=240 guibg=#ffffff
  autocmd ColorScheme github highlight VertSplit guibg=#ffffff guifg=#444444 gui=NONE cterm=NONE
  autocmd ColorScheme github highlight StatusLineNC ctermbg=255 guibg=#cecece gui=NONE cterm=NONE
  autocmd ColorScheme github highlight SignifySignAdd cterm=bold gui=bold ctermfg=002 guifg=#008000 guibg=#ffffff
  autocmd ColorScheme github highlight SignifySignDelete cterm=bold gui=bold ctermfg=001 guifg=#800000 guibg=#ffffff
  autocmd ColorScheme github highlight SignifySignChange cterm=bold gui=bold ctermfg=003 guifg=#0087af guibg=#ffffff
  autocmd ColorScheme github highlight CursorLine ctermbg=255 guibg=#f5f5f5 gui=NONE cterm=NONE
  autocmd ColorScheme github highlight ALEErrorSign ctermfg=196 guifg=#ff0000 ctermbg=231 guibg=#ffffff
  autocmd ColorScheme github highlight ALEWarningSign ctermfg=226 guifg=#e75600 ctermbg=231 guibg=#ffffff
  autocmd ColorScheme github highlight ALEError ctermfg=052 guifg=#63120c
augroup end

augroup colorscheme_typewriter
  autocmd!
  autocmd ColorScheme typewriter highlight Normal ctermbg=231 guibg=#ffffff
  autocmd ColorScheme typewriter highlight SignColumn ctermbg=231 guibg=#ffffff
  autocmd ColorScheme typewriter highlight LineNr ctermbg=240 guibg=#ffffff
  autocmd ColorScheme typewriter highlight VertSplit guibg=#ffffff guifg=#444444 gui=NONE cterm=NONE
  autocmd ColorScheme typewriter highlight StatusLineNC ctermbg=255 guibg=#cecece gui=NONE cterm=NONE
  autocmd ColorScheme typewriter highlight SignifySignAdd cterm=bold gui=bold ctermfg=002 guifg=#008000 guibg=#ffffff
  autocmd ColorScheme typewriter highlight SignifySignDelete cterm=bold gui=bold ctermfg=001 guifg=#800000 guibg=#ffffff
  autocmd ColorScheme typewriter highlight SignifySignChange cterm=bold gui=bold ctermfg=003 guifg=#0087af guibg=#ffffff
  autocmd ColorScheme typewriter highlight CursorLine ctermbg=255 guibg=#f5f5f5 gui=NONE cterm=NONE
  autocmd ColorScheme typewriter highlight ALEErrorSign ctermfg=196 guifg=#ff0000 ctermbg=231 guibg=#ffffff
  autocmd ColorScheme typewriter highlight ALEWarningSign ctermfg=226 guifg=#e75600 ctermbg=231 guibg=#ffffff
  autocmd ColorScheme typewriter highlight ALEError ctermfg=052 guifg=#63120c
augroup end

augroup colorscheme_typewriter-night
  autocmd!
  autocmd ColorScheme typewriter-night highlight Normal ctermbg=233 guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight SignColumn ctermbg=233 guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight LineNr ctermbg=233 guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight VertSplit guibg=#1C1C1C guifg=#f5f5f5 gui=NONE cterm=NONE
  autocmd ColorScheme typewriter-night highlight StatusLineNC ctermbg=255 guibg=#1C1C1C gui=NONE cterm=NONE
  autocmd ColorScheme typewriter-night highlight SignifySignAdd cterm=bold gui=bold ctermfg=119 guifg=#87ff5f guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight SignifySignDelete cterm=bold gui=bold ctermfg=167 guifg=#d75f5f guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight SignifySignChange cterm=bold gui=bold ctermfg=227 guifg=#ffff5f guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight CursorLine ctermbg=255 guibg=#f5f5f5 gui=NONE cterm=NONE
  autocmd ColorScheme typewriter-night highlight ALEErrorSign ctermfg=196 guifg=#ff0000 ctermbg=231 guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight ALEWarningSign ctermfg=226 guifg=#e75600 ctermbg=231 guibg=#1C1C1C
  autocmd ColorScheme typewriter-night highlight ALEError ctermfg=052 guifg=#63120c
augroup end

" ColorScheme change ( janah )
augroup colorscheme_janah
  autocmd!
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
  autocmd ColorScheme janah highlight NonText ctermfg=238 guifg=#444444
augroup end

" ColorScheme change (pencil:light)
augroup colorscheme_pencil
  autocmd!
  autocmd ColorScheme pencil highlight SignifySignAdd    cterm=bold guifg=#00ff00
  autocmd ColorScheme pencil highlight SignifySignDelete cterm=bold guifg=#ff0000
  autocmd ColorScheme pencil highlight SignifySignChange cterm=bold guifg=#0000ff
augroup end

" Setting colorscheme
" janah, base16-greyscale, redblack, seoul256, gruvbox, grb256,
" tomorrow-night, zenburn, base16-classic-light, base16-classic-dark
" light:pencil
set termguicolors
if $DARK_MODE
  set background=dark
  colorscheme typewriter-night
else
  set background=light
  colorscheme typewriter
endif

" Use italics for some text
highlight htmlArg gui=italic
highlight htmlArg cterm=italic
highlight Comment gui=italic
highlight Comment cterm=italic
highlight Type    gui=italic
highlight Type    cterm=italic

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
highlight SpellBad cterm=underline gui=underline ctermfg=11 guifg=#ffaf00

" Don't you f'in touch my cursor
set guicursor=

" Set filetype for custom files
augroup custom_filetype
  autocmd!
  autocmd BufNewFile,BufRead *.zsh-theme set filetype=zsh
  autocmd BufNewFile,BufRead Dockerfile-* set filetype=dockerfile
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
autocmd BufWritePre *.css,*.html,*.cpp,*.c,*.java,*.go,*.rs,*.ts,*.cljs,*.clj :%s/\s\+$//e

" Trun of syntax hilighting if huge
autocmd BufReadPre * if getfsize(expand("%")) > 10000000 | syntax off | endif
autocmd BufReadPre *.min.* syntax off

" Hightlighted yank
augroup highlight_yank
  autocmd!
  autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank("IncSearch", 200)
augroup END




"                             Abbreviations                            "
"                    ==============================                    "

iabbr cosnt const
iabbr imprt import
iabbr imoprt import
iabbr improt import
iabbr consle console
augroup custom_iabbr_javascript
  autocmd!
  autocmd FileType javascript inoreab <buffer> fn function
  autocmd FileType javascript inoreab <buffer> cn className=
augroup end
augroup custom_iabbr_python
  autocmd!
  autocmd FileType python inoreab <buffer> im import
  autocmd FileType python inoreab <buffer> ipdb __import__('ipdb').set_trace()
  autocmd FileType python inoreab <buffer> pudb __import__('pudb').set_trace()
augroup end
augroup custom_iabbr_rust
  autocmd!
  autocmd FileType rust inoreab <buffer> xxx iter().map(\|x\|
augroup end




"                               inoremaps                              "
"                    ==============================                    "

" utils
function! ExpandSnip(snip)
  let line = getline('.')
  call setline('.', line . a:snip)
  redraw
  call inputsave()
  let expand_value = input('{++}: ')
  call inputrestore()
  call setline('.', line . substitute(a:snip, '{++}', expand_value, 'g'))
endfunction

" common
nnoremap ;; /<++><cr>:nohls<cr>cf>
inoremap ;; <esc>/<++><cr>:nohls<cr>cf>
vnoremap ;; <esc>/<++><cr>:nohls<cr>cf>

augroup custom_inormaps
  autocmd!

  " python
  autocmd FileType python inoremap <silent><buffer> ;def def <++>(<++>):<cr><++><esc>k$?def<cr>:nohls<cr>
  autocmd FileType python inoremap <silent><buffer> ;i __import__('ipdb').set_trace()<esc>
  autocmd FileType python inoremap <silent><buffer> ;dp __import__('pudb').set_trace()<esc>
  autocmd FileType python inoremap <silent><buffer> ;fs f""<left>
  autocmd FileType python inoremap <silent><buffer> ;pr print()<left>
  autocmd FileType python inoremap <silent><buffer> ;ld logger.debug(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;li logger.info(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;lw logger.warning(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;le logger.error(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;lc logger.critical(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;lx logger.exception(f"")<left><left>
  autocmd FileType python inoremap <silent><buffer> ;bang #!/usr/bin/env python3<esc>

  " rust
  autocmd FileType rust inoremap <silent><buffer> ;a println!("");<esc>hhi<c-r>=line(".")<cr><esc>
  autocmd FileType rust nnoremap <silent><buffer> ;a oprintln!("");<esc>hhi<c-r>=line(".")<cr><esc>

  " vim
  autocmd FileType vim inoremap <silent><buffer> ;ag augroup custom_<cr>autocmd!<cr><++><cr>augroup end<esc>?custom_<cr>:nohls<cr>A
  autocmd FileType vim inoremap <silent><buffer> ;af autocmd FileType inoremap <silent><buffer> <++><esc>?FileType<cr>ea<space>

  " shell
  autocmd FileType sh inoremap <silent><buffer> ;bang #!/bin/sh<cr><cr>
  autocmd FileType bash inoremap <silent><buffer> ;bang #!/bin/bash<cr><cr>
  autocmd FileType zsh inoremap <silent><buffer> ;bang #!/bin/zsh<cr><cr>
  autocmd FileType sh,bash,zsh inoremap <silent><buffer> ;s ;;
  autocmd BufNewFile *.sh :norm! i#!/bin/sh

  " gitcommit
  autocmd FileType gitcommit inoremap <silent><buffer> ;bang <++>: <++>

  " html
  autocmd FileType html inoremap <silent><buffer> ;bang <esc>:0read !cat ~/.config/datafiles/html_starter<cr>gg
  autocmd FileType html inoremap <silent><buffer> ;fa <i class="fas fa-"></i><esc>F"i
  autocmd FileType html inoremap <silent><buffer> ;css <link rel="stylesheet" href="index.css" type="text/css" /><esc>?index.css<cr>vi"
  autocmd FileType html inoremap <silent><buffer> ;js <script src="index.js"></script><esc>?index.js<cr>vi"
  autocmd FileType html inoremap <silent><buffer> ;cl class=""<left>

  " go
  autocmd FileType go inoremap <silent><buffer> ;ee :=
  autocmd FileType go inoremap <silent><buffer> ;err if err != nil {<cr>log.Fatal(err)<cr>}

  " css
  autocmd FileType css inoremap <silent><buffer> ;se <Esc>A;
  autocmd FileType css inoremap <silent><buffer> ;wh width: 100%;<cr>height: 100%;
  autocmd FileType css inoremap <silent><buffer> ;bc background-color: #f5f5f5;<esc>Bvt;
  autocmd FileType css inoremap <silent><buffer> ;bi background-image: url("");<esc>F"i./
  autocmd FileType css inoremap <silent><buffer> ;br border-radius: 5px;<esc>Bvt;
  autocmd FileType css inoremap <silent><buffer> ;media @media only screen and (max-width: 600px) {<cr>}<esc>O
  autocmd FileType css inoremap <silent><buffer> ;df display: flex;
  autocmd FileType css inoremap <silent><buffer> ;center justify-content: center;<cr>align-items: center;
  autocmd FileType css inoremap <silent><buffer> ;cp cursor: pointer;<esc>Bvt;
  autocmd FileType css inoremap <silent><buffer> ;base * {<cr>box-sizing: border-box;<cr>}
  autocmd FileType css inoremap <silent><buffer> ;body body {<cr>margin: 0;<cr>padding: 0;<cr>}
  autocmd FileType css inoremap <silent><buffer> ;shadow box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02), 0 0px 2px rgba(0, 0, 0, 0.12);
  autocmd FileType css inoremap <silent><buffer> ;fc flex-direction: column;
  autocmd FileType css inoremap <silent><buffer> ;ta text-align: center;<esc>hviw
  autocmd FileType css inoremap <silent><buffer> ;cl .<esc>"*pa{<cr>}<esc>O

  " javascript
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;flow // @flow 
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;cn className=""<left>
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;rf <React.Fragment><cr></React.Fragment><esc>O
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;rfs <React.Fragment>
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;rfe </React.Fragment>
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;con constructor(props: Props) {<cr>super(props)<cr>}O
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;ds dispatch({<cr>type: '<++>',<cr>payload: <++><cr>})<esc>?dispatch<cr>:nohls<cr>jci'
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;rl const <++> = React.lazy(() => import('<++>'))
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;bind <esc>:call ExpandSnip("self.{++} = self.{++}.bind(this)")<cr>
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;cim <esc>:call ExpandSnip("import {++} from '../{++}/{++}'")<cr>
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;stop if (e) {<cr>e.stopPropagaion()<cr>if(e.nativeEvent) e.nativeEvent.stopImmediatePropagation()<cr>}
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ;di <div className=""><++></div><esc>11hi
  autocmd FileType javascript,javascript.jsx inoremap <silent><buffer> ,. <></><left><left><left>

  " markdown
  autocmd FileType markdown inoremap <silent><buffer> ;lkj  <esc>Bi`<esc>Ea`<esc>A

  " others
  autocmd BufReadPost *.bin/mailsync inoremap <silent><buffer> ;t <esc>k"tyy"tp$hhdi'ifrom:<esc>"*p
augroup end





"                                Commands                              "
"                    ==============================                    "

" vertical help
ca h vert h

" Save as sudo
ca w!! w !sudo tee "%"

" I am too lazy to take my hands from shift
command! WQ wq
command! Wq wq
command! W w

command! GPush :normal! :Dispatch! git push origin $(git branch | grep "\*" | sed s:^..::g )<cr>





"                               Key remaps                             "
"                    ==============================                    "


" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Easier line navigatin
nnoremap H g^
nnoremap L g_

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

" Search
nnoremap n nzz
nnoremap N Nzz
nnoremap <silent><Leader>/ :nohls<cr>

" Terminal split jump
tnoremap <m-h> <C-\><C-N><C-w>h
tnoremap <m-j> <C-\><C-N><C-w>j
tnoremap <m-k> <C-\><C-N><C-w>k
tnoremap <m-l> <C-\><C-N><C-w>l

" Terminal mode esc remap
tnoremap <Leader><Esc> <C-\><C-n>

" Don't change Enter in all buffers
augroup custom_enter_remaps
  autocmd!
  autocmd FileType help nnoremap <buffer> <Enter> <Enter>
  autocmd FileType vim-plug nnoremap <buffer> <Enter> <Enter>
augroup end

" Go back and fwd smoother
nnoremap <silent>\ <c-o>
nnoremap <silent>~ <c-i>

" Split switching in insert mode
inoremap <silent> <m-h> <esc>:TmuxNavigateLeft<cr>
inoremap <silent> <m-j> <esc>:TmuxNavigateDown<cr>
inoremap <silent> <m-k> <esc>:TmuxNavigateUp<cr>
inoremap <silent> <m-l> <esc>:TmuxNavigateRight<cr>

" Visual star
vnoremap * "sy/<c-r>s<CR>

" Source current line
vnoremap <silent><leader>S y:execute @@<cr>
nnoremap <silent><leader>S ^vg_y:execute @@<cr>

" vv to select line
nnoremap vv ^vg_

" Quick make
nnoremap M :Dispatch! maker<cr>

" Terminal colors
let $NVIM_TUI_ENABLE_TRUE_COLOR=1





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <silent><leader><leader> :w<cr>

" Split like a boss
nnoremap <silent><Leader>v :vsplit<cr>
nnoremap <silent><Leader>h :split<cr>

" Quick fold and unfold
nnoremap <silent><Leader><esc> :normal!za<cr>

" Quick switch tabs
nnoremap <silent><Leader>k :tabn<cr>
nnoremap <silent><Leader>j :tabp<cr>
nnoremap <silent><s-right> :tabm +1<cr>
nnoremap <silent><s-left> :tabm -1<cr>

" Easier switching of quickfix list items
function! QfNext() abort
  let l:qflen = len(getqflist())
  if l:qflen > 0
    cnext
  else
    ALENext
  endif
endfunction
function! QfPrev() abort
  let l:qflen = len(getqflist())
  if l:qflen > 0
    cprevious
  else
    ALEPrevious
  endif
endfunction
nnoremap <silent><leader>m :call QfNext()<cr>
nnoremap <silent><leader>n :call QfPrev()<cr>

" Change from class to className
augroup custom_classtocalssname
  autocmd!
  autocmd FileType javascript nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
  autocmd FileType javascript.jsx nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
  autocmd FileType typescript nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
  autocmd FileType typescript.tsx nnoremap <buffer><leader>c mc?class=<CR>ciwclassName<ESC>`c4l
augroup end

" quick search and replace
nnoremap <leader>; :%s/\v
vnoremap <leader>; :s/\v

" quick edits
nnoremap sec :e ~/.dotfiles/nvim/.config/nvim/init.vim<cr>
nnoremap sez :e ~/.dotfiles/zsh/.config/zsh/.zshrc<cr>





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
  autocmd FileType gitcommit setlocal spell
  autocmd FileType help setlocal nospell
  autocmd FileType mail setlocal spell
augroup end
set complete+=kspell
function! FixLastSpellingError()
    normal! mz[s1z=`z
endfunction
nnoremap <silent>Z :call FixLastSpellingError()<cr>
inoremap <silent><c-z> :call FixLastSpellingError()<cr>

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
nnoremap <c-q> :call MinimizeIfZoomed() \|:mksession! /tmp/vim-prev \| :qa<cr>y

" Strip trailing whitespaces
function! StripTrailingWhitespace()
    if !&binary && &filetype !=# 'diff'
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
  execute '%!fixmalformedjson | jq'
  set filetype=json
endfunction
command! -range JSONFormat <line1>,<line2>call JSONFormat()

" Load shells
function! s:LoadShell(filname)
  execute '0read !cat '.a:filname
endfunction
augroup custom_shell_files
  autocmd!
  autocmd BufNewFile .gitignore :call s:LoadShell('~/.config/datafiles/sample_gitignore')
  autocmd BufNewFile index.html :call s:LoadShell('~/.config/datafiles/html_starter')
augroup end

" Auto add note header
function! s:AddNoteHeader()
  let l:note_title = substitute(substitute(trim(expand('%:t')), '.md$', '', ''), '_', ' ', 'g')
  call setline(1, '---')
  call setline(2, 'title: '.l:note_title)
  call setline(3, 'created: '.trim(system('date')))
  call setline(4, '---')
  call setline(5, '')
  call setline(6, '<++>')
  normal! ggjf:llv$
endfunction
augroup custom_note_header
  autocmd!
  autocmd BufNewFile ~/.notes/* :call <SID>AddNoteHeader()
augroup end

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
  if previous_last_char =~# '(\|[\' && current_char ==# ' '
    normal! x
  endif
endfunction
nnoremap <silent>J :call Join()<CR>

function! s:OpenLastFile()
  let last = filter(copy(v:oldfiles), 'filereadable(v:val)')
  if !empty(last)
    execute 'edit' fnameescape(last[0])
  endif
endfunction
command! OpenLastFile :call s:OpenLastFile()

" Search for css defenittion
function! s:CSSSearchForClassDef()
  setlocal iskeyword+=-
  let word = expand('<cword>')
  setlocal iskeyword-=-
  execute ':Find .' . word
endfunction
command! CSSSearchForClassDef :call s:CSSSearchForClassDef()

function! s:QuickFixPopulate(word)
  cex system('rg --vimgrep '.a:word)
  redraw!
  cl
endfunction
command! -nargs=1 QuickFixPopulate :call s:QuickFixPopulate(<f-args>)
nnoremap <leader>F :QuickFixPopulate<space>

" Create a floating buffer
function! Floater(...)
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')
  if a:0 == 4
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
endfunction


let g:term_buf = 0
let g:term_job_id = 0
function! FloatTerm(...)
  if g:term_buf == bufnr('')
    setlocal bufhidden=hide
    close
  else
    let height=float2nr(0.7*&lines)
    let width=float2nr(0.8*&columns)
    let horizontal = float2nr((&columns - width) / 2)
    let vertical = float2nr((&lines - height) / 2)

    " Terminal Drawer (Option 1)
    botright new __fterm__
    exec 'resize' float2nr(&lines * 0.25)
    " setlocal laststatus=0 noshowmode noruler
    " setlocal nobuflisted

    " Floating Terminal (Option 2)
    " call Floater(height, width, horizontal, vertical)

    try
      exec 'buffer '.g:term_buf
    catch
      terminal
      let g:term_buf = bufnr('')
      let g:term_job_id = b:terminal_job_id
      augroup custom_floatterm
        autocmd!
        autocmd TermClose * ++once :bd! | let g:term_buf = 0
        autocmd BufEnter <buffer> if (winnr('$') == 1 && bufnr('') == g:term_buf) | q | endif
      augroup end
    endtry
    startinsert!
    if a:0 != 0
      call jobsend(g:term_job_id, a:1."\n")
    endif
  endif
endfunction
nnoremap <silent><m-t> <C-\><C-n>:call FloatTerm()<cr>
inoremap <silent><m-t> <C-\><C-n>:call FloatTerm()<cr>
tnoremap <silent><m-t> <C-\><C-n>:call FloatTerm()<cr>
nnoremap <silent><m-r> <C-\><C-n>:call FloatTerm("vimruncmd")<cr>
inoremap <silent><m-r> <C-\><C-n>:call FloatTerm("vimruncmd")<cr>
tnoremap <silent><m-r> <C-\><C-n>:call FloatTerm("vimruncmd")<cr>

" Quick edit .vimruncmd
let g:vimruncmd_editor_open = 0
let g:vimruncmd_buffer_number = 0
function! ToggleVimRunEditor()
  if g:vimruncmd_editor_open == 1
    if g:vimruncmd_buffer_number == bufnr('')
      w
      q
    else
      exec g:vimruncmd_buffer_number.'bd'  
    endif
    let g:vimruncmd_editor_open = 0
    return
  endif
  topleft new .vimruncmd
  resize 5
  let g:vimruncmd_editor_open = 1
  let g:vimruncmd_buffer_number =  bufnr('')
endfunction
nnoremap <silent><leader>R :call ToggleVimRunEditor()<cr>
augroup custom_vimruncmd
  autocmd!
  autocmd BufEnter .vimruncmd if (winnr("$") == 1) | q | endif
  autocmd BufNewFile .vimruncmd :w | :!chmod +x %
  autocmd BufNewFile,BufRead .vimruncmd set filetype=sh
augroup end


function! RrgOpen()
  let l:filename = matchstr(getline('.'), '\v^[^:]*:[^:]*:[^:]*')
  let l:items = split(l:filename, ':')
  execute "normal \<c-w>\<c-w>"
  exec 'e +'. l:items[1].' '.l:items[0]
  exec 'normal 'l:items[2].'|'
endfunction

function! Rrg(term)
  " Not using quickfix list because ale always rewrites qf
  botright new
  resize 5
  exec '0read!rg --vimgrep ' . a:term
  normal! gg
  set filetype=ripgrep
  setlocal buftype=nofile
  setlocal nowrap
  nnoremap <buffer><enter> :call RrgOpen()<cr>
endfunction
command! -nargs=1 Rrg :call Rrg(<f-args>)

function! GHOpen() abort
    let l:git_origin = system('git config --get remote.origin.url')
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
endfunction
command! -range GhOpen :call GHOpen()

" Startpage
function! StartPage(force)
  if !(argc() == 0 && &filetype ==# '') && !a:force
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
if argc() == 0
  augroup custom_startpage
    autocmd!
    autocmd VimEnter * call StartPage(v:false)
  augroup end
endif
nnoremap <silent>,l :call StartPage(v:true)<cr>

" Markdown preview
function! s:MarkdownPreview()
  call jobstart('pandocmarkdownpreview '.expand('%'))
  echo 'Generating preview...'
endfunction
nnoremap <silent><leader>M :call <SID>MarkdownPreview()<CR>


" Stratr profiling
function! Profile()
  profile start profile.log
  profile func *
  profile file *
endfunction





"                            Plugin settings                           "
"                    ==============================                    "

" Fzf fuzzy search
let $FZF_DEFAULT_COMMAND = 'fd --type f --hidden --follow'
let $FZF_DEFAULT_OPTS=' --layout=reverse --margin=0,0,1,0'
let g:fzf_tags_command = 'ctags -R'
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
let g:fzf_colors =
      \ { 'fg':      ['fg', 'Comment'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Normal'],
      \ 'fg+':     ['fg', 'Identifier', 'Normal', 'Normal'],
      \ 'bg+':     ['bg', 'Normal', 'Normal'],
      \ 'hl+':     ['fg', 'Type'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'WildMenu'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }
" let g:fzf_layout = { 'window': "call Floater(winheight('%')/2, winwidth('%')/4*3)" }
let g:fzf_layout = { 'window': 'call Floater()' }
command! -bang History call fzf#vim#history({'options': ['--query', '!.git/ !.vim/ ', '--no-sort', '--preview', 'codepreview {}']}, <bang>0)
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -bang Open call fzf#run({'source': 'rg --files --hidden --follow --glob "!.git/*"', 'sink': 'e', 'down': '40%', 'options': '--preview "codepreview {}"'})
nnoremap <silent><Enter> :FZF<cr>
nnoremap <silent> <leader><Enter> :History<cr>
command! -bang -nargs=* Find
      \ call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>),
      \ 1, <bang>0)
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Find<cr>
augroup custom_fzf
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup end

" Fugitive
nnoremap <silent><leader>g :Gstatus\|normal!gg7j<cr>
augroup custom_fugitive
  autocmd! FileType fugitive nnoremap <silent>1 :norm V-<cr>
augroup end

" GitMessenger
nnoremap <silent><leader>G :GitMessenger<cr>

" Drag Visuals
let g:Schlepp#reindent = 1
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight
vmap D <Plug>SchleppDup

" Ale
nnoremap <silent>,, :ALEFix<cr>
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '⚠'
let g:ale_virtualtext_cursor = 1
let g:ale_virtualtext_prefix = ' ¤ '
highlight clear ALEWarning
let g:ale_statusline_format = ['✖ %d', '⚠ %d', '⬥ ok']
let g:ale_linters = {
\  'rust': [],
\  'javascript': ['eslint'],
\  'typescript' : ['eslint'],
\  'typescript.tsx' : ['tslint', 'eslint'],
\  'python': ['pycodestyle', 'mypy', 'bandit'],
\  'html': ['tidy'],
\  'css': ['stylelint'],
\  'bash': ['shellcheck'],
\  'zsh': ['shellcheck'],
\  'vim': ['vint'],
\}
let g:ale_virtualenv_dir_names = [$WORKON_HOME]
let g:ale_echo_msg_error_str = '✖'
let g:ale_echo_msg_warning_str = '⚠'
let g:ale_echo_msg_format = 'ALE: %severity% %linter%: %s '
let g:ale_lint_on_enter = 0
let g:ale_fixers = {
\  'javascript' : ['prettier'],
\  'typescript' : ['prettier'],
\  'typescript.tsx' : ['prettier'],
\  'json' : ['fixjson'],
\  'python' : ['black', 'isort'],
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

" Signfy
let g:signify_sign_add = 'ǁ'
let g:signify_sign_delete = 'ǁ'
let g:signify_sign_change = 'ǁ'

" Sleuth auto indent
let g:sleuth_automatic = 1

" Vim-json
let g:vim_json_syntax_conceal = 0

" Markdown
let g:markdown_syntax_conceal = 0
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'json', 'javascript', 'css']

" Gentags
let g:gen_tags#blacklist = ['$HOME']

" Vim Router
let g:rooter_use_lcd = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
let g:rooter_patterns = ['Rakefile', 'Makefile', '.git/', '.vscode']

" JSX Typescript
augroup custom_jsx
  autocmd!
  autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx
augroup end
hi xmlTagName guifg=#87d787 ctermfg=114
hi xmlTag guifg=#87d787 ctermfg=114
hi xmlEndTag guifg=#87d787 ctermfg=114

" Other art
augroup other_art
  au FileType javascript,typescript nnoremap <silent><leader>a :Dispatch! npm run build<cr>
  au FileType javascript,typescript nnoremap <silent><leader>r :Dispatch npm start<cr>

  au FileType rust nnoremap <silent><leader>a :Dispatch! cargo build<cr>
  au FileType rust nnoremap <silent><leader>r :Start cargo run<cr>
augroup end

" Emmet
let user_emmet_expandabbr_key = '<m-i>'
let g:user_emmet_settings = {
\  'javascript.jsx' : {
\      'extends' : 'jsx',
\  },
\}

" SuperTab
let g:SuperTabDefaultCompletionType = '<c-n>'

" Language server protocol
let g:LanguageClient_autoStart = 1
let g:LanguageClient_useVirtualText = 'Diagnostics'
let g:LanguageClient_virtualTextPrefix = ' ¤ '
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rust-analyzer'],
    \ 'javascript': ['/usr/local/bin/javascript-typescript-stdio', 'lsp'],
    \ 'javascript.jsx': ['/usr/local/bin/javascript-typescript-stdio', 'lsp'],
    \ 'typescript': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'typescript.tsx': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'go': ['go-langserver'],
    \ 'css': ['css-languageserver', '--stdio'],
    \ 'python': ['pyls'],
    \ 'sh': ['bash-language-server', 'start'],
    \ 'zsh': ['bash-language-server', 'start'],
    \ 'dockerfile': ['docker-langserver', '--stdio'],
    \ 'lua': ['lua-lsp'],
    \ }
let g:LanguageClient_diagnosticsDisplay = {
      \1: {
        \'name': 'Error',
        \'texthl': 'ALEError',
        \'signText': '✖',
        \'signTexthl': 'ALEErrorSign',
        \'virtualTexthl': 'ALEErrorSign',
      \},
      \2: {
        \'name': 'Warning',
        \'texthl': 'ALEWarning',
        \'signText': '⚠',
        \'signTexthl': 'ALEWarningSign',
        \'virtualTexthl': 'ALEWarningSign',
      \},
      \3: {
        \'name': 'Information',
        \'texthl': 'ALEInfo',
        \'signText': '∴',
        \'signTexthl': 'ALEInfoSign',
        \'virtualTexthl': 'ALEInfo',
      \},
      \4: {
        \'name': 'Hint',
        \'texthl': 'ALEInfo',
        \'signText': '➤',
        \'signTexthl': 'ALEInfoSign',
        \'virtualTexthl': 'ALEInfo',
      \},
      \}
nnoremap <silent><leader>: :call LanguageClient_contextMenu()<CR>
nnoremap <silent> gx :call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>
nnoremap <silent> gh :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> ga :call LanguageClient_textDocument_codeAction()<CR>
nnoremap <silent> g, :call LanguageClient_textDocument_formatting()<CR>
vnoremap <silent> g, :call LanguageClient_textDocument_rangeFormatting()<CR>
nnoremap <silent><leader>l :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> <leader>I :call LanguageClient#explainErrorAtPoint()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call LanguageClient#textDocument_hover()
  endif
endfunction

" Use deoplete.
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('max_list', 20)
call deoplete#custom#option('smart_case', v:true)

" Echodoc
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'virtual'

" Indentline
let g:indentLine_color_term = 236
let g:indentLine_color_gui = '#303030'

" AutoPair
let g:AutoPairsShortcutToggle='<c-p>'
let g:AutoPairsMultilineClose = 0

" Vim Plug
command! LoadAllPlugins call plug#load(keys(g:plugs))

" Vim instant markdown
let g:instant_markdown_autostart = 0

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

" Color Swap
augroup custom_css
  autocmd!
  autocmd FileType css nnoremap <buffer><leader>c :ColorSwap<CR>
augroup end

" Vim package info
let g:vim_package_info_virutaltext_prefix = '  ¤ '
" let g:vim_package_info_virutaltext_highlight = 'WildMenu'

" Vim printer
let g:vim_printer_print_below_keybinding = '<leader>p'
let g:vim_printer_print_above_keybinding = '<leader>P'
let g:vim_printer_items = {
      \ 'typescript.tsx': 'console.log("{$}:", {$})',
      \ 'sh': 'echo "{$}: ${$}"',
      \ 'bash': 'echo "{$}: ${$}"',
      \ 'zsh': 'echo "{$}: ${$}"',
      \ }


" Vista
nnoremap <silent><leader>L :Vista!!<cr>
augroup custom_vista
  " autocmd VimEnter * call vista#RunForNearestMethodOrFunction()
  autocmd FileType markdown nnoremap <silent><buffer><leader>l :Vista toc<cr>
augroup end
let g:vista_default_executive = 'lcn'
let g:vista_echo_cursor_strategy='floating_win'
let g:vista_close_on_jump=0
let g:vista_echo_cursor=1
let g:vista_cursor_delay=100
let g:vista_blink=[3,20]
let g:vista#renderer#enable_icon = 0
let g:vista_icon_indent = ['▸ ', '']
let g:vista_executive_for = {
  \ 'markdown': 'ctags',
  \ }
let g:vista#renderer#icons = {
\   'function': 'f',
\   'variable': 'v',
\  }

" Colorizer
lua << EOF
require 'colorizer'.setup ({
  'markdown';
  css = { names = true; };
  javascript = { names = true; };
  html = { names = true; };
}, {
  names = false;
  rgb_fn = true;
  hsl_fn = true;
})
EOF

" Tag Along
let g:tagalong_additional_filetypes = ['javascript', 'typescript', 'javascript.jsx', 'typescript.tsx', 'html', 'xml']

" Searcher
nnoremap <silent><leader>s :Searcher <c-r><c-w><cr>
vnoremap <silent><leader>s y:Searcher <c-r>"<cr>
vnoremap <silent><leader>S c<C-R>=SearcherMarkdownAutoLinkGenerate(getreg('"'))<cr><esc>
nnoremap <silent><leader>S viwc<C-R>=SearcherMarkdownAutoLinkGenerate(getreg('"'))<cr><esc>

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

" Dirvish
let g:loaded_netrwPlugin = 'v165'  " Prevent loading netrw
let g:dirvish_relative_paths = 1  " Needed with syntax off as conceal gets disabled
nnoremap <silent>- :Dirvish %<cr>

" vim-test
" maybe try creating a new strategy for running tests in a floating terminal
let test#strategy = 'vtr'
nnoremap <silent>sn :TestNearest<cr>
nnoremap <silent><leader>tn :TestNearest<cr>
nnoremap <silent><leader>tf :TestFile<cr>
nnoremap <silent><leader>tl :TestLast<cr>

" VTR
function! VtrCustomCommand()
  VtrAttachToPane 2
  VtrSendCommandToRunner runner
  nnoremap <silent>sf :VtrSendCommandToRunner<cr>
endfunction
nnoremap <silent>sf :call VtrCustomCommand()<cr>

" CarbonNowSh
let g:carbon_now_sh_options =
  \ {'bg':'rgba(187,187,187,1)',
  \ 't':'solarized light',
  \ 'wt':'sharp',
  \ 'ds':'true',
  \ 'dsyoff':'3px',
  \ 'dsblur':'13px',
  \ 'wc':'false',
  \ 'wa':'true',
  \ 'pv':'58px',
  \ 'ph':'56px',
  \ 'ln':'false',
  \ 'fl':'1',
  \ 'fm':'Inconsolata',
  \ 'fs':'16px',
  \ 'lh':'122%25',
  \ 'si':'false',
  \ 'es':'2x',
  \ 'wm':'false' }

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
augroup custom_goyo
  autocmd!
  autocmd! User GoyoEnter nested call <SID>goyo_enter()
  autocmd! User GoyoLeave nested call <SID>goyo_leave()
augroup end

"                             Source External                          "
"                    ==============================                    "

" File manipulations
augroup load_file_manip
    autocmd!
    autocmd CursorHold,CursorHoldI * source ~/.config/nvim/filemanip.vim | autocmd! load_file_manip
augroup end

" Git stuff
augroup load_git_utils
    autocmd!
    autocmd CursorHold,CursorHoldI * source ~/.config/nvim/gitutils.vim | autocmd! load_git_utils
augroup end

augroup load_additional_plugins_onwrite
    autocmd!
    autocmd CursorHold,CursorHoldI * call plug#load('vim-signify') |
          \ call plug#load('vim-package-info') | 
          \ call plug#load('vim-signify') |
          \ call plug#load('vim-googler') |
          \ call plug#load('vim-dirvish') |
          \ call plug#load('vim-surround') |
          \ call plug#load('auto-pairs') |
          \ call plug#load('tagalong.vim') |
          \ call plug#load('ale') |
          \ call plug#load('echodoc.vim') |
          \ call plug#load('vim-repeat') |
          \ call plug#load('vim-package-info') |
          \ call plug#load('vim-printer') |
          \ autocmd! load_additional_plugins_onwrite
augroup end

augroup load_additional_plugins
    autocmd!
    autocmd BufWritePost * call plug#load('indentLine') |
          \ call plug#load('vim-package-info') | 
          \ call plug#load('vim-signify') |
          \ call plug#load('vim-googler') |
          \ call plug#load('vim-dirvish') |
          \ call plug#load('vim-surround') |
          \ call plug#load('auto-pairs') |
          \ call plug#load('tagalong.vim') |
          \ call plug#load('ale') |
          \ call plug#load('echodoc.vim') |
          \ call plug#load('vim-repeat') |
          \ call plug#load('vim-package-info') |
          \ call plug#load('vim-printer') |
          \ autocmd! load_additional_plugins
augroup end

" Statusline
source ~/.config/nvim/statusline.vim
