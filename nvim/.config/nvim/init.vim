"                            Plugin Management                         "
"                    ==============================                    "

let g:javascript_filetypes = ['javascript', 'javascript.jsx', 'typescript', 'typescript.tsx']
let g:schlepp_triggers = ['<Plug>SchleppUp', '<Plug>SchleppDown', '<Plug>SchleppLeft', '<Plug>SchleppRight', '<Plug>SchleppDup']
call plug#begin('~/.local/share/nvim/plugged')

" Visual enhancements
Plug '~/Documents/Projects/projects/hima-vim'                                                  " Minmal light colorschme
Plug 'Yggdroot/indentLine'                                                                     " Show indent
Plug 'airblade/vim-gitgutter', { 'on': 'LazyLoadPlugins' }                                     " Git diff icons in gutter
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }                                           " Hyper focus editing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }                                                     " Centerify
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }                                      " Highlight color values

" Added functinality
Plug '~/Documents/Projects/others/done/_vim/vim-googler', { 'on': ['LazyLoadPlugins', 'Searcher'] } " Search from within vim
Plug '/usr/local/opt/fzf'                                                                      " Fzf
Plug 'junegunn/fzf.vim'                                                                        " Fzf for vim
Plug 'christoomey/vim-tmux-navigator'                                                          " Seamless navigation between vim and tmux
Plug 'suan/vim-instant-markdown', { 'for': [ 'md', 'markdown' ] }                              " View markdown in browser while editing
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
Plug 'justinmk/vim-dirvish'                                                                    " File browser
Plug 'scrooloose/nerdtree', { 'on': [ 'NERDTree', 'NERDTreeFind', 'NERDTreeToggle' ] }         " Nerdtree
Plug 'meain/vim-automkdir'                                                                     " Automatically create parent dirs
Plug 'nvim-treesitter/nvim-treesitter'                                                         " Treesitter integration and highlight
Plug 'nvim-treesitter/playground', { 'on': 'TSPlaygroundToggle' }                              " Treesitter debugger

" Morph code
Plug 'tpope/vim-surround'                                                                      " Surround
Plug 'tmsvg/pear-tree'                                                                         " Autopair
Plug 'tomtom/tcomment_vim'                                                                     " Code commenting

" Language helpers
Plug 'elzr/vim-json', { 'for': 'json' }                                                        " Json syntax highlight
Plug 'chr4/nginx.vim', { 'for': 'nginx' }                                                      " Nginx config file syntax
Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'jinja' }                                            " Jinja templates syntax highlight
Plug 'HerringtonDarkholme/yats.vim', { 'for': [ 'typescript', 'typescript.tsx' ] }             " Typescript syntax highlight
Plug 'peitalin/vim-jsx-typescript', { 'for': [ 'typescript', 'typescript.tsx' ] }              " Typescript JSX syntax highlight
Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx'] }                   " Inproved JSX syntax
Plug 'tpope/vim-markdown', { 'for': ['md', 'markdown'] }                                       " Better markdown support
Plug 'cespare/vim-toml', { 'for': 'toml' }                                                     " Toml highlight
Plug 'raimon49/requirements.txt.vim', { 'for': 'requirements' }                                " Requirements file
Plug 'jxnblk/vim-mdx-js', { 'for': 'mdx' }                                                     " MDX stuff
Plug 'pechorin/any-jump.vim'

" Language enhacements
Plug '~/Documents/Projects/projects/vim-jsontogo' , { 'for': ['go'] }                          " Convert JSON to Go struct

" Linting / Checking
Plug 'w0rp/ale'                                                                                " Linter formatter and more

" Autocomplete
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
" Plug 'nvim-lua/diagnostic-nvim'
Plug 'nvim-lua/lsp-status.nvim'
Plug 'steelsojka/completion-buffers'
Plug 'nvim-treesitter/completion-treesitter'
Plug 'Shougo/echodoc.vim', { 'on': 'LazyLoadPlugins' }                                         " Show signature

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

" I do need syntax
syntax on

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

" Disable hard wrapping
set textwidth=0 
set wrapmargin=0

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
set updatetime=500

" Tabs config
set expandtab
set smarttab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround

" Don't show --INSERT-- at bottom
set noshowmode

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
set completeopt+=menuone
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
set foldignore=
set foldlevelstart=10
set foldnestmax=10

" Use rg for grep
if executable('rg')
  set grepprg=rg\ --vimgrep
endif

" Setting colorscheme
" janah, base16-greyscale, redblack, seoul256, gruvbox, grb256,
" tomorrow-night, zenburn, base16-classic-light, base16-classic-dark
" light:pencil
set termguicolors

let g:light_or_dark = ""
function! SetColorscheme(...)
  let l:light_or_dark = trim(system('setdarkmode query'))
  if l:light_or_dark ==? g:light_or_dark
    return
  endif
  if l:light_or_dark ==? 'dark'
    set background=dark
    colorscheme hima-dark
  else
    set background=light
    colorscheme hima
  endif
  let g:light_or_dark = l:light_or_dark
endfunction
call SetColorscheme()
" let timer = timer_start(2000, 'SetColorscheme',{'repeat':-1})

" Highlight git conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Set up leader keys
let mapleader = ' '
let maplocalleader = ','

" Smart colorcolumn
function SmartColorcolumn() abort
  if &buftype !=# 'terminal'
    call matchadd('ColorColumn', '\%160v', 100)
  endif
endfunction
augroup custom_colorcolum
  autocmd!
  autocmd BufEnter * call SmartColorcolumn()
augroup end

" Don't you f'in touch my cursor
set guicursor=

" Set filetype for custom files
augroup custom_filetype
  autocmd!
  autocmd BufNewFile,BufRead *.zsh-theme set filetype=zsh
  autocmd BufNewFile,BufRead Dockerfile-* set filetype=dockerfile
  autocmd BufNewFile,BufRead *.jsql set filetype=jinja
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
  autocmd TextYankPost * silent! lua vim.highlight.on_yank({higroup="IncSearch", timeout=200})
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

" Fixed I/A for visual
xnoremap <expr> I mode() ==# 'v' ? "\<c-v>I" : mode() ==# 'V' ? "\<c-v>^o^I" : "I"
xnoremap <expr> A mode() ==# 'v' ? "\<c-v>A" : mode() ==# 'V' ? "\<c-v>Oo$A" : "A"




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
  if len(getqflist()) != 0
    try
      cnext
    catch
      echo "Reached last item"
    endtry
  else
    ALENext
  endif
endfunction
function! QfPrev() abort
  if len(getqflist()) != 0
    try
      cprevious
    catch
      echo "Reached first item"
    endtry
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

augroup custom_mail_config
  autocmd!
  autocmd FileType mail setlocal wrap
  autocmd FileType mail setlocal textwidth=0 
  autocmd FileType mail setlocal wrapmargin=0
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
nnoremap <silent> <leader>i :call functions#QuickfixToggle()<cr>

" Zoom in and out of windows
command! ZoomToggle call functions#ZoomToggle()
nnoremap <silent> `` :ZoomToggle<CR> \| :normal! 0<cr>

" Save session and quit all buffers (for use with viml command)
nnoremap <c-q> :call functions#MinimizeIfZoomed() \|:mksession! /tmp/vim-prev \| :qa<cr>y

" Strip trailing whitespaces
command! StripTrailingWhitespace :call functions#StripTrailingWhitespace()

" Better marks
command! Marks call functions#Marks()
nnoremap <silent>_ :call functions#Marks()<cr>

" Json format
command! -range JSONFormat <line1>,<line2>call functions#JSONFormat()

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
augroup custom_note_header
  autocmd!
  autocmd BufNewFile ~/.notes/* :call functions#AddNoteHeader()
augroup end

" Close unused buffers
command! Cleanup call functions#CloseHiddenBuffers()

" Find highlight group of char under the cursor
nnoremap <silent><leader>gh :call functions#ShowHightlightGroup()<cr>

" Remove space after bracket on joining
nnoremap <silent>J :call functions#Join()<CR>

" For use with vv
command! OpenLastFile :call functions#OpenLastFile()

" Search for css defenittion
command! CSSSearchForClassDef :call functions#CSSSearchForClassDef()

" Github link copy
command! -range GhOpen :call functions#GHOpen(v:true)
command! -range GithubOpen :call functions#GHOpen(v:true)
command! -range OpenInGithub :call functions#GHOpen(v:true)
command! -range GhCopy :call functions#GHOpen(v:false)

" Startpage
if argc() == 0
  augroup custom_startpage
    autocmd!
    autocmd VimEnter * call functions#StartPage(v:false)
  augroup end
endif
nnoremap <silent>,l :call functions#StartPage(v:true)<cr>

" Markdown preview
command! MarkdownPreview :call functions#MarkdownPreview()<CR>

" Copy all matching string
command! -register CopyMatches call functions#CopyMatches(<q-reg>)


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
      \ 'fg+':     ['fg', 'Normal'],
      \ 'bg+':     ['bg', 'Normal'],
      \ 'hl+':     ['fg', 'Type'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'WildMenu'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }
" let g:fzf_layout = { 'window': "call functions#Floater(winheight('%')/2, winwidth('%')/4*3)" }
let g:fzf_layout = { 'window': 'call functions#Floater()' }
command! -bang History call fzf#vim#history({'options': ['--query', '!.git/ !.vim/ ', '--no-sort', '--preview', 'codepreview {}']}, <bang>0)
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -bang Open call fzf#run({'source': 'rg --files --hidden --follow --glob "!.git/*"', 'sink': 'e', 'down': '40%', 'options': '--preview "codepreview {}"'})
nnoremap <silent><Enter> :FZF<cr>
nnoremap <silent> <leader><Enter> :History<cr>
command! -bang -range -nargs=* Find
      \ call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>),
      \ 1, <bang>0)
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Find<cr>
vnoremap <leader>f :Find <c-r><c-w><cr>
function s:OpenInBuffer(lines) abort
  execute 'e +'.split(a:lines[0], ':')[1].' '.split(a:lines[0], ':')[0]
endfunction
command! -bang -nargs=* FindInProject call fzf#run({'source': 'rg --vimgrep --hidden --follow --glob "!.git/*" '.shellescape(<q-args>), 'sink*': function('s:OpenInBuffer'),
      \ 'window': 'call functions#Floater(&lines - 10, &columns - 10, 5, 5, 10)',
      \ 'options': ["--delimiter", ":", "--preview", "bat --color always --theme GitHub {1} --highlight-line {2}","--preview-window", "+{2}-".winheight('%')/3,
      \ '--color', 'fg:-1,hl:4,fg+:1,bg+:-1,hl+:4,info:108,prompt:242,spinner:108,pointer:1,marker:168']})
augroup custom_fzf
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup end
nnoremap <leader>F :FindInProject<space>
vnoremap <leader>F :FindInProject <c-r><c-w>

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
\  'sh': ['shfmt'],
\  'dart': ['dartfmt']
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

" gitgutter
let g:gitgutter_map_keys = 0
nmap ghn <Plug>(GitGutterNextHunk)
nmap ghp <Plug>(GitGutterPrevHunk)
nmap ghs <Plug>(GitGutterStageHunk)
nmap ghu <Plug>(GitGutterUndoHunk)
let g:gitgutter_sign_added = '|'
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_removed = '|'
let g:gitgutter_sign_removed_first_line = '^'
let g:gitgutter_sign_removed_above_and_below = '{'
let g:gitgutter_sign_modified_removed = 'w'

" Sleuth auto indent
let g:sleuth_automatic = 1

" Vim-json
let g:vim_json_syntax_conceal = 0

" Markdown
let g:markdown_syntax_conceal = 0
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'json', 'javascript', 'css']

" Vim Router
let g:rooter_cd_cmd="lcd"
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

" Echodoc
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'virtual'

" Indentline
let g:indentLine_color_term = 236
let g:indentLine_color_gui = '#303030'

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
      \ 'html': 'console.log("{$}:", {$})',
      \ 'sh': 'echo "{$}: ${$}"',
      \ 'bash': 'echo "{$}: ${$}"',
      \ 'zsh': 'echo "{$}: ${$}"',
      \ 'lua': 'print("{$}:", {$})'
      \ }


" Vista
nnoremap <silent><leader>L :Vista!!<cr>
augroup custom_vista
  autocmd FileType markdown nnoremap <silent><buffer><leader>L :Vista toc<cr>
augroup end
let g:vista_default_executive = 'nvim_lsp'
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

" Color hex codes
let g:Hexokinase_highlighters = ['backgroundfull']

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
let g:dirvish_relative_paths = 0  " Needed with syntax off as conceal gets disabled
nnoremap <silent>- :Dirvish %<cr>
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) | exe 'Dirvish' argv()[0] | endif

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

" Anyjump
let g:any_jump_disable_default_keybindings = 1
nnoremap <leader>J :AnyJump<CR>
xnoremap <leader>J :AnyJumpVisual<CR>

" vim-mtodo
highlight default link MTODOTitle SpecialKey
highlight default link MTODOPending Normal
highlight default link MTODODone Comment
highlight default link MTODOImportant Question
let g:vim_mtodo_move_done_to_bottom=1

" nvim-treesitter
nnoremap <leader>ts :write <bar> edit <bar> TSBufEnable highlight<cr>

" lsp-config
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> g,    <cmd>lua vim.lsp.buf.formatting(nil, 1000)<CR>
vnoremap <silent> g,    <cmd>lua vim.lsp.buf.range_formatting()<CR>
nnoremap <silent> ga    <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> gi    <cmd>lua print(require'custom_functions'.get_current_diagnostics())<CR>
" autocmd CursorMoved <buffer> <cmd>lua print(require'custom_functions'.get_current_diagnostics())<CR>
command! LanguageclientStop <cmd>lua vim.lsp.stop_client(vim.lsp.get_active_clients())<cr>


" nvim-completions
autocmd BufEnter * lua require'completion'.on_attach()
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <silent><expr> <c-l> completion#trigger_completion()
lua require'completion'.addCompletionSource('email', require'emailcomplete'.complete_item)
let g:completion_chain_complete_list = {
    \'default' : [
    \    {'complete_items': ['lsp', 'ts', 'path', 'buffers']},
    \    {'mode': '<c-p>'},
    \    {'mode': '<c-n>'}
    \],
    \'mail' : [
    \    {'complete_items': ['email', 'path', 'buffers']},
    \    {'mode': '<c-p>'},
    \    {'mode': '<c-n>'}
    \]
    \}

" diagnostics
let g:diagnostic_enable_virtual_text = 0
nnoremap <silent>sm <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
nnoremap <silent>sn <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>

" status
augroup custom_lua_formatting
  autocmd!
  autocmd BufNewFile,BufRead *.lua nnoremap <buffer><silent>,, :!luafmt -i 2 -w replace %<cr>:e!<cr>
augroup end
augroup custom_lsp_status
  autocmd!
  autocmd CursorHold <buffer> lua require('lsp-status').update_current_function()
augroup end


"                             Source External                          "
"                    ==============================                    "

" File manipulations
augroup load_file_manip
    autocmd!
    autocmd CursorHold,CursorHoldI,BufWritePost * source ~/.config/nvim/filemanip.vim | autocmd! load_file_manip
augroup end

" Git stuff
augroup load_git_utils
    autocmd!
    autocmd CursorHold,CursorHoldI,BufWritePost * source ~/.config/nvim/gitutils.vim | autocmd! load_git_utils
augroup end

augroup load_additional_plugins
    autocmd!
    autocmd CursorHold,CursorHoldI,BufWritePost * call plug#load('vim-googler') |
          \ call plug#load('vim-gitgutter') |
          \ call plug#load('vim-package-info') | 
          \ call plug#load('echodoc.vim') |
          \ call plug#load('vim-repeat') |
          \ call plug#load('vim-package-info') |
          \ call plug#load('vim-printer') |
          \ call plug#load('vista.vim') |
          \ autocmd! load_additional_plugins
augroup end

" Statusline
source ~/.config/nvim/statusline.vim

" lua stuff
lua require('configs')
