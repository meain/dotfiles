"                            Plugin Management                         "
"                    ==============================                    "

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'mhinz/vim-startify'                                                                  " A fancy start page for vim
Plug 'flazz/vim-colorschemes'                                                              " Vim colorscheme
Plug 'ayu-theme/ayu-vim'                                                                   " A sick colorscheme
Plug 'junegunn/vim-easy-align'                                                             " Some prettification
Plug 'roxma/nvim-completion-manager'                                                       " Better autocompletion
Plug 'roxma/nvim-cm-tern',  {'do': 'npm install', 'for': 'javascript'}                     " Completing for js
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }                    " nvim language client
Plug 'SirVer/ultisnips'                                                                    " Snippet manager
Plug 'honza/vim-snippets'                                                                  " Snippet packages
Plug 'tpope/vim-vinegar'                                                                   " Better netrw
Plug 'scrooloose/nerdtree', { 'on': ['NERDTree', 'NERDTreeToggle'] }                       " Nerdtree
Plug 'ervandew/supertab'                                                                   " Autocomplete on tab
Plug 'christoomey/vim-tmux-navigator'                                                      " Seamless navigation between vim and tmux
Plug 'tomtom/tcomment_vim'                                                                 " Code commenting
Plug 'zirrostig/vim-schlepp'                                                               " Better drag visuals
Plug 'kshenoy/vim-signature'                                                               " Show marks
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }                                         " Undo tree
Plug 'unblevable/quick-scope'                                                              " Better f and d
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }                          " Fzf for vim
Plug 'junegunn/fzf.vim'                                                                    " fzf for vim
Plug 'majutsushi/tagbar', { 'on' : 'Tagbar' }                                              " Class/module browser
Plug 'tpope/vim-fugitive'                                                                  " Git stuff from within vim
Plug 'gregsexton/gitv', {'on': ['Gitv']}                                                   " Magit like git interface
Plug 'itchyny/lightline.vim'                                                               " Statusline plugin
Plug 'tpope/vim-sleuth'                                                                    " Automatic indentation setting
Plug 'tpope/vim-surround'                                                                  " Surround
Plug 'jiangmiao/auto-pairs'                                                                " Autopair
Plug 'mhinz/vim-signify'                                                                   " Git/mercurial/others diff icons on the side of the file lines
Plug 'w0rp/ale'                                                                            " Neomake - linting and stuf
Plug 'Yggdroot/indentLine'                                                                 " Show indent
Plug 'vim-scripts/mru.vim'                                                                 " Mru - mostly for use with v in shell
Plug 'Chiel92/vim-autoformat'                                                              " Beautify code
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }                                       " Hyper focus editing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }                                                 " Centerify
Plug 'mattn/webapi-vim'                                                                    " Inplementation of differnt web apis
Plug 'Rykka/colorv.vim'                                                                    " Color picker, color schemes etc
Plug 'vim-scripts/IndexedSearch'                                                           " Indexed search
Plug 'meain/hlnext.vim'                                                                    " Hilight next search
Plug 'tmhedberg/matchit', { 'for': ['html','xml', 'tex'] }                                 " Match tags for html, xml latex etc
Plug 'vim-scripts/restore_view.vim'                                                        " Restore file pointer
Plug 'suan/vim-instant-markdown', {'for': 'markdown' ,'do': 'npm i -g instant-markdown-d'} " Markdown realtime preview
Plug 'sheerun/vim-polyglot'                                                                " Multiple language support
Plug 'tpope/vim-markdown'
Plug 'suan/vim-instant-markdown'
Plug 'davidhalter/jedi-vim', { 'for': ['python'] }                                         " Python helper
Plug 'leafgarland/typescript-vim', { 'for': ['js', 'typescript', 'tsc'] }                  " Much better js and tsc support
" Plug 'blueyed/vim-diminactive'                                                           " Dim inactive windows
" Plug 'gko/vim-coloresque', { 'for': 'css' }                                              " Paint css colors with the real color
" Plug 'luochen1990/rainbow'                                                               " Rainbow delim

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

" Highlighted search results
set hlsearch

" Search ignore case
set ignorecase

" Allow plugins by file type (required for plugins!)
filetype plugin on
filetype indent on

" Redraw only when essential
set lazyredraw

" Setting as hidden ( for scratch )
set hidden

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
set listchars=tab:▸~,eol:¬,trail:⋅
set showbreak=↪

" When scrolling, keep cursor 5 lines away from screen border
set scrolloff=5

" Autocompletion of files and commands behaves like zsh
set wildmenu
set wildmode=full

" Setting up ignores
set wildignore+=*/tmp/*,*.so,*.pyc,*.png,*.jpg,*.gif
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*
set wildignore+=*.swp,*~,._*

" Indent based folding
set foldmethod=indent
set foldignore=
set foldlevelstart=10
set foldnestmax=10

" Setting colorscheme
set background=dark
colorscheme buddy

" Set up leader keys
let mapleader = "\<Space>"
let maplocalleader = "\\"

" Smart colorcolumn
highlight ColorColumn ctermbg=0 guibg=#474747
autocmd BufEnter * call matchadd('ColorColumn', '\%120v', 100)

" Better coloring for errors
hi clear SpellBad
hi SpellBad cterm=underline

" Don't you f'in touch my cursor
set guicursor=





"                                Commands                              "
"                    ==============================                    "

" Save as sudo
ca w!! w !sudo tee "%"

" I am too lazy to take my hands from shift
command WQ wq
command Wq wq
command W w
" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.cpp,*.c,*.java :%s/\s\+$//e

" Alphabetically sort CSS properties in file with :SortCSS
command! SortCSS :g#\({\n\)\@<=#.,/}/sort

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap

" Set cwd to the one in open file
" autocmd BufEnter * silent! lcd %:p:h





"                               Key remaps                             "
"                    ==============================                    "


" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Unset J and K for now
nnoremap J 10<c-y>
nnoremap K 10<c-e>

"Get back to where you were easily
nnoremap gg mpgg
nnoremap G mpG
nnoremap / mp/

" Quick command mode (second binding to get find next)
nnoremap ; :
nnoremap ' ;

" Use the clipboard for copy and paste
nnoremap y "+y
nnoremap Y "+Y
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
nnoremap <Down> 10<c-y>
nnoremap <Up> 10<c-e>

" Easy buffer switching
nnoremap <silent><Tab> :bn<cr>
nnoremap <silent><s-Tab> :bp<cr>
nnoremap <silent><Leader><Tab> :enew\|:Startify\|:MRUFzf<cr>

" Terminal split jump
tnoremap <m-h> <C-\><C-N><C-w>h
tnoremap <m-j> <C-\><C-N><C-w>j
tnoremap <m-k> <C-\><C-N><C-w>k
tnoremap <m-l> <C-\><C-N><C-w>l

" Terminal mode esc remap
tnoremap <Leader><Esc> <C-\><C-n>





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <silent><Leader><Leader> :w<cr>
nnoremap <silent><Leader>q :bd<cr>
nnoremap <silent><Leader>w :w<cr>

" Split like a boss
" nnoremap <silent><Leader>v :vsplit\|:Startify\|:MRUFzf<cr>
" nnoremap <silent><Leader>h :sp\|:Startify\|:MRUFzf<cr>
nnoremap <silent><Leader>v :vsplit\|:Startify<cr>
nnoremap <silent><Leader>h :sp\|:Startify<cr>

" Clear search highlight
nnoremap <silent><Leader>/ :nohls<CR>

" Easy tag navigation
nnoremap <silent><Leader>; <C-]>
nnoremap <silent><Leader>' <C-o>

" Quick fold and unfold
nnoremap <silent><Leader><esc> :normal!za<cr>

" Quick excecute python
nnoremap <silent><Leader>e :!python %<cr>

" Quick switch tabs
nnoremap <silent><Leader>n :tabn<cr>
nnoremap <silent><Leader>p :tabp<cr>





"                                Code                                  "
"                    ==============================                    "


" Switch between using tabs or spaces bsed on the file
function TabsOrSpaces()
    if getfsize(bufname("%")) > 256000
        return
    endif
    let numTabs=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^\\t"'))
    let numSpaces=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^ "'))
    if numTabs > numSpaces
        setlocal noexpandtab
    endif
endfunction
autocmd BufReadPost * call TabsOrSpaces()

" Spell checking
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.org setlocal spell
autocmd BufRead,BufNewFile *.txt setlocal spell
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
function StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
      normal mz
      %s/\s\+$//e
      normal `z
    endif
endfunction
command! StripTrailingWhitespace :call StripTrailingWhitespace()

" Better marks
function Marks()
    marks abcdefghijklmnopqrstuvwxyz.
    echo 'Jump to mark: '
    let mark=nr2char(getchar())
    redraw
    execute 'normal! `'.mark
endfunction
command Marks call Marks()
nnoremap <silent>`` :call Marks()<cr>





"                            Plugin settings                           "
"                    ==============================                    "

" Fzf fuzzy search
let $FZF_DEFAULT_COMMAND = 'ag --hidden -l -g ""'
command! -bang -nargs=* MRUFzf call fzf#vim#history(fzf#vim#with_preview())
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
command! History call fzf#run({'sink': 'e', 'source': 'tail -n+3 ~/.vim_mru_files | grep -v ".git"', 'down': '40%', 'options': '--preview "coderay {}"' })
nnoremap <silent>,, :History<cr>
nnoremap ,e :FZF<cr>
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)
nnoremap <leader>f :GFiles<CR>
nnoremap <leader>l :Commands<CR>
nnoremap <leader>t :Tags<CR>
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
let g:jedi#documentation_command = "<leader>k"
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = ""
let g:jedi#rename_command = "<leader>r"
let g:jedi#max_doc_height = 30

" Rainbow delim
let g:rainbow_active = 1

" MRU
let MRU_Max_Entries = 100
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Auto_Close = 1
let MRU_Max_Menu_Entries = 10

" nvim-completion-engine
set shortmess+=c   " supress the annoying completion messages
" autocmd CompleteDone * pclose
" set completeopt+=noinsert
" set completeopt-=preview

" Fugitive
nnoremap <silent><leader>g :Gstatus\|normal!gg8j<cr>
command! Gl normal! :!git vhm<cr>

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'distinguished'
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#tabline#enabled = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = ' '
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = ' '
let g:airline_symbols.branch = '✌'
let g:airline_symbols.readonly = 'ro'
let g:airline_symbols.linenr = ' '

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
vmap <unique> <up>    <Plug>SchleppUp
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight
vmap <unique> D <Plug>SchleppDup

" Limelight
let g:limelight_conceal_ctermfg=0

" Quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
let g:qs_first_occurrence_highlight_color = 155
let g:qs_second_occurrence_highlight_color = 81

" Ale
let g:ale_sign_column_always = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_sign_error = ':x'
let g:ale_sign_warning = ':!'
highlight ALEErrorSign ctermfg=196
highlight ALEWarningSign ctermfg=226
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'python': ['flake8']
\}

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
autocmd TermOpen * IndentLinesDisable

" Easy allign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Signfy
highlight SignifySignAdd    cterm=bold ctermbg=240  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=240  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=240  ctermfg=227

" Lightline
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }

" Sleuth auto indent
let g:sleuth_automatic = 1

" Polyglot
let g:polyglot_disabled = ['markdown', 'md']

" Markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']
