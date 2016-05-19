"==========================
"Vim plug to manage plugins
"==========================
call plug#begin('~/.vim/plugged')

" A fancy start page for vim
Plug 'mhinz/vim-startify'
" Supertab - kind of does the autocompletion on pressing tab
Plug 'ervandew/supertab'
" Org mode - kind of like in emacs
Plug 'jceb/vim-orgmode'
" dependencey for orgmode
Plug 'tpope/vim-speeddating'
" Python autocomplete
Plug 'davidhalter/jedi-vim'
" Vim colorscheme
Plug 'flazz/vim-colorschemes'
" Better file browser
Plug 'scrooloose/nerdtree'
" Code commenter
Plug 'tpope/vim-commentary'
" Class/module browser
Plug 'majutsushi/tagbar'
" Code and files fuzzy finder
Plug 'kien/ctrlp.vim'
" Git stuff from within vim
Plug 'tpope/vim-fugitive'
" Git add commit and stuff
Plug 'motemen/git-vim'
" Extension to ctrlp, for fuzzy command finder
Plug 'fisadev/vim-ctrlp-cmdpalette'
" Zen coding
Plug 'rstacruz/sparkup'
" Airline
Plug 'bling/vim-airline'
" Airline themes
Plug 'vim-airline/vim-airline-themes'
" Surround
Plug 'tpope/vim-surround'
" Autoclose
Plug 'Townk/vim-autoclose'
" Indent text object
Plug 'michaeljsmith/vim-indent-object'
" Better autocompletion
Plug 'Shougo/neocomplcache.vim'
" Snippets manager (SnipMate), dependencies, and snippets repo
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'honza/vim-snippets'
Plug 'garbas/vim-snipmate'
" Git/mercurial/others diff icons on the side of the file lines
Plug 'mhinz/vim-signify'
" Drag visual blocks arround
Plug 'fisadev/dragvisuals.vim'
" Window chooser
Plug 't9md/vim-choosewin'
" Paint css colors with the real color
Plug 'lilydjwg/colorizer'
" Neomake - linting and stuf
Plug 'neomake/neomake'
" Easier seakin to the desired text
Plug 'justinmk/vim-sneak'
"javascript complete after install the plugin, you must cd the install
"directory and run `npm install`, then add a .tern-project config file
"the doc at http://ternjs.net/doc/manual.html#vim
Plug 'marijnh/tern_for_vim'
" JSX syntax highlight.
Plug 'mxw/vim-jsx'
" Markdown syntastic highlight
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
" Markdown realtime preview
" Before you want to use it, please run
" `sudo npm -g install instant-markdown-d`
Plug 'suan/vim-instant-markdown'
"Easy vim and tmux splitting
" Plug 'christoomey/vim-tmux-navigator'
" Better python folding
Plug 'tmhedberg/SimpylFold'


" Plugins from vim-scripts repos:

" Search results counter
Plug 'IndexedSearch'
" XML/HTML tags navigation
Plug 'matchit.zip'
" Restore file pointer
Plug 'restore_view.vim'


call plug#end()






" =============================
" Keybindings - non plugin ones
" =============================

" Mapping for leader and local leader
let mapleader = "\<Space>"
let maplocalleader = "\\"

" Easier sourcing of init.vim
nnoremap <Leader>0 :source ~/.config/nvim/init.vim<cr>

" Saving and quitting keybindings
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>ww :wq<CR>
nnoremap W :w<CR>
nnoremap Q :q<CR>

" Window resizing
nnoremap <silent> <Leader>= :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>
nnoremap <silent> <Leader>0 :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent> <Leader>9 :exe "vertical resize " . (winwidth(0) * 2/3)<CR>

" Necessary stuff
" nnoremap ; :
" nnoremap ' ;

" Indent based folding
set foldmethod=indent
set foldignore=
set foldlevelstart=10
set foldnestmax=10
nnoremap <c-j> za

" Redraw only when essential
set lazyredraw

" make backspace work like most other apps
set backspace=2

" allow plugins by file type (required for plugins!)
filetype plugin on
filetype indent on

" tabs and spaces handling
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
" highlight cursor line and column
set cursorline
" set cursorcolumn
" hidden startup messages
set shortmess=atI
" auto read and write
set autowrite
set autoread
" when deal with unsaved files ask what to do
set confirm
" no backup files
set nobackup
" allow mouse
set mouse=a


"auto open or close on start
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" always show status bar
set laststatus=2

" incremental search
set incsearch
" highlighted search results
set hlsearch
" search ignore case
set ignorecase

"Clear search highlight
nnoremap <silent><Leader>/ :nohls<CR>

"Repeat last macro with !
map ! @@

" syntax highlight on
syntax on

" better line numberings
set nu
set relativenumber

" Enable going down in case text is wrapped
:nnoremap j gj
:nnoremap k gk

" Just something I have to do
:command WQ wq
:command Wq wq
:command W w
:command Q q

" tab navigation mappings
map <Leader>tn :tabn<CR>
map <Leader>tp :tabp<CR>
map <Leader>tm :tabm
map <Leader>tt :tabnew\|:Startify<cr>
map <Leader>ts :tab split<CR>
map <C-S-Right> :tabn<CR>
imap <C-S-Right> <ESC>:tabn<CR>
map <C-S-Left> :tabp<CR>
imap <C-S-Left> <ESC>:tabp<CR>
set splitbelow
set splitright

" Proper indenting on pasting
:nnoremap p  mp"*p=`]`p

" Copy and paste to the system clipboard - this also helps with dd or x
" replacing the content in the clipboard
nnoremap  y "*y
nnoremap  Y "*Y
nnoremap  P "*P
vnoremap y "*y
vnoremap Y "*Y
vnoremap p "*p
vnoremap P "*P

" Easy save
nnoremap <Leader><Leader> :w<cr>

" Buffer switching
map <Leader>bn :bn<CR>
map <Leader>bp :bp<CR>

" navigate splits with shift and hjkl
" rotate with shift r
map <s-l> <c-w>l
map <s-h> <c-w>h
map <s-k> <c-w>k
map <s-j> <c-w>j
map <s-r> <c-w>r

" Spell checking
" Initially choose the file types which support spell check
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.org setlocal spell
autocmd BufRead,BufNewFile *.txt setlocal spell
autocmd BufRead,BufNewFile *.mk setlocal spel
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

" Inser a space before and after
nnoremap <Leader>9 :normal! i<Space><Esc>lli<Space><Esc>

" save as sudo
ca w!! w !sudo tee "%"

" Setting colorscheme
colorscheme CandyPaper

" when scrolling, keep cursor 3 lines away from screen border
set scrolloff=5

" autocompletion of files and commands behaves like zsh
" (autocomplete menu)
set wildmenu
set wildmode=full

" better backup, swap and undos storage
set directory=~/.vim/dirs/tmp     " directory to place swap files in
set backup                        " make backup files
set backupdir=~/.vim/dirs/backups " where to put backup files
set undofile                      " persistent undos - undo after you re-open the file
set undodir=~/.vim/dirs/undos
set viminfo+=n~/.vim/dirs/viminfo
" store yankring history file there too
let g:yankring_history_dir = '~/.vim/dirs/'

" create needed directories if they don't exist
if !isdirectory(&backupdir)
    call mkdir(&backupdir, "p")
endif
if !isdirectory(&directory)
    call mkdir(&directory, "p")
endif
if !isdirectory(&undodir)
    call mkdir(&undodir, "p")
endif

" Splitting keybindings
nnoremap <Leader>h :split <cr>
nnoremap <Leader>v :vsplit\|:Startify<cr>
nnoremap <Leader>n :tabnew\|:Startify<cr>

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
nnoremap <silent> _ :ZoomToggle<CR>

" Save session as zzz and quit all buffers - y at the end as it ask to replace
" old zzz
function! MinimizeIfZoomed()
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    endif
endfunction
nnoremap <c-q> :call MinimizeIfZoomed() \|:SSave zPreviousSession \| :qa<cr>y

" Easier session reload
nnoremap <Leader>z :SLoad zPreviousSession<cr>

" Easier indentation - does dot loose selection
vnoremap > >gv
vnoremap < <gv

" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.md,*.cpp,*.c,*.java :%s/\s\+$//e

" Python with virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF

" Setting ipdb breakponts
python << EOF
import vim
import re

ipdb_breakpoint = 'import ipdb; ipdb.set_trace()'

def set_breakpoint():
    breakpoint_line = int(vim.eval('line(".")')) - 1

    current_line = vim.current.line
    white_spaces = re.search('^(\s*)', current_line).group(1)

    vim.current.buffer.append(white_spaces + ipdb_breakpoint, breakpoint_line)

def remove_breakpoints():
    op = 'g/^.*%s.*/d' % ipdb_breakpoint
    vim.command(op)

def toggle_breakpoint():
    breakpoint_line = int(vim.eval('line(".")')) - 1
    if 'import ipdb; ipdb.set_trace()' in vim.current.buffer[breakpoint_line]:
        remove_breakpoints()
    elif 'import ipdb; ipdb.set_trace()' in vim.current.buffer[breakpoint_line-1]:
        remove_breakpoints()
    else :
        set_breakpoint()
    vim.command(':w')

vim.command('map <f6> :py toggle_breakpoint()<cr>')
EOF

" Open new terminal
nnoremap <F1> :vsp\|:terminal<cr>

" Open new termial and run the currently open python file
nnoremap <F2> :vsp\|:terminal python %<cr>

" Easier increment and decrement
nnoremap + <C-a>
nnoremap - <C-x>

"Better line limit making
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27

" Remap for easier use of arrow keys
nnoremap <c-j> <Down>
inoremap <c-j> <Down>
nnoremap <c-k> <Up>
inoremap <c-k> <Up>


" =============================
" Keybindings - plugin specific
" =============================

" Startify
nnoremap ,l :Startify<cr>

" NERDTree

" toggle nerdtree display
map <F3> :NERDTreeToggle<CR>
map <Leader>t :NERDTreeToggle<CR>
" open nerdtree with the current file selected
nnoremap ,t :NERDTreeFind<CR>
" don't show these file types
let NERDTreeIgnore = ['\.pyc$', '\.pyo$']

" Vim session manager using startify
nnoremap <Leader>s :SSave
nnoremap <Leader>o :SLoad

" Vim-debug
" disable default mappings, have a lot of conflicts with other plugins
let g:vim_debug_disable_mappings = 1
" add some useful mappings
" map <F5> :Dbg over<CR>
" map <F6> :Dbg into<CR>
" map <F7> :Dbg out<CR>
" map <F8> :Dbg here<CR>
" map <F9> :Dbg break<CR>
" map <F10> :Dbg watch<CR>
" map <F11> :Dbg down<CR>
" map <F12> :Dbg up<CR>

" CtrlP
" file finder mapping
let g:ctrlp_map = ',e'
"hidden some types files
let g:ctrlp_show_hidden = 1
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.png,*.jpg,*.gif           "Linux
" tags (symbols) in current file finder mapping
nnoremap ,g :CtrlPBufTag<CR>
" tags (symbols) in all files finder mapping
nnoremap ,G :CtrlPBufTagAll<CR>
" general code finder in all files mapping
nnoremap ,f :CtrlPLine<CR>
" recent files finder mapping
nnoremap ,m :CtrlPMRUFiles<CR>
" commands finder mapping
nnoremap ,c :CtrlPCmdPalette<CR>
" to be able to call CtrlP with default search text
function! CtrlPWithSearchText(search_text, ctrlp_command_end)
    execute ':CtrlP' . a:ctrlp_command_end
    call feedkeys(a:search_text)
endfunction
" same as previous mappings, but calling with current word as default text
nnoremap ,wg :call CtrlPWithSearchText(expand('<cword>'), 'BufTag')<CR>
nnoremap ,wG :call CtrlPWithSearchText(expand('<cword>'), 'BufTagAll')<CR>
nnoremap ,wf :call CtrlPWithSearchText(expand('<cword>'), 'Line')<CR>
nnoremap ,we :call CtrlPWithSearchText(expand('<cword>'), '')<CR>
nnoremap ,pe :call CtrlPWithSearchText(expand('<cfile>'), '')<CR>
nnoremap ,wm :call CtrlPWithSearchText(expand('<cword>'), 'MRUFiles')<CR>
nnoremap ,wc :call CtrlPWithSearchText(expand('<cword>'), 'CmdPalette')<CR>
" don't change working directory
let g:ctrlp_working_path_mode = 0
" ignore these files and folders on file finder
let g:ctrlp_custom_ignore = {
            \ 'dir':  '\v[\/](\.git|\.hg|\.svn)$',
            \ 'file': '\.pyc$\|\.pyo$',
            \ }

" NeoComplCache
" most of them not documented because I'm not sure how they work
" (docs aren't good, had to do a lot of trial and error to make
" it play nice)
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_ignore_case = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_auto_select = 1
let g:neocomplcache_enable_fuzzy_completion = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_fuzzy_completion_start_length = 1
let g:neocomplcache_auto_completion_start_length = 1
let g:neocomplcache_manual_completion_start_length = 1
let g:neocomplcache_min_keyword_length = 1
let g:neocomplcache_min_syntax_length = 1
" complete with workds from any opened file
let g:neocomplcache_same_filetype_lists = {}
let g:neocomplcache_same_filetype_lists._ = '_'

" Autoclose
" Fix to let ESC work as espected with Autoclose plugin
let g:AutoClosePumvisible = {"ENTER": "\<C-Y>", "ESC": "\<ESC>"}

" Simple fold
autocmd BufWinEnter *.py setlocal foldexpr=SimpylFold(v:lnum) foldmethod=expr
autocmd BufWinLeave *.py setlocal foldexpr< foldmethod<

" DragVisuals
" mappings to move blocks in 4 directions
vmap <expr> <S-LEFT> DVB_Drag('left')
vmap <expr> <S-RIGHT> DVB_Drag('right')
vmap <expr> <S-DOWN> DVB_Drag('down')
vmap <expr> <S-UP> DVB_Drag('up')
" mapping to duplicate block
vmap <expr> D DVB_Duplicate()

" Signify
" this first setting decides in which order try to guess your current vcs
" UPDATE it to reflect your preferences, it will speed up opening files
let g:signify_vcs_list = [ 'git', 'hg' ]
" mappings to jump to changed blocks
nnoremap <leader>sn <plug>(signify-next-hunk)
nnoremap <leader>sp <plug>(signify-prev-hunk)
" nicer colors
highlight DiffAdd           cterm=bold ctermbg=none ctermfg=119
highlight DiffDelete        cterm=bold ctermbg=none ctermfg=167
highlight DiffChange        cterm=bold ctermbg=none ctermfg=227
highlight SignifySignAdd    cterm=bold ctermbg=237  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=237  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=237  ctermfg=227

" Window Chooser
" mapping
nmap  -  <Plug>(choosewin)
" show big letters
let g:choosewin_overlay_enable = 1

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#whitespace#enabled = 1
" Top bar
let g:airline#extensions#tabline#enabled = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = '  '
let g:airline_left_alt_sep = ' '
let g:airline_right_sep = '  '
let g:airline_right_alt_sep = '  '
let g:airline_symbols.branch = '  '
let g:airline_symbols.readonly = '  '
let g:airline_symbols.linenr = '  '

" new file set title and turn to endline
autocmd BufNewFile *.sh,*.py,*.rb exec ":call SetTitle()"
function SetTitle()
    if &filetype == 'sh'
        call setline(1,"\#!/bin/bash")
        call append(line("."), "")

    elseif &filetype == 'python'
        call setline(1,"#!/usr/bin/env python")
        call append(line("."),"# coding=utf-8")
        call append(line(".")+1, "")

    elseif &filetype == 'ruby'
        call setline(1,"#!/usr/bin/env ruby")
        call append(line("."),"# encoding: utf-8")
        call append(line(".")+1, "")
    endif
endfunction
autocmd BufNewFile * normal G

" Tagbar
:nnoremap <F8> :TagbarToggle<CR>

" Vim-jsx
" if you use JSX syntax in .js file, please enable it.
let g:jsx_ext_required = 0

" Vim-markdown
" Disabled automatically folding
let g:vim_markdown_folding_disabled=1
" LeTeX math
let g:vim_markdown_math=1
" Highlight YAML frontmatter
let g:vim_markdown_frontmatter=1

" Neomake linting
autocmd! BufWritePost,BufEnter * Neomake

" Jedi for python
let g:jedi#use_splits_not_buffers = "bottom"
let g:jedi#goto_command = "<leader>d"
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = ""
let g:jedi#documentation_command = "<leader>k"
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"

" Snipmate
nnoremap <c-s> <Plug>snipMateTrigger
imap <c-s> <Plug>snipMateTrigger
