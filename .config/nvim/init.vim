"                            Plugin Management                         "
"                    ==============================                    "

call plug#begin('~/.vim/plugged')

" Vim colorscheme
Plug 'flazz/vim-colorschemes'

" Better file browser
Plug 'scrooloose/nerdtree', { 'on' : 'NERDTreeToggle' }

" Autocomplete on tab
Plug 'ervandew/supertab'

" Code commenter
Plug 'tpope/vim-commentary'

" Fzf for vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Rainbow delim
Plug 'luochen1990/rainbow'

" Class/module browser
Plug 'majutsushi/tagbar', { 'on' : 'Tagbar' }

" Git stuff from within vim
Plug 'tpope/vim-fugitive'

" Airline
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Surround
Plug 'tpope/vim-surround'

" Autopair
Plug 'jiangmiao/auto-pairs'

" Git/mercurial/others diff icons on the side of the file lines
Plug 'mhinz/vim-signify'

" Neomake - linting and stuf
Plug 'neomake/neomake'

" Show indent
Plug 'Yggdroot/indentLine'

" Mru - mostly for use with v in shell
Plug 'vim-scripts/mru.vim'

" See images in vim
Plug 'ashisha/image.vim'

" Hyper focus editing
Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'

" Search results counter
Plug 'IndexedSearch'

" XML/HTML tags navigation
Plug 'matchit.zip'

" Restore file pointer
Plug 'restore_view.vim'

" Paint css colors with the real color
Plug 'gko/vim-coloresque'

" Markdown realtime preview
" `sudo npm -g install instant-markdown-d`
Plug 'suan/vim-instant-markdown', { 'for' : 'markdown' }

" Go development
Plug 'fatih/vim-go'

call plug#end()





"                           Editor Settings                            "
"                    ==============================                    "


" Set encoding to utf8
set encoding=utf8

" Make autocomplete for filenames work
set path+=**

" Turn on line numbers
set number

" Highlight cursor line
set cursorline

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
set backup                        " make backup files
set backupdir=~/.vim/dirs/backups " where to put backup files
set undofile                      " persistent undos - undo after you re-open the file

" Allow mouse
set mouse=a

" Incremental search
set incsearch

" Highlighted search results
set hlsearch

" Search ignore case
set ignorecase

" Remove delay on hitting esc ( tmux )
set noesckeys
set ttimeout
set ttimeoutlen=1

" Allow plugins by file type (required for plugins!)
filetype plugin on
filetype indent on

" Redraw only when essential
set lazyredraw

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

" Show invisibles
set list
set listchars=tab:▸\ ,eol:¬,trail:⋅
"set listchars=tab:▸\ ,trail:⋅
set showbreak=↪

" When scrolling, keep cursor 5 lines away from screen border
set scrolloff=5

" Autocompletion of files and commands behaves like zsh
set wildmenu
set wildmode=full

" Setting up ignores
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.png,*.jpg,*.gif  "Linux

" Indent based folding
set foldmethod=indent
set foldignore=
set foldlevelstart=10
set foldnestmax=10

" Setting colorscheme
set background=dark
colorscheme gruvbox

" Set up leader keys
let mapleader = "\<Space>"
let maplocalleader = "\\"

" Terminal mode esc remap
tnoremap <Leader><Esc> <C-\><C-n>




"                                Commands                              "
"                    ==============================                    "

" Save as sudo
ca w!! w !sudo tee "%"

" I am too lazy to take my hands from shift
command WQ wq
command Wq wq
command W w
command Q q

" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.cpp,*.c,*.java :%s/\s\+$//e

" Alphabetically sort CSS properties in file with :SortCSS
command! SortCSS :g#\({\n\)\@<=#.,/}/sort

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap



"                               Key remaps                             "
"                    ==============================                    "


" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk

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
nnoremap p "+p
nnoremap P "+P
vnoremap y "+y
vnoremap Y "+Y
vnoremap p "+p
vnoremap P "+P

" Easy navigation
map <s-l> <c-w>l
map <s-h> <c-w>h
map <s-k> <c-w>k
map <s-j> <c-w>j

" Easier indentation - does dot loose selection
vnoremap > >gv
vnoremap < <gv





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <Leader><Leader> :w<cr>
nnoremap <Leader>q :q<cr>
nnoremap <Leader>w :w<cr>

" Split like a boss
nnoremap <Leader>v :vsp<cr>
nnoremap <Leader>h :sp<cr>

" Clear search highlight
nnoremap <silent><Leader>/ :nohls<CR>





"                                Code                                  "
"                    ==============================                    "


" Switch between using tabs or spaces bsed on the file
function TabsOrSpaces()
    if getfsize(bufname("%")) > 256000
        return  " File is huge
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
nnoremap <silent> - :ZoomToggle<CR> \| :normal! 0<cr>

" Save session and quit all buffers (for use with viml command)
function! MinimizeIfZoomed()
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    endif
endfunction
nnoremap <c-q> :call MinimizeIfZoomed() \|:SSave zPreviousSession \| :qa<cr>y

function StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
      normal mz
      %s/\s\+$//e
      normal `z
    endif
endfunction
command! StripTrailingWhitespace :call StripTrailingWhitespace()




"                            Plugin settings                           "
"                    ==============================                    "

" Fzf fuzzy search
let $FZF_DEFAULT_COMMAND = 'ag --hidden -l -g ""'
command! -bang -nargs=* MRUFzf call fzf#vim#history(fzf#vim#with_preview())
command! -bang -nargs=? -complete=dir GFiles
\ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)
nnoremap ,, :MRUFzf<CR>
nnoremap <leader>f :GFiles<CR>
nnoremap <leader>l :Commands<CR>
" nnoremap <leader>t :Tags<CR>
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

" Rainbow delim
let g:rainbow_active = 1

" Fugitive
nnoremap <leader>g :Gstatus<cr>
command! Gl normal! :!git vhm<cr>

" MRU
let MRU_Max_Entries = 1000
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Auto_Close = 1
let MRU_Max_Menu_Entries = 10

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
set completeopt+=noinsert

" Neomake linting
autocmd! BufWritePost,BufEnter * Neomake
let g:neomake_javascript_enabled_makers = ['eslint']

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
let g:airline_symbols.branch = ' '
let g:airline_symbols.readonly = ' '
let g:airline_symbols.linenr = ' '
