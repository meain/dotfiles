"==========================
"Vim plug to manage plugins
"==========================
call plug#begin('~/.vim/plugged')

" A fancy start page for vim
Plug 'mhinz/vim-startify'
" Python autocomplete
Plug 'davidhalter/jedi-vim', { 'for' : 'python' }
" Better autocompletion
function! DoRemote(arg)
	UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
" Vim colorscheme
Plug 'flazz/vim-colorschemes'
" Better file browser
Plug 'scrooloose/nerdtree', { 'on' : 'NERDTreeToggle' }
" Code commenter
Plug 'tpope/vim-commentary'
" Class/module browser
Plug 'majutsushi/tagbar', { 'on' : 'Tagbar' }
" Git stuff from within vim
Plug 'tpope/vim-fugitive'
" Code and files fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'
" Extension to ctrlp, for fuzzy command finder
Plug 'fisadev/vim-ctrlp-cmdpalette'
" Zen coding
Plug 'rstacruz/sparkup', { 'for' : 'html' }
" Airline
Plug 'bling/vim-airline'
" Airline themes
Plug 'vim-airline/vim-airline-themes'
" Surround
Plug 'tpope/vim-surround'
" Autopair
Plug 'jiangmiao/auto-pairs'
" Snippets manager (SnipMate), dependencies, and snippets repo
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'honza/vim-snippets'
Plug 'garbas/vim-snipmate'
" Git/mercurial/others diff icons on the side of the file lines
Plug 'mhinz/vim-signify'
" Paint css colors with the real color
Plug 'lilydjwg/colorizer'
" Neomake - linting and stuf
Plug 'neomake/neomake'
"javascript complete after install the plugin, you must cd the install
"directory and run `npm install`, then add a .tern-project config file
"the doc at http://ternjs.net/doc/manual.html#vim
Plug 'marijnh/tern_for_vim'
" For javascript
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'nathanaelkane/vim-indent-guides'
" Markdown realtime preview
" Before you want to use it, please run
" `sudo npm -g install instant-markdown-d`
Plug 'suan/vim-instant-markdown', { 'for' : 'markdown' }
"Easy vim and tmux splitting
" Plug 'christoomey/vim-tmux-navigator'
" Jade syntax and indentation
Plug 'digitaltoad/vim-pug', { 'for' : 'pug' }
" Stylus syntax and indentation
Plug 'wavded/vim-stylus', { 'for' : 'stylus' }
" Show indent
Plug 'Yggdroot/indentLine'
" Ack like code search in vim
Plug 'rking/ag.vim', { 'on' : 'Ag' }
" Mru - mostly for use with v in shell
Plug 'vim-scripts/mru.vim'


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
" Set encoding
set encoding=utf8

" Set path variable so that the autocomplete for filenames is complete
set path+=**

" Mapping for leader and local leader
let mapleader = "\<Space>"
let maplocalleader = "\\"

" Saving and quitting keybindings
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>ww :wq<CR>

" Necessary stuff
nnoremap ; :
nnoremap ' ;

" Show invisibles
set list
set listchars=tab:▸\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
set showbreak=↪

" Indent based folding
set foldmethod=indent
set foldignore=
set foldlevelstart=10
set foldnestmax=10

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
" Switch between using tabs or spaces bsed on the file
function TabsOrSpaces()
    " Determines whether to use spaces or tabs on the current buffer.
    if getfsize(bufname("%")) > 256000
        " File is very large, just use the default.
        return
    endif
    let numTabs=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^\\t"'))
    let numSpaces=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^ "'))
    if numTabs > numSpaces
        setlocal noexpandtab
    endif
endfunction
autocmd BufReadPost * call TabsOrSpaces()

" highlight cursor line
set cursorline
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
" smart case
set smartcase

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=1

"auto open or close on start
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Enble and Disable wrapping
function! DisableWrap()
	set nowrap
	set nolinebreak
	set formatoptions-=cro
endfunction
autocmd BufNew,BufAdd,BufCreate,VimEnter *  call DisableWrap()

" Disable autocomment
function! DisableAutoComment()
	set formatoptions-=qr
endfunction
autocmd BufNew,BufAdd,BufCreate,VimEnter * call DisableAutoComment()

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

" syntax highlight on
syntax on

" Enable going down in case text is wrapped
nnoremap j gj
nnoremap k gk

" Just something I have to do
command WQ wq
command Wq wq
command W w
command Q q

" tab navigation mappings
map <Leader>u :tabn<CR>
map <Leader>t :tabnew\|:Startify<cr>
map <Leader>y :tabp<CR>

" Buffer switching
map <Leader>n :bn<CR>
map <Leader>m :bp<CR>

" Better split opening
set splitbelow
set splitright

" Proper indenting on pasting
nnoremap p  "*]p`]

" Copy and paste to the system clipboard - this also helps with dd or x
" replacing the content in the clipboard
nnoremap  y "*y
nnoremap  Y "*Y
nnoremap  P "*P
vnoremap y "*y`]
vnoremap Y "*Y
vnoremap p "*p
vnoremap P "*P

"Get back to where you were easily
nnoremap gg mpgg
nnoremap G mpG
nnoremap / mp/

" Easy save
nnoremap <Leader><Leader> :w<cr>

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
set complete+=kspell
function! FixLastSpellingError()
    normal! mz[s1z=`z
endfunction
nnoremap Z :call FixLastSpellingError()<cr>
inoremap <c-z> :call FixLastSpellingError()<cr>
" Disable spell check in help files
autocmd FileType help setlocal nospell

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
set background=dark
colorscheme gruvbox

" when scrolling, keep cursor 5 lines away from screen border
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

" Google search from within vim
function! GoogleSearch(arg)
py << EOF
import vim
def frame_querry():
	term = vim.eval("a:arg")
	vim.command("vsp")
	vim.command("term googler --count 7 " + term)
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

" Easier indentation - does dot loose selection
vnoremap > >gv
vnoremap < <gv

" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.cpp,*.c,*.java :%s/\s\+$//e

" Setting ipdb breakponts
nnoremap <Leader>b :normal! Oimport ipdb; ipdb.set_trace()<cr>

" Open new terminal
nnoremap <F1> :vsp\|:terminal<cr>
" Open new termial and run the currently open python file
nnoremap <F2> :vsp\|:terminal python %<cr>

"Better line limit
highlight ColorColumn ctermbg=0
autocmd BufNew,BufAdd,BufCreate,VimEnter * normal! :set cc=80

" Alphabetically sort CSS properties in file with :SortCSS
command! SortCSS :g#\({\n\)\@<=#.,/}/sort

" Compile pug and jade files on save and refresh chrome on any web file change
autocmd BufWritePost *.pug :silent ! pug -P % > /dev/null
autocmd BufWritePost *.pug :silent ! chromix with http://localhost:4500/ reload > /dev/null
autocmd BufWritePost *.stylus :silent ! stylus % > /dev/null
autocmd BufWritePost *.stylus :silent ! chromix with http://localhost:4500/ reload > /dev/null

" Reload chrome tab on save of web files
autocmd BufWritePost *.html,*.js,*.css :silent ! chromix with http://localhost:4500/ reload > /dev/null
autocmd BufWritePost *.html,*.js,*.css :silent ! chromix with http://localhost* reload > /dev/null

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap



" =============================
" Keybindings - plugin specific
" =============================

" Startify
nnoremap ,l :Startify<cr>
let g:startify_custom_footer = ".                       *meain*"
highlight StartifyBracket ctermfg=240
highlight StartifyFooter  ctermfg=240
highlight StartifyHeader  ctermfg=114
highlight StartifyNumber  ctermfg=215
highlight StartifyPath    ctermfg=245
highlight StartifySlash   ctermfg=240
highlight StartifySpecial ctermfg=240

" NERDTree
" toggle nerdtree display
map <F3> :NERDTreeToggle<CR>
" don't show these file types
let NERDTreeIgnore = ['\.pyc$', '\.pyo$']

" CtrlP
" file finder mapping
let g:ctrlp_map = ',e'
"hidden some types files
let g:ctrlp_show_hidden = 1
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.png,*.jpg,*.gif  "Linux
" tags (symbols) in current file finder mapping
nnoremap ,f :CtrlPBufTag<CR>
" tags (symbols) in all files finder mapping
nnoremap ,F :CtrlPBufTagAll<CR>
" general code finder in all files mapping
nnoremap ,g :CtrlPLine<CR>
" recent files finder mapping
nnoremap ,m :CtrlPMRUFiles<CR>
nnoremap ,, :CtrlPMRUFiles<CR>
" commands finder mapping
nnoremap ,c :CtrlPCmdPalette<CR>
nnoremap <leader>l :CtrlPCmdPalette<CR>
" to be able to call CtrlP with default search text
function! CtrlPWithSearchText(search_text, ctrlp_command_end)
    execute ':CtrlP' . a:ctrlp_command_end
    call feedkeys(a:search_text)
endfunction
" same as previous mappings, but calling with current word as default text
nnoremap ,wf :call CtrlPWithSearchText(expand('<cword>'), 'BufTag')<CR>
nnoremap ,wF :call CtrlPWithSearchText(expand('<cword>'), 'BufTagAll')<CR>
nnoremap ,wg :call CtrlPWithSearchText(expand('<cword>'), 'Line')<CR>
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

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#whitespace#enabled = 1
" Top bar
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

" Vim-jsx
" if you use JSX syntax in .js file, please enable it.
let g:jsx_ext_required = 0

" Neomake linting
autocmd! BufWritePost,BufEnter * Neomake
let g:neomake_javascript_enabled_makers = ['eslint']

" Jedi python
let g:jedi#use_splits_not_buffers = "bottom"
let g:jedi#goto_command = "<leader>d"
let g:jedi#goto_assignments_command = "<leader>a"
let g:jedi#goto_definitions_command = "<leader>k"
let g:jedi#documentation_command = "<leader>c"
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"
let g:jedi#max_doc_height = 30
autocmd FileType python setlocal completeopt-=preview

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
set completeopt+=noinsert

" Smooth scroll
noremap <silent> 9 :normal!10j<cr>
noremap <silent> 8 :normal!10k<cr>

" sparkup
let g:sparkupExecuteMapping='<c-e>'

" Fugitive
nnoremap <leader>g :Gstatus<cr>
command! Gl normal! :!git vhm<cr>

" MRU
let MRU_Max_Entries = 1000
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Auto_Close = 1
let MRU_Max_Menu_Entries = 10
