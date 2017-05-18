"                            Plugin Management                         "
"                    ==============================                    "

call plug#begin('~/.vim/plugged')

" A fancy start page for vim
Plug 'mhinz/vim-startify'

" Vim colorscheme
Plug 'flazz/vim-colorschemes'

" Better autocompletion
function! DoRemote(arg)
	UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'zchee/deoplete-go', { 'for': 'go', 'do': 'make'}
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern'}
Plug 'othree/jspc.vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'zchee/deoplete-jedi', { 'for': 'python' }

" Better netrw
Plug 'tpope/vim-vinegar'

" Autocomplete on tab
Plug 'ervandew/supertab'

" Smoother scrolling
Plug 'yuttie/comfortable-motion.vim'

" Code commenting
Plug 'scrooloose/nerdcommenter'

" Better drag visuals
Plug 'zirrostig/vim-schlepp'

" Undo tree
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }

" Auto skeleton for new files
Plug 'noahfrederick/vim-skeleton', { 'on': ['SkelEdit', 'SkelInsert'] }

" Better f and d
Plug 'unblevable/quick-scope'

" Scratch buffer
Plug 'mtth/scratch.vim', { 'on': ['Scratch', 'ScratchPreview'] }

" Dim inactive windows
Plug 'blueyed/vim-diminactive'

" Fzf for vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" colorschemes for vim
Plug 'flazz/vim-colorschemes'

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
Plug 'w0rp/ale'

" Show indent
Plug 'Yggdroot/indentLine'

" Mru - mostly for use with v in shell
Plug 'vim-scripts/mru.vim'

" See images in vim
Plug 'ashisha/image.vim'

" Beautify code
Plug '~/Documents/Projects/beautify.vim'

" Hyper focus editing
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

" Better search
Plug 'IndexedSearch'
" Plug 'meain/hlnext.vim'

" XML/HTML tags navigation
Plug 'matchit.zip', { 'for': ['html','xml'] }

" Restore file pointer
Plug 'restore_view.vim'

" Paint css colors with the real color
Plug 'gko/vim-coloresque', { 'for': 'css' }

" Markdown realtime preview
Plug 'suan/vim-instant-markdown', { 'for' : 'markdown' , 'do': 'npm install -g instant-markdown-d' }

" Go development
Plug 'fatih/vim-go', { 'for': 'go' }

" Python development
Plug 'davidhalter/jedi-vim', { 'for': 'python' }

" Vue development
Plug 'posva/vim-vue', { 'for': 'vue', 'do': 'npm i -g eslint eslint-plugin-vue'}

" Latex plugin
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

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

" Remove delay on hitting esc ( tmux )
set noesckeys
set ttimeout
set ttimeoutlen=1

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

" We have pluins that show this
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

" Smart colorcolumn
highlight ColorColumn ctermbg=0 guibg=#eee8d5
call matchadd('ColorColumn', '\%120v', 100)

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
" Remove whitespace at save
autocmd BufWritePre *.py,*.vim,*.css,*.js,*.html,*.cpp,*.c,*.java :%s/\s\+$//e

" Alphabetically sort CSS properties in file with :SortCSS
command! SortCSS :g#\({\n\)\@<=#.,/}/sort

" Fix overflow ( above 80 )
command! FixOverflow :normal! gqap

" Set cwd to the one in open file
autocmd BufEnter * silent! lcd %:p:h



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
nnoremap p "+p`]
nnoremap P "+P`]
vnoremap y "+y
vnoremap Y "+Y
vnoremap p "+p
vnoremap P "+P

" Split navigation
nnoremap <s-l> <c-w>l
nnoremap <s-h> <c-w>h
nnoremap <s-k> <c-w>k
nnoremap <s-j> <c-w>j

" Easier indentation - does dot loose selection
vnoremap > >gv
vnoremap < <gv

" Navigaiion
nnoremap <Down> 10<c-y>
nnoremap <Up> 10<c-e>





"                            Leader key maps                           "
"                    ==============================                    "


" Quick save an quit
nnoremap <silent><Leader><Leader> :w<cr>
nnoremap <silent><Leader>q :q<cr>
nnoremap <silent><Leader>w :w<cr>

" Split like a boss
nnoremap <silent><Leader>v :vsplit\|:Startify<cr>
nnoremap <silent><Leader>h :sp<cr>

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
nnoremap <silent> ~ :ZoomToggle<CR> \| :normal! 0<cr>

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
nnoremap ,, :MRUFzf<CR>
nnoremap ,e :FZF<cr>
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

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
" autocmd CompleteDone * pclose
set completeopt+=noinsert
set completeopt-=preview

" Fugitive
nnoremap <silent><leader>g :Gstatus\|normal!gg8j<cr>
command! Gl normal! :!git vhm<cr>

" Neomake linting
" autocmd! BufWritePost,BufEnter * Neomake
" let g:neomake_javascript_enabled_makers = ['eslint']

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
let g:airline_symbols.branch = 'Y'
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

" LaTeX
autocmd Filetype tex setl updatetime=1
let g:livepreview_previewer = 'open -a Preview'

" Drag Visuals
vmap <unique> <up>    <Plug>SchleppUp
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight
vmap <unique> D <Plug>SchleppDup
" let g:Schlepp#allowSquishingLines = 1
" let g:Schlepp#allowSquishingBlocks = 1

" Nerd comment ( just out of habbit )
let g:NERDSpaceDelims = 1
nnoremap <silent> gc :call NERDComment(0,"toggle")<CR>
vnoremap <silent> gc :call NERDComment(0,"toggle")<CR>

" Vue stuff
autocmd FileType vue syntax sync fromstart
autocmd BufRead,BufNewFile *.vue setlocal filetype=vue.html.javascript.css
let g:ft = ''
function! NERDCommenter_before()
    if &ft == 'vue'
        let g:ft = 'vue'
        let stack = synstack(line('.'), col('.'))
        if len(stack) > 0
            let syn = synIDattr((stack)[0], 'name')
            if len(syn) > 0
                exe 'setf ' . substitute(tolower(syn), '^vue_', '', '')
            endif
        endif
    endif
endfunction
function! NERDCommenter_after()
    if g:ft == 'vue'
        setf vue
        let g:ft = ''
    endif
endfunction

" Limelight
let g:limelight_conceal_ctermfg=0

" Quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
let g:qs_first_occurrence_highlight_color = 155
let g:qs_second_occurrence_highlight_color = 81

" Ale
let g:ale_sign_column_always = 1
let g:ale_sign_error = ':x'
let g:ale_sign_warning = ':!'
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']

" Smooth scrolling
let g:comfortable_motion_scroll_down_key = "j"
let g:comfortable_motion_scroll_up_key = "k"
let g:comfortable_motion_interval = 50.0
let g:comfortable_motion_friction = 0.0
let g:comfortable_motion_air_drag = 4.0

" Scratch buffer
nnoremap <silent><leader>s :Scratch<cr>
let g:scratch_height = 20
let g:scratch_top = 1
" let g:scratch_persistenc_file='~/.vim/scratch'
let g:scratch_persistence_file = '/tmp/scratch'
let g:scratch_autohide = &hidden
let g:scratch_filetype = 'yaml'
