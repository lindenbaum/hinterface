# This file contains a Nixpkgs overlay with 
# preconfigured text editors. 
#
# It contains neovim with Haskell and Nix support,
# and Emacs.
#
# To be included in ./overlay.nix
#
self: super: {
  emacs = (super.emacsPackagesGen super.emacs-nox).emacsWithPackages
    (epkgs: with epkgs;
    [
      notmuch
      erlang
      nix-mode
      zoom
      undo-tree
      zen-mode
      zen-and-art-theme
      company
      company-erlang
      company-c-headers
      company-shell
      magit
      magit-popup
      zenburn-theme
    ]
    );
  neovim = super.neovim.override {
    configure = {
      customRC =
        ''
          packloadall
          " TextEdit might fail if hidden is not set.
          set hidden

          " Some servers have issues with backup files, see #649.
          set nobackup
          set nowritebackup

          " sane search
          " set ignorecase
          set smartcase

          " Give more space for displaying messages.
          set cmdheight=2

          " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
          " delays and poor user experience.
          set updatetime=300

          " Don't pass messages to |ins-completion-menu|.
          set shortmess+=c

          " Always show the signcolumn, otherwise it would shift the text each time
          " diagnostics appear/become resolved.
          set signcolumn=yes

          " Set the coc-config
          call coc#config('languageserver', {
                  \  "haskell": {
                  \    "command": "haskell-language-server",
                  \    "args": ["--lsp"],
                  \    "rootPatterns": [ "*.cabal", ],
                  \    "filetypes": [ "hs", "lhs", "haskell" ],
                  \  }
                  \})
          nmap <leader>1 :call coc#config('diagnostic.messageTarget', 'echo')<CR>
          nmap <leader>2 :call coc#config('diagnostic.messageTarget', 'float')<CR>
          " Use tab for trigger completion with characters ahead and navigate.
          " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
          " other plugin before putting this into your config.
          inoremap <silent><expr> <TAB>
                \ pumvisible() ? "\<C-n>" :
                \ <SID>check_back_space() ? "\<TAB>" :
                \ coc#refresh()
          inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

          function! s:check_back_space() abort
            let col = col('.') - 1
            return !col || getline('.')[col - 1]  =~# '\s'
          endfunction

          " Use <c-space> to trigger completion.
          inoremap <silent><expr> <c-space> coc#refresh()

          " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
          " position. Coc only does snippet and additional edit on confirm.
          if exists('*complete_info')
            inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
          else
            imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
          endif

          " Use `[g` and `]g` to navigate diagnostics
          nmap <silent> [g <Plug>(coc-diagnostic-prev)
          nmap <silent> ]g <Plug>(coc-diagnostic-next)

          " GoTo code navigation.
          nmap <silent> gd <Plug>(coc-definition)
          nmap <silent> gy <Plug>(coc-type-definition)
          nmap <silent> gi <Plug>(coc-implementation)
          nmap <silent> gr <Plug>(coc-references)

          " Use K to show documentation in preview window.
          nnoremap <silent> K :call <SID>show_documentation()<CR>

          function! s:show_documentation()
            if (index(['vim','help'], &filetype) >= 0)
              execute 'h '.expand('<cword>')
            else
              call CocAction('doHover')
            endif
          endfunction

          " Highlight the symbol and its references when holding the cursor.
          autocmd CursorHold * silent call CocActionAsync('highlight')

          " Symbol renaming.
          nmap <leader>rn <Plug>(coc-rename)

          " Formatting selected code.
          xmap <leader>f  <Plug>(coc-format-selected)
          nmap <leader>f  <Plug>(coc-format-selected)

          augroup mygroup
            autocmd!
            " Setup formatexpr specified filetype(s).
            autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
            " Update signature help on jump placeholder.
            autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
          augroup end

          " Applying codeAction to the selected region.
          " Example: `<leader>aap` for current paragraph
          xmap <leader>a  <Plug>(coc-codeaction-selected)
          nmap <leader>a  <Plug>(coc-codeaction-selected)

          " Remap keys for applying codeAction to the current line.
          nmap <leader>ac  <Plug>(coc-codeaction)
          " Apply AutoFix to problem on the current line.
          nmap <leader>qf  <Plug>(coc-fix-current)

          " Introduce function text object
          " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
          xmap if <Plug>(coc-funcobj-i)
          xmap af <Plug>(coc-funcobj-a)
          omap if <Plug>(coc-funcobj-i)
          omap af <Plug>(coc-funcobj-a)

          " Use <TAB> for selections ranges.
          " NOTE: Requires 'textDocument/selectionRange' support from the language server.
          " coc-tsserver, coc-python are the examples of servers that support it.
          nmap <silent> <TAB> <Plug>(coc-range-select)
          xmap <silent> <TAB> <Plug>(coc-range-select)

          " Add `:Format` command to format current buffer.
          command! -nargs=0 Format :call CocAction('format')

          " Add `:Fold` command to fold current buffer.
          command! -nargs=? Fold :call     CocAction('fold', <f-args>)

          " Add `:OR` command for organize imports of the current buffer.
          command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

          " Add (Neo)Vim's native statusline support.
          " NOTE: Please see `:h coc-status` for integrations with external plugins that
          " provide custom statusline: lightline.vim, vim-airline.
          set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}
          set statusline+=%F

          let g:lightline = {
                \ 'colorscheme': 'wombat',
                \ 'active': {
                \   'left': [ [ 'mode', 'paste' ],
                \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ]
                \ },
                \ 'component_function': {
                \   'cocstatus': 'coc#status'
                \ },
                \ }
          " Use auocmd to force lightline update.
          autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()

          " Mappings using CoCList:
          " Show all diagnostics.
          nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
          " Manage extensions.
          nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
          " Show commands.
          nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
          " Find symbol of current document.
          nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
          " Search workspace symbols.
          nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
          " Do default action for next item.
          nnoremap <silent> <space>j  :<C-u>CocNext<CR>
          " Do default action for previous item.
          nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
          " Resume latest coc list.
          nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

          set termguicolors
          set diffopt=vertical,filler
          colorscheme morning
          " colorscheme PaperColor
          set list
          set listchars=tab:>-
          set expandtab
          set shiftwidth=2
          set softtabstop=2
          set tabstop=2

          " denite configuration
          call denite#custom#var('file/rec', 'command', ['${self.ripgrep}/bin/rg', '--files', '--glob', '!.git'])

          call denite#custom#var('grep', 'command', ['${self.ripgrep}/bin/rg'])
          call denite#custom#var('grep', 'default_opts',
          \ ['-i', '--vimgrep', '--no-heading'])
          call denite#custom#var('grep', 'recursive_opts', [])
          call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
          call denite#custom#var('grep', 'separator', ['--'])
          call denite#custom#var('grep', 'final_opts', [])
          call denite#custom#option('default', 'prompt', 'λ')


          nmap <leader>p :Denite -start-filter file/rec<CR>
          nmap <leader>b :Denite buffer<CR>
          nnoremap <leader>g :Denite grep<CR>
          nnoremap <leader>/ :DeniteCursorWord grep<CR>

          augroup mygroup2
            autocmd!
            autocmd FileType denite nnoremap <silent><buffer><expr> <CR>  denite#do_map('do_action')
            autocmd FileType denite nnoremap <silent><buffer><expr> d     denite#do_map('do_action', 'delete')
            autocmd FileType denite nnoremap <silent><buffer><expr> p     denite#do_map('do_action', 'preview')
            autocmd FileType denite nnoremap <silent><buffer><expr> <C-v> denite#do_map('do_action', 'vsplit')
            autocmd FileType denite nnoremap <silent><buffer><expr> <C-x> denite#do_map('do_action', 'split')
            autocmd FileType denite nnoremap <silent><buffer><expr> <Esc> denite#do_map('quit')
            autocmd FileType denite nnoremap <silent><buffer><expr> i     denite#do_map('open_filter_buffer')
            autocmd FileType denite nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
            autocmd FileType denite-filter imap <silent><buffer> <Esc> <Plug>(denite_filter_quit)
          augroup END

          nnoremap <A-j> :m .+1<CR>==
          nnoremap <A-k> :m .-2<CR>==
          inoremap <A-j> <Esc>:m .+1<CR>==gi
          inoremap <A-k> <Esc>:m .-2<CR>==gi
          vnoremap <A-j> :m '>+1<CR>gv=gv
          vnoremap <A-k> :m '<-2<CR>gv=gv
                
          if !exists("g:ormolu_command")
             let g:ormolu_command = "ormolu"
          endif

          if !exists("g:nixpkgs_fmt_command")
              let g:nixpkgs_fmt_command = "${self.nixpkgs-fmt}/bin/nixpkgs-fmt"
          endif

          "run ormolu on save
          function FormatBuffer()
            echom "formatter"
            if &modified && &filetype ==? "haskell"
              echom "haskell formatter"
              silent !clear
              let cursor_pos = getcurpos()
              let formatted_content = systemlist(g:ormolu_command, getbufline("%",0,"$"))
              if v:shell_error ==? 0
                normal ggdG
                call append(0, formatted_content)
                call setpos('.', cursor_pos)
              else
                echom ("Failed to format buffer: " . expand("%"))
              endif  
            elseif &modified && &filetype ==? "nix"
              echom "nix formatter"
              silent !clear
              let cursor_pos = getcurpos()
              let formatted_content = systemlist(g:nixpkgs_fmt_command, getbufline("%",0,"$"))
              if v:shell_error ==? 0
                normal ggdG
                call append(0, formatted_content)
                call setpos('.', cursor_pos)
              else
                echom ("Failed to format buffer: " . expand("%"))
              endif  
            endif
          endfunction

          augroup mygroup3
            autocmd!
            autocmd BufWritePre * :call FormatBuffer()
          augroup END

          set makeprg=cabal\ build\ --disable-optimization
          " mark undefined, error in error color
          syntax keyword indivError undefined error
          highlight link indivError Error
          augroup mygroup4
            autocmd!
            autocmd FileType haskell syntax keyword indivError undefined error
          augroup END

          set cursorline
          set cursorcolumn
        '';
      packages.myVimPackage = {
        # see examples below how to use custom packages
        start =
          with super.vimPlugins;
          [
            fugitive
            vim-nix
            coc-nvim
            denite
            denite-nvim
            denite-extra
            limelight-vim
            lightline-vim
            papercolor-theme
            vim-json
            vim-yaml
            multiple-cursors
            ghcid
          ];
        # If a vim plugin has a dependency that is not explicitly listed in
        # opt that dependency will always be added to start to avoid confusion.
        opt = [ ];
      };
    };
  };
}

