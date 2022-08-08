-- general config
vim.o.cursorline = true
vim.o.guicursor = ''
vim.o.hidden = true
vim.o.number = true
vim.o.termguicolors = true
vim.o.laststatus = 0

-- spaces please
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.softtabstop = 4

-- clear most colours
vim.api.nvim_exec([[
au VimEnter * hi clear ColorColumn
au VimEnter * hi clear Comment
au VimEnter * hi clear Conceal
au VimEnter * hi clear Constant
au VimEnter * hi clear Cursor
au VimEnter * hi clear CursorColumn
au VimEnter * hi clear CursorLine
au VimEnter * hi clear CursorLineNr
au VimEnter * hi clear DiffAdd
au VimEnter * hi clear DiffChange
au VimEnter * hi clear DiffDelete
au VimEnter * hi clear DiffText
au VimEnter * hi clear Directory
au VimEnter * hi clear Error
au VimEnter * hi clear ErrorMsg
au VimEnter * hi clear FloatShadow
au VimEnter * hi clear FloatShadowThrough
au VimEnter * hi clear FoldColumn
au VimEnter * hi clear Folded
au VimEnter * hi clear Identifier
au VimEnter * hi clear Ignore
au VimEnter * hi clear IncSearch
au VimEnter * hi clear LineNr
au VimEnter * hi clear MatchParen
au VimEnter * hi clear ModeMsg
au VimEnter * hi clear MoreMsg
au VimEnter * hi clear MsgArea
au VimEnter * hi clear NonText
au VimEnter * hi clear Normal
au VimEnter * hi clear NormalNC
au VimEnter * hi clear NvimInternalError
au VimEnter * hi clear Pmenu
au VimEnter * hi clear PmenuSbar
au VimEnter * hi clear PmenuSel
au VimEnter * hi clear PmenuThumb
au VimEnter * hi clear PreProc
au VimEnter * hi clear Question
au VimEnter * hi clear RedrawDebugClear
au VimEnter * hi clear RedrawDebugComposed
au VimEnter * hi clear RedrawDebugNormal
au VimEnter * hi clear RedrawDebugRecompose
au VimEnter * hi clear Search
au VimEnter * hi clear SignColumn
au VimEnter * hi clear Special
au VimEnter * hi clear SpecialKey
au VimEnter * hi clear SpellBad
au VimEnter * hi clear SpellCap
au VimEnter * hi clear SpellLocal
au VimEnter * hi clear SpellRare
au VimEnter * hi clear Statement
au VimEnter * hi clear StatusLine
au VimEnter * hi clear StatusLineNC
au VimEnter * hi clear TabLine
au VimEnter * hi clear TabLineFill
au VimEnter * hi clear TabLineSel
au VimEnter * hi clear TermCursor
au VimEnter * hi clear TermCursorNC
au VimEnter * hi clear Title
au VimEnter * hi clear Todo
au VimEnter * hi clear Type
au VimEnter * hi clear Underlined
au VimEnter * hi clear VertSplit
au VimEnter * hi clear Visual
au VimEnter * hi clear VisualNC
au VimEnter * hi clear WarningMsg
au VimEnter * hi clear WildMenu
au VimEnter * hi clear lCursor

au VimEnter * hi! link Delimiter Normal
au VimEnter * hi! link TSConstructor Normal
au VimEnter * hi! link TSVariableBuiltin Special
au VimEnter * hi! link haskellTSConstructor Type

au VimEnter * hi Normal guifg=#ffffff
au VimEnter * hi Comment guifg=#a8a8a8 cterm=italic gui=italic

au VimEnter * hi Constant guifg=#00bcff
au VimEnter * hi CursorLine guibg=#151823
au VimEnter * hi CursorLineNr guibg=#323232 guifg=#ffffff gui=bold
au VimEnter * hi Identifier guifg=#feacd0
au VimEnter * hi LineNr guibg=#100f10 guifg=#a8a8a8
au VimEnter * hi MatchParen guibg=#6f3355 guifg=#ffffff
au VimEnter * hi PreProc guifg=#ff9077
au VimEnter * hi Special guifg=#f78fe7
au VimEnter * hi Statement guifg=#b6a0ff
au VimEnter * hi Operator guifg=#00d3d0
au VimEnter * hi StatusLine gui=bold guibg=#323232 guifg=#f4f4f4
au VimEnter * hi String guifg=#79a8ff
au VimEnter * hi Type guifg=#6ae4b9
au VimEnter * hi Todo gui=italic,bold guifg=#dbbe5f

au VimEnter * hi! link Search Visual
au VimEnter * hi IncSearch guibg=#006800 guifg=#ffffff
au VimEnter * hi Visual guibg=#004065 guifg=#8ae4f2

au VimEnter * hi Pmenu guibg=#100f10
au VimEnter * hi! link PmenuSel Search
au VimEnter * hi PmenuSbar guibg=#100f10
au VimEnter * hi PmenuThumb guibg=#323232
]], false)

-- unset arrow keys
for _, key in ipairs({'<up>', '<down>', '<left>', '<right>'}) do
    for _, mode in ipairs({'', 'i'}) do
        vim.api.nvim_set_keymap(mode, key, '', {noremap = true})
    end
end

-- manage packages with packer, the use-package of neovim
::PackerConfig::
local status, packer = pcall(require, 'packer')
if not (status) then
    vim.fn.system({
        'git', 'clone', 'https://github.com/wbthomason/packer.nvim',
        vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
    })
    vim.api.nvim_command('packadd packer.nvim')
    firstRun = true
    goto PackerConfig
end

packer.startup(function()
    use {
        'wbthomason/packer.nvim',
        config = function()
            vim.api.nvim_exec([[
              augroup Packer
                autocmd!
                autocmd BufWritePost init.lua PackerCompile
              augroup end]], false)
        end
    }

    use {
        'ethanholz/nvim-lastplace',
        config = function() require('nvim-lastplace').setup() end
    }

    use {
        'f-person/git-blame.nvim',
        config = function()
            vim.g.gitblame_message_template = '<sha>, <date>, <author>'
        end
    }

    -- remove trailing whitespace automatically
    use {
        'lewis6991/spaceless.nvim',
        config = function() require('spaceless').setup() end
    }

    use {
        'norcalli/nvim-colorizer.lua',
        config = function()
            require('colorizer').setup({'*', '!rst', '!vimwiki'})
        end
    }

    -- fennel
    use {'Olical/conjure', ft = 'fennel'}
    use {
        'Olical/aniseed',
        requires = {'Olical/conjure'},
        config = function() vim.api.nvim_command('packadd packer.nvim') end
    }

    -- the whole reason to use neovim
    use {
        'nvim-treesitter/nvim-treesitter',
        requires = {'nvim-treesitter/nvim-treesitter-refactor'},
        run = ':TSUpdate',
        config = function()
            vim.o.updatetime = 50
            require('nvim-treesitter.configs').setup({
                ensure_installed = 'all',
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false
                },
                refactor = {
                    highlight_definitions = {enable = true},
                    smart_rename = {enable = true},
                    navigation = {enable = true}
                }
            })

            function jump_to_def()
                local ts_utils = require('nvim-treesitter.ts_utils')
                local locals = require('nvim-treesitter.locals')
                local buf = vim.api.nvim_get_current_buf()
                local point = ts_utils.get_node_at_cursor()

                if not point then return end

                local _, _, kind = locals.find_definition(point, buf)
                local def = locals.find_definition(point, buf)

                if tostring(kind) == 'nil' then
                    local name = ts_utils.get_node_text(point, nil)[1]
                    vim.cmd(string.format('tag %s', name))
                else
                    ts_utils.goto_node(def)
                end
            end

            vim.api.nvim_set_keymap('n', 'gd', '<cmd>lua jump_to_def()<CR>',
                                    {noremap = true})
        end
    }

    -- -- vimwiki using MediaWiki syntax
    -- use {
    --     'vimwiki/vimwiki',
    --     config = function()
    --         vim.g.vimwiki_list = {
    --             {auto_export = 1, path_html = '~/wiki', path = '~/wiki'}
    --         }
    --     end
    -- }

    if firstRun then packer.sync() end
end)
