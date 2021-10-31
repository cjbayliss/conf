-- general config
vim.o.cursorline = true
vim.o.guicursor = ''
vim.o.hidden = true
vim.o.number = true
vim.o.termguicolors = true

-- spaces please
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.softtabstop = 4

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
    use 'wbthomason/packer.nvim'

    use {
        'Mofiqul/vscode.nvim',
        config = function()
            vim.g.vscode_style = 'dark'
            vim.cmd('colorscheme vscode')
            vim.api.nvim_command('highlight CursorLine guibg=#1f1f1f')
            vim.api.nvim_command(
                'highlight CursorLineNr guifg=#ffaf00 guibg=#1f1f1f')
            vim.api.nvim_command('highlight EndOfBuffer guifg=#D4D4D4')
            vim.api.nvim_command('highlight LineNr guibg=#151515')
            vim.api.nvim_command('highlight ModeMsg guibg=none')
            vim.api.nvim_command('highlight Normal guibg=#0f0f0f')
            vim.api.nvim_command('highlight NormalFloat guibg=#0f0f0f')
            vim.api.nvim_command('highlight SignColumn guibg=none')
            vim.api.nvim_command('highlight StatusLine guibg=#151515')
            vim.api.nvim_command('highlight StatusLineNC guibg=none')
            vim.api.nvim_command('highlight VertSplit guibg=none')
        end
    }

    use {
        'ethanholz/nvim-lastplace',
        config = function() require('nvim-lastplace').setup() end
    }

    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        config = function()
            require('gitsigns').setup({
                current_line_blame = true,
                current_line_blame_opts = {delay = 500}
            })
        end
    }

    -- remove trailing whitespace automatically
    use {
        'lewis6991/spaceless.nvim',
        config = function() require('spaceless').setup() end
    }

    use {
        'norcalli/nvim-colorizer.lua',
        config = function() require('colorizer').setup() end
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

    -- treesitter deals with the *current* file, use tags for other files
    use 'ludovicchabant/vim-gutentags'

    if firstRun then packer.sync() end
end)
