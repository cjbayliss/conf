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
        run = ':TSUpdate',
        config = function()
            require('nvim-treesitter.configs').setup({
                ensure_installed = 'all',
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false
                }
            })
        end
    }
    if firstRun then packer.sync() end
end)
