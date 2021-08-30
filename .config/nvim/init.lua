-- ensure packer is installed
local install_path = vim.fn.stdpath('data') ..
                         '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({
        'git', 'clone', 'https://github.com/wbthomason/packer.nvim',
        install_path
    })
    vim.api.nvim_command 'packadd packer.nvim'
    first_run = true
end

-- general config
vim.o.cursorline = true
vim.o.guicursor = ''
vim.o.number = true
vim.o.termguicolors = true

-- spaces please
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.softtabstop = 4
vim.o.tabstop = 4

-- set/unset keybinds
function nvSetKey(modes, key, command)
    for k, v in ipairs(modes) do
        vim.api.nvim_set_keymap(v, key, command, {noremap = true})
    end
end

local keys = {'<up>', '<down>', '<pageup>', '<pagedown>', '<left>', '<right>'}
for k, v in ipairs(keys) do nvSetKey({'', 'i'}, v, '') end

-- manage packages with packer, the use-package of neovim
require('packer').startup(function()

    use 'wbthomason/packer.nvim'

    use {
        'Mofiqul/vscode.nvim',
        config = function()
            vim.g.vscode_style = "dark"
            vim.cmd('colorscheme vscode')
        end
    }

    use {'nacro90/numb.nvim', config = function() require('numb').setup() end}
    use {
        'ethanholz/nvim-lastplace',
        config = function() require('nvim-lastplace').setup() end
    }

    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        config = function() require('gitsigns').setup() end
    }

    -- the whole reason to use neovim
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
            require('nvim-treesitter.configs').setup {
                ensure_installed = "all",
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false
                }
            }
        end
    }

    if first_run then require('packer').sync() end
end)
