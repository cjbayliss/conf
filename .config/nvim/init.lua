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
vim.o.number = true
vim.o.termguicolors = true

-- manage packages with packer, the use-package of neovim
require('packer').startup(function()

    use 'wbthomason/packer.nvim'

    use {
        'ishan9299/modus-theme-vim',
        config = function()
            vim.g.modus_moody_enable = true
            vim.cmd('colorscheme modus-vivendi')
        end
    }

    use {
        'hoob3rt/lualine.nvim',
        config = function()
            -- I swear I only intended to make a few changes!!
            local custom_vivendi = require('lualine.themes.modus_vivendi')
            custom_vivendi.command.a.bg = "#2f2f2f"
            custom_vivendi.command.a.fg = "#E0CC00"
            custom_vivendi.command.b.bg = "#2f2f2f"
            custom_vivendi.command.b.fg = "#E0CC00"
            custom_vivendi.inactive.b.bg = "#2f2f2f"
            custom_vivendi.insert.a.bg = "#2f2f2f"
            custom_vivendi.insert.a.fg = "#6AE4B9"
            custom_vivendi.insert.b.bg = "#2f2f2f"
            custom_vivendi.insert.b.fg = "#6AE4B9"
            custom_vivendi.normal.a.bg = "#2f2f2f"
            custom_vivendi.normal.a.fg = "#79A8FF"
            custom_vivendi.normal.a.fg = "#FFFFFF"
            custom_vivendi.normal.b.bg = "#2f2f2f"
            custom_vivendi.normal.b.fg = "#FFFFFF"
            custom_vivendi.replace.b.bg = "#2f2f2f"
            custom_vivendi.visual.a.bg = "#2f2f2f"
            custom_vivendi.visual.a.fg = "#FEACD0"
            custom_vivendi.visual.b.bg = "#2f2f2f"
            custom_vivendi.visual.b.fg = "#FEACD0"
            require('lualine').setup {
                options = {
                    component_separators = {'•', '•'},
                    section_separators = {'', ''},
                    theme = custom_vivendi
                },
                sections = {
                    lualine_x = {{'o:encoding', upper = true}, {'filetype'}}
                }
            }
        end
    }

    use {
        'akinsho/nvim-bufferline.lua',
        config = function()
            require("bufferline").setup {
                options = {always_show_bufferline = false}
            }
        end
    }

    use {
        'TimUntersberger/neogit',
        requires = 'nvim-lua/plenary.nvim',
        config = function() require('neogit').setup() end
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
