-- general config
vim.g.mapleader = ' '
vim.o.cursorline = true
vim.o.guicursor = 'a:hor20,i:ver20,a:blinkwait300-blinkon200-blinkoff150'
vim.o.hidden = true
vim.o.laststatus = 0
vim.o.number = true
vim.o.signcolumn = 'yes'
vim.o.textwidth = 72

-- folding
vim.o.foldenable = false
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
vim.o.foldmethod = 'expr'
vim.o.foldnestmax = 1

-- better cmdline
vim.o.wildoptions = 'pum,fuzzy,tagfile'
vim.o.ignorecase = true

-- spaces please
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.softtabstop = 4

-- undo these format options
vim.api.nvim_create_autocmd({ 'FileType' }, {
    command = 'setlocal formatoptions-=o',
})

require('user.theme')
require('user.keybinds')
require('user.lsp.config')
require('user.plugins')
