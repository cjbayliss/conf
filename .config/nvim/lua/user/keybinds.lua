-- general keybindings
vim.api.nvim_set_keymap('', 'U', '<cmd>redo<CR>', { noremap = true, desc = 'Go to next buffer' })
vim.api.nvim_set_keymap('', 'gn', '<cmd>bnext<CR>', { noremap = true, desc = 'Go to next buffer' })
vim.api.nvim_set_keymap('', 'gp', '<cmd>bprevious<CR>', { noremap = true, desc = 'Go to previous buffer' })
