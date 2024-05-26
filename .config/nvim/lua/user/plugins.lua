local paqpath = vim.fn.stdpath('data') .. 'site/pack/paqs/start/paq-nvim'
if not (vim.uv or vim.loop).fs_stat(paqpath) then
  vim.fn.system({
    'git',
    'clone',
    '--depth=1',
    'https://github.com/savq/paq-nvim.git',
    paqpath,
  })
end
vim.opt.rtp:prepend(paqpath)

require('paq') {
    'folke/which-key.nvim', -- suggest keys that are in a chain
    'lewis6991/gitsigns.nvim', -- git status in the margin
    'lewis6991/spaceless.nvim', -- auto remove tailing spaces
    'neovim/nvim-lspconfig', -- predefined lsp configs
    'savq/paq-nvim', -- let paq manage itself
    'windwp/nvim-autopairs', -- auto insert matching pair
    'nvim-treesitter/nvim-treesitter', -- sementic syntax highlighting and more
}

require('gitsigns').setup({
    preview_config = {
        border = {
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
            { '', 'floatborder' },
        },
        row = 1,
        col = 0,
    },
})

require('nvim-autopairs').setup({})

require('nvim-treesitter.configs').setup({
    auto_install = true,
    ensure_installed = {
        'bash',
        'c',
        'cpp',
        'diff',
        'git_config',
        'git_rebase',
        'gitcommit',
        'gitignore',
        'lua',
        'python',
        'vim',
        'vimdoc',
    },
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false
    },
    ignore_install = {},
    modules = {},
    sync_install = true,
})

require('which-key').setup({})
