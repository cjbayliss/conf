-- diagnostics config
vim.diagnostic.config({
    -- nice icons
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = '',
            [vim.diagnostic.severity.WARN] = '',
            [vim.diagnostic.severity.HINT] = '',
            [vim.diagnostic.severity.INFO] = '',
        },
    },
    -- only show when on line
    virtual_lines = { only_current_line = true },
    virtual_text = {
        format = function(diagnostic)
            if diagnostic.severity == vim.diagnostic.severity.ERROR then
                return string.format('E: %s', diagnostic.message)
            end
            if diagnostic.severity == vim.diagnostic.severity.WARN then
                return string.format('W: %s', diagnostic.message)
            end
            if diagnostic.severity == vim.diagnostic.severity.HINT then
                return string.format('H: %s', diagnostic.message)
            end
            if diagnostic.severity == vim.diagnostic.severity.INFO then
                return string.format('I: %s', diagnostic.message)
            end
            return diagnostic.message
        end,
        prefix = '',
    },
})

require('lspconfig').clangd.setup({})

require('lspconfig').lua_ls.setup({
    settings = {
        Lua = {
            runtime = { version = 'LuaJIT' },
            diagnostics = { globals = { 'vim', 'use' } },
            workspace = {
                library = vim.api.nvim_get_runtime_file('', true),
                checkThirdParty = false,
            },
            telemetry = { enable = false },
        },
    },
})

require('lspconfig').pylsp.setup({})
require('lspconfig').rust_analyzer.setup({})
