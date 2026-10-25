maki.setup({
    always_yolo = true,
    always_thinking = "high",
    provider = {
        default_model = "synthetic/hf:deepseek-ai/DeepSeek-V4.1-Flash-1017",
    },
    ui = {
        splash_animation = false,
        theme = "terminal",
    },
    plugins = {
        memory = { enabled = false },
    },
})

-- Semble semantic code search, exposed as tools by the `semble` MCP server
-- configured in mcp.toml.
maki.api.register_prompt_hint({
    slot = "tool_usage",
    content = table.concat({
        "- `semble__search` searches this repository semantically. Pass the project root (your working directory) as `repo`.",
        '- Prefer it over grep for conceptual or cross-file questions ("where is auth validated?"); use grep/glob for exact strings, symbols, and filenames.',
        "- Search once, then open the reported file at the reported line instead of grepping for the same text.",
        "- `semble__find_related` takes a file path and a 1-indexed line and returns semantically similar code.",
    }, "\n"),
})

maki.keymap.set("n", "<C-d>", function()
    maki.api.run_command("/exit")
end, { desc = "exit maki" })
