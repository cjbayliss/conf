maki.setup({
    always_yolo = true,
    always_thinking = "high",
    provider = {
        default_model = "synthetic/hf:zai-org/GLM-5.3-Flash",
    },
    ui = {
        splash_animation = false,
        theme = "terminal",
        thinking_lines = -1,
    },
    plugins = {
        memory = { enabled = false },
    },
})

maki.keymap.set({ "i" }, "<C-d>", function()
    maki.api.run_command("/exit")
end, { desc = "exit maki" })
