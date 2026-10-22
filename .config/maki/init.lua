maki.setup({
    always_yolo = true,
    always_thinking = "adaptive",
    provider = {
        default_model = "synthetic/hf:deepseek-ai/DeepSeek-V4.1-Flash-1017",
    },
    ui = {
        splash_animation = false,
        theme = "terminal",
    },
})

maki.keymap.set("n", "<C-d>", function()
  maki.api.run_command("/exit")
end, { desc = "exit maki" })
