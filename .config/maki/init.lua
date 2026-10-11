maki.setup({
    always_yolo = true,
    ui = {
        splash_animation = false,
        theme = "terminal",
    },
})

maki.keymap.set("n", "<C-d>", function()
  maki.api.run_command("/exit")
end, { desc = "exit maki" })
