set -U fish_greeting

set -gx DISABLE_TELEMETRY 1

set -gx EDITOR "kak"
set -gx VISUAL "kak"

set -gx PAGER "cat"
set -gx MANWIDTH 72
set -gx MANPAGER "less --mouse --wheel-lines 3"

set -gx PATH "$PATH:$HOME/.local/bin:$HOME/.cargo/bin"

set -gx NAME "Christopher Bayliss"
set -gx EMAIL "cjbdev@icloud.com"

if [ -f /opt/homebrew/bin/brew ]
    set -gx HOMEBREW_ASK 1

    eval (/opt/homebrew/bin/brew shellenv fish)

    if test -d (brew --prefix)"/share/fish/completions"
        set -p fish_complete_path (brew --prefix)/share/fish/completions
    end
    if test -d (brew --prefix)"/share/fish/vendor_completions.d"
        set -p fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
    end
end

if status is-interactive
    set -gx GPG_TTY (tty)
    set -gx SHELL (command -v fish)

    function fish_prompt
        string join '' -- (prompt_hostname) ' ' (set_color brcyan) (prompt_pwd) (set_color --reset) ' $ '
    end
end
