# only execute this file once per shell.
set -q __fish_config_sourced; and exit
set -g __fish_config_sourced 1

status is-login; and begin
    if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]
        exec sx
    end
end

status is-interactive; and begin
    set fish_greeting

    # aliases
    alias ls 'ls --hyperlink --color=auto'

    # allow urls with '?' in them
    set -U fish_features qmark-noglob

    # colours
    set -U fish_color_autosuggestion brblue
    set -U fish_color_cancel -r
    set -U fish_color_command white --bold
    set -U fish_color_comment brblue
    set -U fish_color_cwd brcyan
    set -U fish_color_cwd_root red
    set -U fish_color_end brmagenta
    set -U fish_color_error brred
    set -U fish_color_escape brcyan
    set -U fish_color_history_current --bold
    set -U fish_color_host normal
    set -U fish_color_match --background=brblue
    set -U fish_color_normal normal
    set -U fish_color_operator normal
    set -U fish_color_param normal
    set -U fish_color_quote yellow
    set -U fish_color_redirection bryellow
    set -U fish_color_search_match bryellow '--background=brblack'
    set -U fish_color_selection white --bold '--background=brblack'
    set -U fish_color_status red
    set -U fish_color_user green
    set -U fish_color_valid_path --underline
    set -U fish_pager_color_completion normal
    set -U fish_pager_color_description yellow
    set -U fish_pager_color_prefix white --bold --underline
    set -U fish_pager_color_progress -r white

    alias ps "echo \"don't you mean procs(1)?\""

    # man colours
    export LESS_TERMCAP_mb="$(tput bold; tput setaf 1)"
    export LESS_TERMCAP_md="$(tput bold; tput setaf 1)"
    export LESS_TERMCAP_me="$(tput sgr0)"
    export LESS_TERMCAP_so="$(tput bold; tput setaf 12)"
    export LESS_TERMCAP_se="$(tput sgr0)"
    export LESS_TERMCAP_us="$(tput bold; tput setaf 14)"
    export LESS_TERMCAP_ue="$(tput sgr0)"
    # required for man colours to work
    export GROFF_NO_SGR=1

    # prompt
    function __git_branch
        git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/'
    end

    function __git_status
        if [ (git ls-files 2>/dev/null | wc -l) -lt 2000 ]
            git status --short 2>/dev/null | sed 's/^ //g' | cut -d' ' -f1 | sort -u | tr -d '\n' | sed 's/^/ /'
        else
            printf " [NOSTAT]"
        end
    end

    function fish_right_prompt
        set -l last_status $status
        if [ $last_status -ne 0 ]
            set_color --bold $fish_color_error
            printf '%s ' $last_status
            set_color normal
        end
    end

    function fish_prompt
        # host
        set_color normal
        printf '%s ' (prompt_hostname)

        # pwd
        set_color $fish_color_cwd
        echo -n (prompt_pwd)
        set_color normal

        # git stuff
        set_color brmagenta
        printf '%s' (__git_branch)
        set_color magenta
        printf '%s ' (__git_status)
        set_color normal

        # prompt delimiter
        echo -n '» '
    end

    # env vars
    # set -gx CUDA_CACHE_PATH '/home/cjb/.local/cache/nv'

    set -gx SUDO_EDITOR hx
    set -gx VISUAL hx
    set -gx EDITOR hx

    set -gx GTK2_RC_FILES '/home/cjb/.config/gtk-2.0/gtkrc'
    set -gx MANPAGER 'less --mouse --wheel-lines 3'
    set -gx MANWIDTH 72
    set -gx MOZ_GTK_TITLEBAR_DECORATION system
    set -gx MOZ_USE_XINPUT2 1
    # set -gx MYPY_CACHE_DIR '/home/cjb/.local/cache/mypy'

    set -gx NAME 'Christopher Bayliss'
    set -gx EMAIL 'cjbdev@icloud.com'

    # set -gx NPM_CONFIG_CACHE '/home/cjb/.local/cache/npm'
    set -gx NPM_CONFIG_TMP "$XDG_RUNTIME_DIR"'/npm'
    set -gx NPM_CONFIG_USERCONFIG '/home/cjb/.config/npm/config'
    set -gx PAGER cat
    set -gx PYTHONSTARTUP '/home/cjb/.config/python/startup.py'
    set -gx RIPGREP_CONFIG_PATH '/home/cjb/.config/ripgrep/ripgreprc'
    set -gx XCURSOR_SIZE 36
    set -gx XCURSOR_THEME Adwaita
    set -gx XDG_CONFIG_HOME '/home/cjb/.config'
    set -gx XDG_DATA_HOME '/home/cjb/.local/share'
    set -gx XDG_DESKTOP_DIR /home/cjb/stuff/desktop
    set -gx XDG_DOCUMENTS_DIR /home/cjb/stuff
    set -gx XDG_DOWNLOAD_DIR /home/cjb/downloads
    set -gx XDG_MUSIC_DIR /home/cjb/music
    set -gx XDG_PICTURES_DIR /home/cjb/pictures
    set -gx XDG_STATE_HOME '/home/cjb/.local/state'
    set -gx XDG_VIDEOS_DIR /home/cjb/videos
    set -gx PATH "$PATH"(test -n "$PATH" && echo ':' || echo)"$HOME"'/.local/bin:/home/cjb/.local/share/npm/bin'
end
