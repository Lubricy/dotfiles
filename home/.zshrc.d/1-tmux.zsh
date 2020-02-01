if which tmux >/dev/null 2>&1; then
    #if not inside a tmux session, and if no session is started, start a new session
    test -z "$SSH_CONNECTION" && test -z "$TMUX" && (tmux attach || tmux new-session)

    # when quitting tmux, try to attach
    while (test -z "$SSH_CONNECTION" && test -z ${TMUX}); do
        tmux attach || break
    done
fi
