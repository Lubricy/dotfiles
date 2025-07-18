#compdef nixos-spec
# This is a template for nixos-spec zsh completions.

_nixos_spec() {
    local -a commands
    local -a spec_names

    local nixos_spec_cmd="nixos-spec"

    commands=(
        'list:List available specialisations'
        'l:Alias for list'
        'activate:Activate a specialisation'
        'a:Alias for activate'
        'reset:Reset to the base configuration'
        'r:Alias for reset'
        'status:Show current status'
        's:Alias for status'
    )
    spec_names=($($nixos_spec_cmd __list-names-for-completion))

    # Zsh's way of handling argument position
    if (( CURRENT == 2 )); then
        # Complete both commands and spec names at the first position
        _alternative \
            "cmds:command:(($commands))" \
            "specs:specialisation:($spec_names)"
    elif (( CURRENT == 3 && ($words[2] == "activate" || $words[2] == "a") )); then
        # Complete only spec names after 'activate'
        _describe 'specialisation' spec_names
    fi
}

_nixos_spec
