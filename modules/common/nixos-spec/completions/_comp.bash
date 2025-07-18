# This is a template for nixos-spec bash completions.

_nixos_spec_completions() {
    local cur prev words cword
    _get_comp_words_by_ref -n : cur prev words cword

    # Define the main subcommands
    local commands="list l activate a reset r status s"

    # Complete subcommands if we are at the second argument
    if [[ ''${cword} -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "''${commands}" -- "''${cur}") )
        return
    fi

    # Complete specialisation names if the previous command was 'activate' or 'a'
    # or if we are completing a standalone spec name.
    case "''${prev}" in
        activate|a)
            local spec_names=$(nixos-spec __list-names-for-completion)
            COMPREPLY=( $(compgen -W "''${spec_names}" -- "''${cur}") )
            return
            ;;
        *)
            # Also complete if we are at the first command and it's not a known subcommand.
            if [[ ''${cword} -eq 1 ]]; then
                local is_known_command=0
                for cmd in ''${commands}; do
                    if [[ "''${cmd}" == "''${cur}"* ]]; then
                        is_known_command=1
                        break
                    fi
                done
                if [[ ''${is_known_command} -eq 0 ]]; then
                    local spec_names=$(nixos-spec __list-names-for-completion)
                    COMPREPLY=( $(compgen -W "''${spec_names}" -- "''${cur}") )
                fi
            fi
            ;;
    esac
}
# Register the completion function for the nixos-spec command.
complete -F _nixos_spec_completions nixos-spec
