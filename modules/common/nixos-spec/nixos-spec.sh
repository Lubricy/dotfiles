#!/usr/bin/env bash
# nixos-spec: A helper script to manage NixOS specialisations.
set -euo pipefail

# --- DYNAMIC PROFILE DETECTION ---
# Find the canonical path of the script itself.
SCRIPT_PATH="${BASH_SOURCE[0]}"
# The base profile is three directories above the script (e.g., .../profile/sw/bin/script -> .../profile)
# This is a standard and reliable way for an executable in a Nix store path to find its root.
DEFAULT_BASE_PROFILE="$(realpath "$(dirname "$(dirname "$(dirname "${SCRIPT_PATH}")")")")"
BASE_SYSTEM_PROFILE="$DEFAULT_BASE_PROFILE" # Initialize with the default

# Static path to the live system symlink (for status comparison).
RUNNING_SYSTEM="/run/current-system"

# --- Helper Functions (No changes here) ---

usage() {
    # ... (usage text is unchanged)
    echo "Usage: $0 [OPTIONS] [COMMAND]"
    echo
    echo "A helper to manage NixOS specialisations."
    echo
    echo "OPTIONS:"
    echo "  -b, --system-base      Force use of the live system profile (/nix/var/nix/profiles/system)."
    echo "  -B, --base-dir <path>  Manually specify the base profile directory to operate on."
    echo "  -h, --help             Show this help message."
    echo
    echo "COMMANDS:"
    echo "  <spec_name>          Alias to activate a named specialisation."
    echo "  (no command)         Show current status and list all available specialisations."
    echo "  list, l              Show current status and list all available specialisations."
    echo "  activate, a <name>   Activate a named specialisation."
    echo "  status, s            Show the currently active specialisation, if any."
    echo "  reset, r             Reset to the base system configuration."
    exit 0
}

get_active_spec_name() {
    # ... (function is unchanged)
    local running_path; running_path=$(readlink -f "${RUNNING_SYSTEM}")
    local base_path; base_path=$(readlink -f "${BASE_SYSTEM_PROFILE}")
    if [ "${running_path}" == "${base_path}" ]; then echo "base"; return; fi
    local specialisations_dir="${BASE_SYSTEM_PROFILE}/specialisation"
    if [ -d "${specialisations_dir}" ]; then
        for spec_path in "${specialisations_dir}"/*; do
            if [ -d "${spec_path}" ]; then
                local current_spec_real_path; current_spec_real_path=$(readlink -f "${spec_path}")
                if [ "${running_path}" == "${current_spec_real_path}" ]; then
                    basename "${spec_path}";
                    return;
                fi
            fi
        done
    fi
    echo "unknown"
}

show_status() {
    local active_spec; active_spec=$(get_active_spec_name)
    if [ "${active_spec}" == "base" ]; then
        echo "Live system is running the base of the selected profile.";
    elif [ "${active_spec}" == "unknown" ]; then
        echo "Warning: Live system is running an unknown or different configuration.";
    else
        echo "Live system is running the '${active_spec}' specialisation of the selected profile.";
    fi
}

list_specs() {
    echo "Operating on profile: ${BASE_SYSTEM_PROFILE}"
    echo "--------------------------------------------------"
    show_status;
    echo
    echo "Available specialisations:"
    local specialisations_dir="${BASE_SYSTEM_PROFILE}/specialisation"
    if [ ! -d "${specialisations_dir}" ] || [ -z "$(ls -A "${specialisations_dir}")" ]; then
        echo "  No specialisations are defined in this profile.";
        return;
    fi
    local active_spec; active_spec=$(get_active_spec_name)
    for spec in "${specialisations_dir}"/*; do
        if [ -d "${spec}" ]; then
            local spec_name; spec_name=$(basename "${spec}")
            if [ "${spec_name}" == "${active_spec}" ]; then
                echo "  * ${spec_name} (live)";
            else
                echo "  - ${spec_name}";
            fi
        fi
    done
}

activate_spec() {
    # ... (function is unchanged)
    local spec_name="$1";
    if [ -z "${spec_name}" ]; then
        echo "Error: Specialisation name not provided." >&2; exit 1;
    fi
    local spec_path="${BASE_SYSTEM_PROFILE}/specialisation/${spec_name}"
    local switch_script="${spec_path}/bin/switch-to-configuration"
    if [ ! -d "${spec_path}" ] || [ ! -f "${switch_script}" ]; then
        echo "Error: Specialisation '${spec_name}' not found in profile '${BASE_SYSTEM_PROFILE}'." >&2;
        exit 1;
    fi
    echo "Activating specialisation '${spec_name}'...";
    sudo "${switch_script}" test;
    echo "Successfully switched to specialisation '${spec_name}'."
}

reset_to_base() {
    # ... (function is unchanged)
    local base_switch_script="${BASE_SYSTEM_PROFILE}/bin/switch-to-configuration"
    if [ ! -f "${base_switch_script}" ]; then
        echo "Error: Could not find the base system's switch-to-configuration script in '${BASE_SYSTEM_PROFILE}'." >&2;
        exit 1;
    fi
    echo "Resetting to the base configuration of '${BASE_SYSTEM_PROFILE}'...";
    sudo "${base_switch_script}" test;
    echo "Successfully reset to the base configuration."
}


# --- MAIN LOGIC ---

# 1. Two-phase argument parsing.
# Phase 1: Iterate through all arguments, separating flags from commands.
commands=()
while [[ "$#" -gt 0 ]]; do
    case "$1" in
        -h|--help)
            usage
            ;;
        -b|--system-base)
            BASE_SYSTEM_PROFILE="/nix/var/nix/profiles/system"
            shift # consume flag
            ;;
        -B|--base-dir)
            if [ -n "$2" ]; then
                BASE_SYSTEM_PROFILE="$2"
                shift 2 # consume flag and its value
            else
                echo "Error: Option '$1' requires an argument." >&2
                exit 1
            fi
            ;;
        *)
            # This is not a flag, so add it to our list of commands.
            commands+=("$1")
            shift # consume the command
            ;;
    esac
done

# Phase 2: Restore the positional parameters ($1, $2, etc.) to only contain the commands.
set -- "${commands[@]}"

# 2. Validate that the final profile directory exists.
if [ ! -d "$BASE_SYSTEM_PROFILE" ]; then
    echo "Error: Base profile not found or not a directory: $BASE_SYSTEM_PROFILE" >&2
    exit 1
fi

# 3. If no command is left after parsing flags, default to list.
if [ "$#" -eq 0 ]; then
    list_specs
    exit 0
fi

# This function is for machine consumption.
# It outputs only the raw names, one per line.
list_spec_names_for_completion() {
    local specialisations_dir="${BASE_SYSTEM_PROFILE}/specialisation"
    if [ ! -d "${specialisations_dir}" ] || [ -z "$(ls -A "${specialisations_dir}")" ]; then
        return
    fi
    for spec in "${specialisations_dir}"/*; do
        if [ -d "${spec}" ]; then
            basename "${spec}"
        fi
    done
}

# 4. Process the remaining arguments as commands.
case "$1" in
    list|l)
        list_specs
        ;;
    status|s)
        show_status
        ;;
    activate|a)
        shift
        activate_spec "$1"
        ;;
    reset|r)
        reset_to_base
        ;;
    __list-names-for-completion)
        list_spec_names_for_completion
        ;;
    *)
        if [ "$#" -eq 1 ]; then
            activate_spec "$1"
        else
            echo "Error: Unknown command or invalid arguments." >&2
            usage
        fi
        ;;
esac
