{lib, ...}:
{
  repo,
  mappings ? {},
  extraMappings ? {},
  hooks ? {}
}:
let
  buildLinkScript = lib.attrsets.mapAttrsToList
    (path: link: ''
      if [ -e "$repoPath/${path}" ]; then
        if [ ! -e "${link}" ]; then
          mkdir -p $(dirname "${link}")
          ln -s "$repoPath/${path}" "${link}"
        else
          echo "'${link}' exists, Skipping..."
        fi
      else
        echo "'$repoPath' does not contain '${path}', Skipping..."
      fi
    '');
  linkFiles = lib.strings.concatLines (
    (if lib.lists.length (builtins.attrNames mappings) == 0
     then [
       ''
        if [ -d "$repoPath/home" ]; then
          for file in $(find "$repoPath/home" -type l -print); do
            link="$HOME/$(removePrefix "$repoPath/home/" "$file")"
            if [ ! -e "$link" ]; then
              mkdir -p $(dirname "$link")
              ln -s "$file" "$link"
            fi
          done
        fi
      ''
     ]
     else buildLinkScript mappings
    ) ++ buildLinkScript extraMappings);
in environment: ''
run cat <<'EOF' | bash -eo pipefail
  source ${environment}
  removePrefix() {
    local prefix="$1"
    local string="$2"
    echo "''${string#$prefix}"
  }

  main() {
    dest="$HOME/.config/repos"
    repoPath="$dest/${repo.name}"

    if [ ! -d "$repoPath" ]; then
      mkdir -p $dest
      git clone "${repo.url}" "$repoPath"
      pushd "$repoPath"
      ${hooks.postClone or ""}
      popd
    else
      echo "$repoPath exists. Skipping..."
    fi
    ${linkFiles}
  }

  main
EOF
''
