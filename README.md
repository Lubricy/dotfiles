# installation

## 1. install nix
https://github.com/DeterminateSystems/nix-installer

## 2. config local-vars

mkdir -p ~/.config
nix flake new ~/.config/nix-local-vars
nix registry add nix-local-vars ~/.config/nix-local-vars
nix flake update local-vars

## 3. install!
nix run 'github:LnL7/nix-darwin' -- --switch --flake . 

