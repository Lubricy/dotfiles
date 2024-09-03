# installation

## 1. install nix

https://github.com/DeterminateSystems/nix-installer

## 2. bootstrap

### NixOS

```shell
cd /etc/nixos
nix flake init -t 'github:Lubricy/dotfiles#nixos'
# resolve all <change-me> in flake.nix
nixos-rebuild switch
```

### Darwin

```shell
cd
nix flake new -t 'github:Lubricy/dotfiles#darwin' ~/.config/nix-darwin
cd ~/.config/nix-darwin
# resolve all <change-me> in flake.nix
sudo nix run . -- switch --flake . # bootstrap
# sudo darwin-rebuild switch --flake .
```

### Home Manager

```shell
nix flake new -t 'github:Lubricy/dotfiles#hm' ~/.config/home-manager
nix flake new ~/.config/home-manager
nix run . -- switch --flake . # bootstrap
# home-manager switch
```
