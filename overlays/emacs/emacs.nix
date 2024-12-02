{emacs-overlay, ...}: {
  nixpkgs.overlays = [emacs-overlay.overlays.emacs];
}
