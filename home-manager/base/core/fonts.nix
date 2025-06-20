{pkgs, ...}: {
  fonts.fontconfig.enable = true;
  fonts.fontconfig.defaultFonts = {
    monospace = [
      "Caskaydia Mono NF"
      "AnonymicePro Nerd Font Propo"
      "Source Han Sans HW SC"
    ];
    serif = [
      "DejaVu Serif"
      "Source Serif 4 Display"
      "Source Han Serif SC"
    ];
    sansSerif = [
      "DejaVu Sans"
      "Source Sans 3"
      "Source Han Sans HW SC"
    ];
  };
  home.packages = with pkgs;
    [
      # icon fonts
      material-design-icons
      font-awesome

      # 思源系列字体是 Adobe 主导的。其中汉字部分被称为「思源黑体」和「思源宋体」，是由 Adobe + Google 共同开发的
      source-sans # 无衬线字体，不含汉字。字族名叫 Source Sans 3 和 Source Sans Pro，以及带字重的变体，加上 Source Sans 3 VF
      source-serif # 衬线字体，不含汉字。字族名叫 Source Code Pro，以及带字重的变体
      source-han-sans # 思源黑体
      source-han-serif # 思源宋体

      # nerdfonts
      # https://github.com/NixOS/nixpkgs/blob/nixos-24.05/pkgs/data/fonts/nerdfonts/shas.nix
      # (nerdfonts.override {
      #   fonts = [
      #     # symbols icon only
      #     "NerdFontsSymbolsOnly"
      #     # Characters
      #     "CascadiaMono" # my choice for Emacs main font
      #     "FiraCode"
      #     "JetBrainsMono"
      #     "Iosevka"
      #     "AnonymousPro"
      #   ];
      # })
    ]
    ++ (with pkgs.nerd-fonts; [
      symbols-only
      # Characters
      caskaydia-cove # my choice for Emacs main font
      fira-code
      jetbrains-mono
      iosevka-term-slab
      anonymice
      ubuntu
    ]);
}
