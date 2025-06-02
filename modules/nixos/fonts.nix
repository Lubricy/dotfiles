{
  lib,
  pkgs,
  config,
  ...
}: {
  config = lib.mkIf config.dot.features.gui.enable {
    fonts = {
      fontconfig = {
        enable = true;
        # Fixes pixelation
        antialias = true;

        # Fixes antialiasing blur
        hinting = {
          enable = true;
          style = "full"; # no difference
          autohint = true; # no difference
        };

        subpixel = {
          # Makes it bolder
          rgba = "rgb";
          lcdfilter = "default"; # no difference
        };
      };
      fontDir.enable = true;

      packages =
        (with pkgs; [
          # icon fonts
          material-design-icons
          font-awesome

          # 思源系列字体是 Adobe 主导的。其中汉字部分被称为「思源黑体」和「思源宋体」，是由 Adobe + Google 共同开发的
          source-sans # 无衬线字体，不含汉字。字族名叫 Source Sans 3 和 Source Sans Pro，以及带字重的变体，加上 Source Sans 3 VF
          source-serif # 衬线字体，不含汉字。字族名叫 Source Code Pro，以及带字重的变体
          source-han-sans # 思源黑体
          source-han-serif # 思源宋体

          # per recommendations from https://github.com/rolandwalker/unicode-fonts#minimum-useful-fonts
          quivira
          symbola
        ])
        ++ (with pkgs.nerd-fonts; [
          symbols-only
          caskaydia-mono
          dejavu-sans-mono
        ]);
    };
  };
}
