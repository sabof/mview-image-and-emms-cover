<h2>MView Image + EMMS Cover</h2>

![screenshot](https://github.com/sabof/mview-image-and-emms-cover/raw/master/screenshot.png)

Emms Cover shows the cover of the currently playing album in EMMS.

MView Image is a simplistic image viewer, based on ImageMagick that shows an
image scaled to fit and centered. It will adjust if the window is resized. EMMS
Cover is derived from this mode. It is mostly of interest to elisp programmers.

<h3>EMMS Cover installation and usage</h3>

Put both files somewhere in your load-path. Add (require 'emms-cover) somewhere
in your .emacs. Then do M-x emms-cover-show.