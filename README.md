<h2>MView Image + EMMS Cover</h2>

![screenshot](https://github.com/sabof/mview-image-and-emms-cover/raw/master/screenshot.png)

Emms Cover shows the cover of the currently playing album in EMMS.

MView Image is a simplistic image viewer, based on ImageMagick that shows an
image scaled to fit and centered. It will adjust if the window is resized. EMMS
Cover is derived from this mode. It is mostly of interest to elisp programmers.

<h3>EMMS Cover installation and usage</h3>

Put both files somewhere in your load-path, add `(require 'emms-cover)` somewhere
in your .emacs, and do `M-x emms-cover-show`.

<h3>Possible improvments (That I'm currently too lazy to implement, but you might not be)</h3>
<h4>MView Image</h4>

* Improve the caching mechanism. It's currently in a rather lame, slightly
better than nothing state. Possibly add a buffer-local switch to turn caching on
and off.

<h4>EMMS Cover</h4>

* Find/make a nicely licenced image for missing covers. Currently you need to
find your own and set the emms-cover-nocover-image variable.
* Overlay a pause sign on top of the cover when the player is paused.