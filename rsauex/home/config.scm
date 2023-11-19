(define-module (rsauex home config)
  #:use-module ((gnu services configuration))
  #:export (xft-config
            xft-config?
            xft-config-antialias?
            xft-config-hinting?
            xft-config-hint-style
            xft-config-rgba
            xft-config-lcd-filter
            xft-config-dpi))

(define (hint-style? thing)
  (member thing '("hintnone" "hintslight" "hintmedium" "hintfull")))

(define (rgba? thing)
  (member thing '("rgb" "bgr" "vrgb" "vbgr" "none")))

(define (lcd-filter? thing)
  (member thing '("lcdnone" "lcddefault" "lcdlight" "lcdlegacy")))

(define (positive-integer? thing)
  (and (integer? thing) (positive? thing)))

(define-configuration/no-serialization xft-config
  (antialias?
   (boolean #t)
   "Whether fonts should be antialiased.")
  (hinting?
   (boolean #t)
   "Whether the rasterizer should use hinting.")
  (hint-style
   (hint-style "hintslight")
   "Hinting style.")
  (rgba
   (rgba "rgb")
   "Subpixel geometry.")
  (lcd-filter
   (lcd-filter "lcddefault")
   "Type of LCD filter.")
  (dpi
   (positive-integer 96)
   "Target dots per inch"))
