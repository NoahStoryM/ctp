#lang racket/base

(require racket/file racket/string)

(define color-table
  #hash(
        ["rgb(255,0,0)"     . "rgb(208,2,27)"]    ; red
        ["rgb(255,128,0)"   . "rgb(245,166,35)"]  ; orange
        ["rgb(255,255,0)"   . "rgb(248,231,28)"]  ; yellow
        ["rgb(0,255,0)"     . "rgb(36,223,40)"]   ; green
        ["rgb(128,255,0)"   . "rgb(126,211,33)"]  ; lime
        ["rgb(0,255,128)"   . "rgb(70,215,130)"]  ; mint
        ["rgb(0,255,255)"   . "rgb(80,227,194)"]  ; cyan
        ["rgb(0,128,255)"   . "rgb(74,144,226)"]  ; sky
        ["rgb(0,0,255)"     . "rgb(70,51,208)"]   ; blue
        ["rgb(128,0,255)"   . "rgb(144,29,254)"]  ; violet
        ["rgb(255,0,255)"   . "rgb(189,16,224)"]  ; fuchsia
        ["rgb(255,0,128)"   . "rgb(215,21,133)"]  ; rose
        ["rgb(128,128,128)" . "rgb(155,155,155)"] ; gray
        ["rgb(1,1,1)"       . "rgb(74,74,74)"]    ; black

        ["rgb(255, 0, 0)"     . "rgb(208, 2, 27)"]    ; red
        ["rgb(255, 128, 0)"   . "rgb(245, 166, 35)"]  ; orange
        ["rgb(255, 255, 0)"   . "rgb(248, 231, 28)"]  ; yellow
        ["rgb(0, 255, 0)"     . "rgb(36, 223, 40)"]   ; green
        ["rgb(128, 255, 0)"   . "rgb(126, 211, 33)"]  ; lime
        ["rgb(0, 255, 128)"   . "rgb(70, 215, 130)"]  ; mint
        ["rgb(0, 255, 255)"   . "rgb(80, 227, 194)"]  ; cyan
        ["rgb(0, 128, 255)"   . "rgb(74, 144, 226)"]  ; sky
        ["rgb(0, 0, 255)"     . "rgb(70, 51, 208)"]   ; blue
        ["rgb(128, 0, 255)"   . "rgb(144, 29, 254)"]  ; violet
        ["rgb(255, 0, 255)"   . "rgb(189, 16, 224)"]  ; fuchsia
        ["rgb(255, 0, 128)"   . "rgb(215, 21, 133)"]  ; rose
        ["rgb(128, 128, 128)" . "rgb(155, 155, 155)"] ; gray
        ["rgb(1, 1, 1)"       . "rgb(74, 74, 74)"]    ; black
        ))

(define (get-paths dn fn)
  (define in-path  (build-path "images" dn fn))
  (define out-path (build-path "scribblings" dn "images" fn))
  (values in-path out-path))

(define (update-colors! dn fn)
  (define-values (in-path out-path) (get-paths dn fn))
  (define str
    (for/fold ([str (file->string in-path)])
              ([(from to) (in-hash color-table)])
      (string-replace str from to)))
  (call-with-output-file out-path
    #:exists 'truncate/replace
    (λ (out) (write-string str out))))

(define (main [argv (current-command-line-arguments)])
  (for* ([d (in-directory "images")]
         #:when (directory-exists? d)
         [f (in-directory d)]
         #:when (and (file-exists? f)
                     (string-suffix? (path->string f) ".svg")))
    (define-values (_db dn _d?) (split-path d))
    (define-values (_fb fn _f?) (split-path f))
    (displayln "\n**********************************************************************")
    (displayln (format "dn : ~a" dn))
    (displayln (format "fn : ~a" fn))
    (update-colors! dn fn))
  (values))
(module+ main (call-with-values main exit))
