;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration and customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(browse-url-browser-function (quote browse-url-w3))
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "cyrillic-jcuken")
 '(frame-background-mode (quote light))
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-splash-screen t)
 '(menu-bar-mode nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(nxml-slash-auto-complete-flag t)
 '(rng-schema-locating-files (quote ("~/schemas/schemas.xml" "schemas.xml" "/usr/share/emacs22/site-lisp/nxml-mode/schema/schemas.xml")))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(visible-bell t))



(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;; my customizations ;;;;;;;;;;;;;;;;;;;;


;; win-1251 support, not needed in emacs 23 (?)
; (codepage-setup '1251)

; theme
(require 'color-theme)

; (color-theme-robin-hood)
(color-theme-charcoal-black)
; (color-theme-jsc-dark)
; (color-theme-goldenrod)


;;;;;;;;;;;;;;;;;;;; my extensions ;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/emacs/")
(add-to-list 'Info-default-directory-list "~/info/")

;; configured databases, see alt-sql.el
(load "alt-sql-profiles")


;; GO support
(add-to-list 'load-path "~/src/go/misc/emacs")
(require 'go-mode-load)


;; Clojure support
; (setq swank-clojure-binary "clojure")

(add-to-list 'load-path "~/src/clojure-mode/")
(add-to-list 'load-path "~/src/swank-clojure/")

(setq swank-clojure-jar-path "~/src/clojure/clojure.jar")
(setq swank-clojure-extra-classpaths
      (list "~/src/clojure-contrib/clojure-contrib.jar" "~/src/swank-clojure/swank-clojure.jar"))

(require 'clojure-mode)
(require 'swank-clojure-autoload)
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)


; (setq slime-lisp-implementations nil)
;; it's broken for some reason by default in debian by sbcl 
(setq slime-lisp-implementations
      '((clojure ("clojure") :init swank-clojure-init)
        (sbcl ("sbcl"))))

; (setq inferior-lisp-program "ny")