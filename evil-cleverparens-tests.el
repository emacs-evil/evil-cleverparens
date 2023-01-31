;; evil-cleverparens-tests.el --- unit tests for Evil Cleverparens -*- coding: utf-8 -*-

;; Author: Tom Dalziel <tom.dalziel@gmail.com>
;; Maintainer: Tom Dalziel <tom.dalziel@gmail.com>

;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

(require 'ert)
(require 'evil)
(require 'evil-cleverparens)
(require 'evil-test-helpers)

(defmacro evil-cp-test-buffer (&rest body)
  (declare (indent defun))
  `(evil-test-buffer
     ,(pop body)
     (evil-cleverparens-mode t)
     ,@body))

(ert-deftest evil-cp-delete-char-or-splice-test ()
  (ert-info ("Can delete a normal character")
    (evil-cp-test-buffer
      "a[b]c"
      ("x")
      "a[c]"))
  (ert-info ("Can splice parens")
    (evil-cp-test-buffer
      "a[(]bc)d"
      ("x")
      "a[b]cd"))
  (ert-info ("Can splice nested parens")
    (evil-cp-test-buffer
      "a[(]b(cd)e)f"
      ("x")
      "a[b](cd)ef"))
  (ert-info ("Can splice double quotes")
    (evil-cp-test-buffer
      "a[\"]bc\"d"
      ("x")
      "a[b]cd"))
  (ert-info ("Can splice curly braces")
    (evil-cp-test-buffer
      "a[{]bc}d"
      ("x")
      "a[b]cd"))
  (ert-info ("Splicing yanks what it deletes")
    (evil-cp-test-buffer
      "a[(]bc)d"
      ("2x$p")
      "acd([b]")))

(ert-deftest evil-cp-delete-char-or-splice-backwards-test ()
  (ert-info ("Can delete a normal character")
    (evil-cp-test-buffer
      "ab[c]"
      ("X")
      "a[c]"))
  (ert-info ("Can splice parens")
    (evil-cp-test-buffer
      "a([b]c)d"
      ("X")
      "a[b]cd"))
  (ert-info ("Can splice nested parens")
    (evil-cp-test-buffer
      "a([b](cd)e)f"
      ("X")
      "a[b](cd)ef"))
  (ert-info ("Can splice double quotes")
    (evil-cp-test-buffer
      "a\"[b]c\"d"
      ("X")
      "a[b]cd"))
  (ert-info ("Can splice curly braces")
    (evil-cp-test-buffer
      "a{[b]c}d"
      ("X")
      "a[b]cd"))
  (ert-info ("Splicing yanks what it deletes")
    (evil-cp-test-buffer
      "a([b]c)d"
      ("2X$p")
      "bcda[(]")))

(ert-deftest evil-cp-delete-backward-word-test ()
  (ert-info ("Can delete a word backwards, just like evil")
    (evil-cp-test-buffer
      "alpha brav[o] charlie"
      ("a\C-w")
      "alpha [] charlie"))
  (ert-info ("Can delete a form when in front of opening paren")
    (evil-cp-test-buffer
      "alpha ([b]ravo charlie) delta"
      ("i\C-w")
      "alpha [] delta"))
  (ert-info ("Can delete a form when in front of opening curly brace")
    (evil-cp-test-buffer
      "alpha {:brav[o] 42} charlie"
      ("F{a\C-w")
      "alpha [] charlie")))

(ert-deftest evil-cp-yank-test ()
  (ert-info ("Can yank balanced block")
    (evil-cp-test-buffer
      "[a]lpha (bravo) charlie
foxtrot delta echo
golf hotel india
golf hotel india"
      ("\C-vjeey" "/hotel" [return] "P")
      "alpha (bravo) charlie
foxtrot delta echo
golf alpha (bravo)hotel india
golf foxtrot deltahotel india"))
  (ert-info ("Can yank unbalanced block")) ;; TODO seems pretty broken
  (ert-info ("Can yank balanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo foxtrot "
      (evil-select-search-module 'evil-search-module 'evil-search)
      ("y/echo/e" [return] "$p")
      "alpha bravo (charlie delta) echo foxtrot bravo (charlie delta) ech[o]"))
  (ert-info ("Can yank unbalanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo foxtrot "
      (evil-select-search-module 'evil-search-module 'evil-search)
      ("y/charlie/e" [return] "$p")
      "alpha bravo (charlie delta) echo foxtrot bravo[ ]"))
  (ert-info ("Can yank balanced line")
    (evil-cp-test-buffer
      "al[p]ha (bravo) charlie\ndelta\necho"
      ("yyjp")
      "alpha (bravo) charlie\ndelta\n[a]lpha (bravo) charlie\necho"))
  (ert-info ("Can yank unbalanced line")
    (evil-cp-test-buffer
      "al[p]ha (bravo\ncharlie)\ndelta\nalpha \necho"
      ("yyjjp"))))

(ert-deftest evil-cp--toggle-balanced-yank-test ()
  (ert-info ("Can balance parens")
    (let ((evil-cleverparens-complete-parens-in-yanked-region t))
      (evil-test-buffer
        :point-start "«"
        :point-end "»"
        "alpha [{bravo\n«c»harlie}]"
        (evil-cleverparens-mode t)
        ("yyp")
        "alpha [{bravo\ncharlie}]\n«[»{charlie}]"))))

(ert-deftest evil-cp-yank-line-test ()
  (ert-info ("Can yank rest of balanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie) delta\necho "
      ("Yj$p")
      "alpha bravo (charlie) delta\necho bravo (charlie) delta"))
  (ert-info ("Can yank rest of unbalanced line") ;; TODO is this desired?
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie\ndelta) echo\nfoxtrot "
      ("Yjj$p")
      "alpha bravo (charlie\ndelta) echo\nfoxtrot bravo (charlie\ndelta)")))

(ert-deftest evil-cp-delete-test ()
  (ert-info ("Can delete unbalanced line")
    (evil-cp-test-buffer
      "alpha (bra[v]o\ncharlie) delta\necho"
      ("dd")
      "(charlie) [d]elta\necho"))
  (ert-info ("Can delete unbalanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dte")
      "alpha ([e] delta) echo")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dfe")
      "alpha [ ]echo")) ;; TODO is this desired?
  (ert-info ("Deleting populates registers correctly")
    (evil-cp-test-buffer
      "[a]lpha bravo charlie delta echo"
      ("dw...")
      "[e]cho"
      ("o" [escape] "\"4p" "\"3p" "\"2p" "\"1p")
      "alpha bravo charlie delta[ ]")))

(ert-deftest evil-cp-delete-line-test ()
  (ert-info ("Can delete rest of balanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie) delta\necho"
      ("D")
      "alpha[ ]\necho"))
  (ert-info ("Can delete rest of unbalanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie\ndelta) echo\nfoxtrot"
      ("D")
      ("alpha [ ]echo\nfoxtrot"))) ;; TODO is this desired?
  (ert-info ("Can delete rest of unbalanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie\ndelta) echo\nfoxtrot"
      ("D")
      "[d]elta) echo\nfoxtrot"))) ;; TODO surely not desired?

(ert-deftest evil-cp-change-test ()
  (ert-info ("Can change unbalanced line")
    (evil-cp-test-buffer
      "alpha (b[r]avo\ncharlie) delta\necho"
      ("cc" "zulu")
      "(zulu\ncharlie) delta\necho"))
  (ert-info ("Can change unbalanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("cte" "zulu")
      "alpha (zulue delta) echo")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("cfe" "zulu")
      "alpha zulu echo"))) ;; TODO is this desired? (c.f. delete)

(ert-deftest evil-cp-change-line-test ()
  (ert-info ("Can change rest of balanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie) delta\necho"
      ("C" "zulu")
      "alpha zulu\necho"))
  (ert-info ("Can change rest of unbalanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie\ndelta) echo\nfoxtrot"
      ("C" "zulu")
      ("alpha zulu echo\nfoxtrot")))
  (ert-info ("Changes whole line when balanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie delta)\necho"
      ("C" "zulu")
      "zuluecho")) ;; TODO definitely not desired (should act like evil C)
  (ert-info ("Changes whole line when unbalanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie\ndelta) echo\nfoxtrot"
      ("C" "zulu")
      "zuludelta) echo\nfoxtrot"))) ;; TODO surely not desired?

(ert-deftest evil-cp-change-whole-line-test ()
  (ert-info ("Can change whole line when balanced")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie) delta\necho"
      ("S" "zulu")
      "zulu\necho"))
  (ert-info ("Can change whole line when unbalanced")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie\ndelta) echo\nfoxtrot"
      ("S" "zulu")
      "zuluecho\nfoxtrot"))) ;; TODO why is the proceeding space missing?

(ert-deftest evil-cp-end-of-defun-test ()
  (ert-info ("Can move to end of defun")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "(defun function-1 ()
  (p[r]int \"Hello, World!\"))
(defun function-2 () 42)"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     (evil-cp-set-additional-movement-keys)
     ("\M-l")
     "(defun function-1 ()
  (print \"Hello, World!\")[)]
(defun function-2 () 42)"
     ("\M-l")
     "(defun function-1 ()
  (print \"Hello, World!\"))
(defun function-2 () 42[)]"))
  (ert-info ("Can move to end of line-consecutive defun")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "(de[f]un foo () bar)  (defun bar () foo)"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     (evil-cp-set-additional-movement-keys)
     ("\M-l")
     "(defun foo () bar[)]  (defun bar () foo)")))

(ert-deftest evil-cp-paren-navigation-test ()
  (ert-info ("Can move to next open paren or string delimiter")
    (evil-cp-test-buffer
      "[a]lpha \"bravo (charlie) delta\" echo (foxtrot) golf"
      (evil-cp-set-additional-movement-keys)
      ("{")
      "alpha [\"]bravo (charlie) delta\" echo (foxtrot) golf"
      ("{")
      "alpha \"bravo (charlie) delta\" echo [(]foxtrot) golf"))
  (ert-info ("Can move to previous open paren or string delimiter")
    (evil-cp-test-buffer
      "alpha \"bravo (charlie) delta\" echo (foxtrot) [g]olf"
      (evil-cp-set-additional-movement-keys)
      ("[")
      "alpha \"bravo (charlie) delta\" echo [(]foxtrot) golf"
      ("[")
      "alpha [\"]bravo (charlie) delta\" echo (foxtrot) golf"))
  (ert-info ("Can move to next closing paren or string delimiter")
    (evil-cp-test-buffer
      "[a]lpha \"bravo (charlie) delta\" echo (foxtrot) golf"
      (evil-cp-set-additional-movement-keys)
      ("]")
      "alpha \"bravo (charlie) delta[\"] echo (foxtrot) golf"
      ("]")
      "alpha \"bravo (charlie) delta\" echo (foxtrot[)] golf"))
  (ert-info ("Can move to previous closing paren or string delimiter")
    (evil-cp-test-buffer
      "alpha \"bravo (charlie) delta\" echo (foxtrot) [g]olf"
      (evil-cp-set-additional-movement-keys)
      ("}")
      "alpha \"bravo (charlie) delta\" echo (foxtrot[)] golf"
      ("}")
      "alpha \"bravo (charlie) delta[\"] echo (foxtrot) golf")))

(ert-deftest evil-cp-backward-up-sexp-test ()
  (ert-info ("Can move up a level of sexp nesting, backwards")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-movement-keys)
      ("(")
      "(alpha (bravo [(]charlie delta) echo) foxtrot)"
      ("(")
      "(alpha [(]bravo (charlie delta) echo) foxtrot)")))

(ert-deftest evil-cp-up-sexp-test ()
  (ert-info ("Can move up a level of sexp nesting, forwards")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-movement-keys)
      (")")
      "(alpha (bravo (charlie delta[)] echo) foxtrot)"
      (")")
      "(alpha (bravo (charlie delta) echo[)] foxtrot)")))

(ert-deftest evil-cp-forward-symbol-begin-test ()
  (ert-info ("Can move forward to the beginning of the next symbol")
    (evil-cp-test-buffer
      "(alpha (b[r]avo (charlie delta) echo) foxtrot)"
      ("W")
      "(alpha (bravo ([c]harlie delta) echo) foxtrot)"
      ("W")
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)")
    (let ((evil-cleverparens-move-skip-delimiters nil))
      (evil-cp-test-buffer
      "(alpha (b[r]avo (charlie delta) echo) foxtrot)"
      ("W")
      "(alpha (bravo [(]charlie delta) echo) foxtrot)"
      ("W")
      "(alpha (bravo ([c]harlie delta) echo) foxtrot)"))))

(ert-deftest evil-cp-forward-symbol-end-test ()
  (ert-info ("Can move forward to the end of the next symbol")
    (evil-cp-test-buffer
      "(alpha (bravo (cha[r]lie delta) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charli[e] delta) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charlie delt[a]) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charlie delta) ech[o]) foxtrot)")
    (let ((evil-cleverparens-move-skip-delimiters nil))
      (evil-cp-test-buffer
      "(alpha (bravo (cha[r]lie delta) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charli[e] delta) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charlie delt[a]) echo) foxtrot)"
      ("E")
      "(alpha (bravo (charlie delta[)] echo) foxtrot)"))))

(ert-deftest evil-cp-backward-symbol-begin-test ()
  (ert-info ("Can move backward to the beginning of the previous symbol")
    (evil-cp-test-buffer
      "(alpha (bravo (char[l]ie delta) echo) foxtrot)"
      ("B")
      "(alpha (bravo ([c]harlie delta) echo) foxtrot)"
      ("B")
      "(alpha ([b]ravo (charlie delta) echo) foxtrot)")
    (let ((evil-cleverparens-move-skip-delimiters nil))
      (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      ("B")
      "(alpha (bravo ([c]harlie delta) echo) foxtrot)"
      ("B")
      "(alpha (bravo [(]charlie delta) echo) foxtrot)"))))

(ert-deftest evil-cp-backward-symbol-end-test ()
  (ert-info ("Can move backward to the end of the previous symbol")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie delta) ec[h]o) foxtrot)"
      ("gE")
      "(alpha (bravo (charlie delt[a]) echo) foxtrot)"
      ("gE")
      "(alpha (bravo (charli[e] delta) echo) foxtrot)")
    (let ((evil-cleverparens-move-skip-delimiters nil))
      (evil-cp-test-buffer
      "(alpha (bravo (charlie delta) ec[h]o) foxtrot)"
      ("gE")
      "(alpha (bravo (charlie delta[)] echo) foxtrot)"
      ("gE")
      "(alpha (bravo (charlie delt[a]) echo) foxtrot)"))))

(ert-deftest evil-cp-<-test ()
  (ert-info ("Can do regular forward barf")
    (evil-cp-test-buffer
      "alpha bravo (charlie [d]elta echo) foxtrot golf"
      ("<")
      "alpha bravo (charlie [d]elta) echo foxtrot golf"))
  (ert-info ("Can slurp backwards when on opening delimiter")
    (evil-cp-test-buffer
      "alpha bravo [(]charlie delta echo) foxtrot golf"
      ("<")
      "alpha [(]bravo charlie delta echo) foxtrot golf"))
  (ert-info ("Can barf forwards when on closing delimiter")
    (evil-cp-test-buffer
      "alpha bravo (charlie delta echo[)] foxtrot golf"
      ("<")
      "alpha bravo (charlie delta[)] echo foxtrot golf"))
  (ert-info ("No barfing forwards when <= 1 element")
    (evil-cp-test-buffer
      "alpha bravo (cha[r]lie) delta echo"
      ("<")
      "alpha bravo (cha[r]lie) delta echo")))

(ert-deftest evil-cp->-test ()
  (ert-info ("Can do regular forward slurp")
    (evil-cp-test-buffer
      "alpha bravo (charlie [d]elta echo) foxtrot golf"
      (">")
      "alpha bravo (charlie [d]elta echo foxtrot) golf"))
  (ert-info ("Can barf backwards when on opening delimiter")
    (evil-cp-test-buffer
      "alpha bravo [(]charlie delta echo) foxtrot golf"
      (">")
      "alpha bravo charlie [(]delta echo) foxtrot golf"))
  (ert-info ("No barfing backwards when <= 1 element")
    (evil-cp-test-buffer
      "alpha bravo [(]charlie) delta echo"
      (">")
      "alpha bravo [(]charlie) delta echo"))
  (ert-info ("Can slurp forwards when on closing delimiter")
    (evil-cp-test-buffer
      "alpha bravo (charlie delta echo[)] foxtrot golf"
      (">")
      "alpha bravo (charlie delta echo foxtrot[)] golf")))

(ert-deftest evil-cp-drag-forward-test ()
  (ert-info ("Can drag symbol or parent form forward")
    (evil-cp-test-buffer
      "alpha (bravo [c]harlie delta) echo foxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-j")
      "alpha (bravo delta [c]harlie) echo foxtrot"
      ("\M-j")
      "alpha echo (bravo delta [c]harlie) foxtrot"))
  (ert-info ("Can drag line forward")
    (evil-cp-test-buffer
      "alpha bravo
cha[r]lie delta
echo foxtrot
golf hotel"
      ("V" "\M-j")
      "alpha bravo
echo foxtrot
cha[r]lie delta
golf hotel"))
  (ert-info ("Can drag comment block forward")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "alpha bravo
;; Comment [1]
charlie delta
echo foxtrot"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     (call-interactively #'evil-cp-drag-forward)
     "alpha bravo
charlie delta
;; Comment [1]
echo foxtrot"))) ;; TODO - multiple contiguous comment blocks throw error

(ert-deftest evil-cp-drag-backward-test ()
  (ert-info ("Can drag symbol or parent form backward")
    (evil-cp-test-buffer
      "alpha bravo (charlie [d]elta echo) foxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-k")
      "alpha bravo ([d]elta charlie echo) foxtrot"
      ("\M-k")
      "alpha ([d]elta charlie echo) bravo foxtrot"))
  (ert-info ("Can drag line backward")
    (evil-cp-test-buffer
      "alpha bravo
charlie delta
ec[h]o foxtrot
golf hotel"
      ("V" "\M-k")
      "alpha bravo
ec[h]o foxtrot
charlie delta
golf hotel"))
  (ert-info ("Can drag comment block backward")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "alpha bravo
charlie delta
;; Comment [1]
echo foxtrot"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     (call-interactively #'evil-cp-drag-backward)
     "alpha bravo
;; Comment [1]
charlie delta
echo foxtrot")))

(ert-deftest evil-cp-substitute-test ()
  (ert-info ("Keep balance when substituting on opening paren")
    (evil-cp-test-buffer
      "alpha [(]bravo charlie)"
      ("s" "new text ")
      "alpha (new text []bravo charlie)"))
  (ert-info ("Keep balance when substituting on closing paren")
    (evil-cp-test-buffer
      "alpha (bravo charlie[)]"
      ("s" " new text")
      "alpha (bravo charlie new text[])")))

(ert-deftest evil-cp-insert-at-end-of-form-test ()
  (ert-info ("Can insert at the end of a form")
    (evil-cp-test-buffer
      "alpha (b[r]avo (charlie delta) echo) foxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-a" " new text")
      "alpha (bravo (charlie delta) echo new text[]) foxtrot")))

(ert-deftest evil-cp-insert-at-beginning-of-form-test ()
  (ert-info ("Can insert at the beginning of a form")
    (evil-cp-test-buffer
      "alpha (bravo (charlie delta) e[c]ho) foxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-i" "new text") ;; when evil-cleverparens-use-regular-insert is non-nil
      "alpha (new text[] bravo (charlie delta) echo) foxtrot")))

(ert-deftest evil-cp-copy-paste-form-test ()
  (ert-info ("Can copy and paste current form")
    (evil-cp-test-buffer
      "alpha (bravo (charlie [d]elta) echo) foxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-w")
      "alpha (bravo (charlie delta)\n(charlie [d]elta) echo) foxtrot"))
  (ert-info ("Can copy and paste top-level form")
    (evil-cp-test-buffer
      "alpha (bravo (charlie [d]elta) echo) foxtrot"
      (evil-cp-set-additional-bindings)
      ("\C-u" "\M-w")
      "alpha (bravo (charlie delta) echo) foxtrot
(bravo (charlie [d]elta) echo)"))
  (ert-info ("Can copy and paste current line if not in a form")
    (evil-cp-test-buffer
      "alpha (bravo (charlie delta) echo) [f]oxtrot"
      (evil-cp-set-additional-bindings)
      ("\M-w")
      "alpha (bravo (charlie delta) echo) foxtrot
alpha (bravo (charlie delta) echo) foxtrot")))

(ert-deftest evil-cp-open-below-form-test ()
  (ert-info ("Can open below a form")
    (evil-cp-test-buffer
      "(alpha (bravo [c]harlie) delta)"
      (evil-cp-set-additional-bindings)
      ("\M-o")
      "(alpha (bravo charlie)\n[]delta)"))
  (ert-info ("Can open below a top-level form")
    (evil-cp-test-buffer
      "(alpha [b]ravo)\n\n(charlie delta)"
      (evil-cp-set-additional-bindings)
      ("\M-o" "echo")
      "(alpha bravo)\n\necho[]\n\n(charlie delta)"))
  (ert-info ("Can open between top-level forms")
    (evil-cp-test-buffer
      "(alpha bravo)\n[]\n(charlie delta)"
      (evil-cp-set-additional-bindings)
      ("\M-o" "echo")
      "(alpha bravo)\n\necho[]\n\n(charlie delta)")))

(ert-deftest evil-cp-open-above-form-test ()
  (ert-info ("Can open above a form")
    (evil-cp-test-buffer
      "(alpha (bravo [c]harlie) delta)"
      (evil-cp-set-additional-bindings)
      ("\M-O")
      "(alpha []\n(bravo charlie) delta)"))
  (ert-info ("Can open below a top-level form")
    (evil-cp-test-buffer
      "(alpha bravo)\n\n(charlie [d]elta)"
      (evil-cp-set-additional-bindings)
      ("\M-O" "echo")
      "(alpha bravo)\n\necho[]\n\n(charlie delta)"))
  (ert-info ("Can open between top-level forms")
    (evil-cp-test-buffer
      "(alpha bravo)\n[]\n(charlie delta)"
      (evil-cp-set-additional-bindings)
      ("\M-O" "echo")
      "(alpha bravo)\necho[]\n\n(charlie delta)")))

(ert-deftest evil-cp-yank-sexp-test ()
  (ert-info ("Can yank a sexp")
    (evil-cp-test-buffer
      "(alpha b[r]avo charlie)"
      (evil-cp-set-additional-bindings)
      ("\M-y" "o" [escape] "p")
      "(alpha bravo charlie)\nrav[o]")
    (evil-cp-test-buffer
      "(alpha bravo charlie[)] (delta)"
      (evil-cp-set-additional-bindings)
      ("\M-y" "$p")
      "(alpha bravo charlie) (delta)(alpha bravo charlie[)]")))

(ert-deftest evil-cp-delete-sexp-test ()
  (ert-info ("Can delete a sexp")
    (evil-cp-test-buffer
      "(alpha b[r]avo charlie)"
      (evil-cp-set-additional-bindings)
      ("\M-d")
      "(alpha b[ ]charlie)"
      ("\M-d")
      "(alpha b[)]"
      ("\M-d")
      "[]")))

(ert-deftest evil-cp-change-sexp-test ()
  (ert-info ("Can change a sexp")
    (evil-cp-test-buffer
      "(alpha b[r]avo charlie)"
      (evil-cp-set-additional-bindings)
      ("\M-c" "eta")
      "(alpha beta[] charlie)")
    (evil-cp-test-buffer
      "(alpha bravo charlie[)] (delta)"
      (evil-cp-set-additional-bindings)
      ("\M-c" "echo")
      "echo[] (delta)")))

(ert-deftest evil-cp-yank-enclosing-test ()
  (ert-info ("Can yank enclosing form")
    (evil-cp-test-buffer
      "(alpha (b[r]avo charlie) delta)"
      (evil-cp-set-additional-bindings)
      ("\M-Y" "o" [escape] "p")
      "(alpha (bravo charlie) delta)\n(bravo charlie[)]"))
  (ert-info ("Can yank enclosing forms with count")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-bindings)
      ("2\M-Y" "o" [escape] "p")
      "(alpha (bravo (charlie delta) echo) foxtrot)\n(bravo (charlie delta) echo[)]"))
  (ert-info ("Can yank top-level form with universal arg")
    (evil-cp-test-buffer
      "(alpha)

(bravo (charlie (delta [e]cho) foxtrot) golf)

(hotel)"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-Y" "jp")
      "(alpha)

(bravo (charlie (delta echo) foxtrot) golf)

(bravo (charlie (delta echo) foxtrot) golf)

(hotel)")))

(ert-deftest evil-cp-delete-enclosing-test ()
  (ert-info ("Can delete enclosing form")
    (evil-cp-test-buffer
      "(alpha (b[r]avo charlie) delta)"
      (evil-cp-set-additional-bindings)
      ("\M-D")
      "(alpha[ ]delta)"))
  (ert-info ("Can delete enclosing forms with count")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-bindings)
      ("2\M-D")
      "(alpha[ ]foxtrot)"))
  (ert-info ("Can delete top-level form with universal arg")
    (evil-cp-test-buffer
      "(alpha)

(bravo (charlie (delta [e]cho) foxtrot) golf)

(hotel)"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-D")
      "(alpha)
[]
(hotel)")))

(ert-deftest evil-cp-change-enclosing-test ()
  (ert-info ("Can change enclosing form")
    (evil-cp-test-buffer
      "(alpha (b[r]avo charlie) delta)"
      (evil-cp-set-additional-bindings)
      ("\M-C" " echo")
      "(alpha echo[] delta)"))
  (ert-info ("Can change enclosing forms with count")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-bindings)
      ("2\M-C" " golf")
      "(alpha golf[] foxtrot)"))
  (ert-info ("Can change top-level form with universal arg")
    (evil-cp-test-buffer
      "(alpha)

(bravo (charlie (delta [e]cho) foxtrot) golf)

(hotel)"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-C" "india")
      "(alpha)

india[]

(hotel)")))

(ert-deftest evil-cp-raise-form-test ()
  (ert-info ("Can raise a form")
    (evil-cp-test-buffer
      "(alpha (bravo (charlie [d]elta) echo) foxtrot)"
      (evil-cp-set-additional-bindings)
      ("\M-R")
      "(alpha (charlie [d]elta) foxtrot)")))

(ert-deftest evil-cp-wrap-next-round-test ()
  (ert-info ("Can wrap next sexp with parens")
    (evil-cp-test-buffer
      "alpha[ ]bravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-(")
      "alpha([ ]bravo) charlie")
    (evil-cp-test-buffer
      "alpha [b]ravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-(")
      "alpha ([b]ravo) charlie")
    (evil-cp-test-buffer
      "alpha brav[o] charlie"
      (evil-cp-set-additional-bindings)
      ("\M-(")
      "alpha (brav[o]) charlie")
    (evil-cp-test-buffer
      "alpha (bravo [c]harlie delta) echo"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-(")
      "alpha ([(]bravo charlie delta)) echo")))

(ert-deftest evil-cp-wrap-previous-round-test ()
  (ert-info ("Can wrap previous sexp with parens")
    (evil-cp-test-buffer
      "alpha bravo[ ]charlie"
      (evil-cp-set-additional-bindings)
      ("\M-)")
      "alpha (bravo[)] charlie")
    (evil-cp-test-buffer
      "alpha brav[o] charlie"
      (evil-cp-set-additional-bindings)
      ("\M-)")
      "alpha (brav[o]) charlie")
    (evil-cp-test-buffer
      "alpha [b]ravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-)")
      "(alpha [)]bravo charlie") ;; TODO surely not desired?
    (evil-cp-test-buffer
      "alpha (bravo [c]harlie delta) echo"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-)")
      "alpha ((bravo charlie delta)[)] echo"))) ;; TODO inconsistent cursor with wrap-next-round

(ert-deftest evil-cp-wrap-next-square-test ()
  (ert-info ("Can wrap next sexp with square brackets")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha« »bravo charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-[")
      "alpha[« »bravo] charlie")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha «b»ravo charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-[")
      "alpha [«b»ravo] charlie")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha brav«o» charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-[")
      "alpha [brav«o»] charlie")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha [bravo «c»harlie delta] echo"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\C-u\M-[")
      "alpha [«[»bravo charlie delta]] echo")))

(ert-deftest evil-cp-wrap-previous-square-test ()
  (ert-info ("Can wrap next sexp with square brackets")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha bravo« »charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-]")
      "alpha [bravo«]» charlie")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha brav«o» charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-]")
      "alpha [brav«o»] charlie")
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha «b»ravo charlie"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\M-]")
      "[alpha «]»bravo charlie") ;; TODO surely not desired?
    (evil-test-buffer
      :point-start "«"
      :point-end "»"
      "alpha [bravo «c»harlie delta] echo"
      (evil-cleverparens-mode t)
      (evil-cp-set-additional-bindings)
      ("\C-u\M-]")
      "alpha [[bravo charlie delta]«]» echo"))) ;; TODO inconsistent cursor with wrap-next-square

(ert-deftest evil-cp-wrap-next-curly-test ()
  (ert-info ("Can wrap next sexp with curly braces")
    (evil-cp-test-buffer
      "alpha[ ]bravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-{")
      "alpha{[ ]bravo} charlie")
    (evil-cp-test-buffer
      "alpha [b]ravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-{")
      "alpha {[b]ravo} charlie")
    (evil-cp-test-buffer
      "alpha brav[o] charlie"
      (evil-cp-set-additional-bindings)
      ("\M-{")
      "alpha {brav[o]} charlie")
    (evil-cp-test-buffer
      "alpha {bravo [c]harlie delta} echo"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-{")
      "alpha {[{]bravo charlie delta}} echo")))

(ert-deftest evil-cp-wrap-previous-curly-test ()
  (ert-info ("Can wrap previous sexp with curly braces")
    (evil-cp-test-buffer
      "alpha bravo[ ]charlie"
      (evil-cp-set-additional-bindings)
      ("\M-}")
      "alpha {bravo[}] charlie")
    (evil-cp-test-buffer
      "alpha brav[o] charlie"
      (evil-cp-set-additional-bindings)
      ("\M-}")
      "alpha {brav[o]} charlie")
    (evil-cp-test-buffer
      "alpha [b]ravo charlie"
      (evil-cp-set-additional-bindings)
      ("\M-}")
      "{alpha [}]bravo charlie") ;; TODO surely not desired?
    (evil-cp-test-buffer
      "alpha {bravo [c]harlie delta} echo"
      (evil-cp-set-additional-bindings)
      ("\C-u\M-}")
      "alpha {{bravo charlie delta}[}] echo"))) ;; TODO inconsistent cursor with wrap-next-curly

(ert-deftest evil-cp-insert-test ()
  (ert-info ("Inserts a space when helpful")
    (evil-cp-test-buffer
      "alpha ([b]ravo charlie) delta"
      ("i" "echo")
      "alpha (echo[] bravo charlie) delta"))
  (ert-info ("Inserts no space when it would be unhelpful")
    (evil-cp-test-buffer
      "alpha ([)] delta"
      ("i" "echo")
      "alpha (echo[]) delta")
    (evil-cp-test-buffer
      "alpha ([ ]bravo charlie) delta"
      ("i" "echo")
      "alpha (echo[] bravo charlie) delta")
    (evil-cp-test-buffer
      "alpha ([b]ravo charlie) delta"
      ("i" [escape])
      "alpha [(]bravo charlie) delta")))

(ert-deftest evil-cp-append-test ()
  (ert-info ("Inserts a space when helpful")
    (evil-cp-test-buffer
      "alpha [(]bravo charlie) delta"
      ("a" "echo")
      "alpha (echo[] bravo charlie) delta"))
  (ert-info ("Inserts no space when it would be unhelpful")
    (evil-cp-test-buffer
      "alpha [(]) delta"
      ("a" "echo")
      "alpha (echo[]) delta")
    (evil-cp-test-buffer
      "alpha [(] bravo charlie) delta"
      ("a" "echo")
      "alpha (echo[] bravo charlie) delta")
    (evil-cp-test-buffer
      "alpha [(]bravo charlie) delta"
      ("a" [escape])
      "alpha [(]bravo charlie) delta")))

(provide 'evil-cleverparens-tests)

;;; evil-cleverparens-tests.el ends here
