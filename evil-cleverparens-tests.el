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
      "alpha (b[r]avo\ncharlie) delta\necho"
      ("dd")
      "(charlie) delta\necho"))
  (ert-info ("Can delete unbalanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dte")
      "alpha ([e] delta) echo")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dfe")
      "alpha [ ]echo"))) ;; TODO is this desired?

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

(provide 'evil-cleverparens-tests)

;;; evil-cleverparens-tests.el ends here