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

(provide 'evil-cleverparens-tests)

;;; evil-cleverparens-tests.el ends here
