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
      "acd(b")))

(provide 'evil-cleverparens-tests)

;;; evil-cleverparens-tests.el ends here
