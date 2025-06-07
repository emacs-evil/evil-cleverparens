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
  (ert-info ("Can delete char in parens")
    (evil-cp-test-buffer
      "([x])"
      ("x")
      "()"))
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
      "a[b]cd")
    (evil-cp-test-buffer
      "\"[\"]"
      ("x")
      ""))
  (ert-info ("Can splice parens inside double quotes")
    (evil-cp-test-buffer
      "a\"b[(]cd)e\"f"
      ("x")
      "a\"b[c]de\"f")
    (evil-cp-test-buffer
      "a\"b(cd[)]e\"f"
      ("x")
      "a\"bcd[e]\"f"))
  (ert-info ("Can splice curly braces")
    (evil-cp-test-buffer
      "a[{]bc}d"
      ("x")
      "a[b]cd"))
  (ert-info ("Splicing yanks what it deletes")
    (evil-cp-test-buffer
      "a[(]bc)d"
      ("2x$p")
      "acd([b]"))
  (ert-info ("Can delete in visual state")
    (evil-cp-test-buffer
      "f[o]o"
      ("vlx")
      "f"))
  (ert-info ("Can transpose chars")
    (evil-cp-test-buffer
      "([o]fo)"
      ("xp")
      "(f[o]o)")))

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
      "bcda[(]"))
  (ert-info ("Can delete in visual state")
    (evil-cp-test-buffer
      "f[o]o"
      ("vlX")
      "f")))

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
      ("yyjjp")))
  (ert-info ("Can yank unbalanced string")
    (evil-cp-test-buffer
     "alpha '[b]ravo } charlie' delta"
     ("yi'" "o" [escape] "p")
     "bravo } charli[e]")))

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
  (ert-info ("Can delete balanced line")
    (evil-cp-test-buffer
      "\"[a]lpha\" 'bravo"
      ("dd")
      "")
    (evil-cp-test-buffer
      "\"\"[]"
      ("dd")
      "")
    (evil-cp-test-buffer
    "[]
"
    ("dd")
    "")
    (evil-cp-test-buffer
      "fo[o]
"
      ("jdd")
      "foo"))
  (ert-info ("Can delete unbalanced line")
    (evil-cp-test-buffer
      "alpha (bra[v]o\ncharlie) delta\necho"
      ("dd")
      "(charlie) [d]elta\necho")
    (evil-cp-test-buffer
      "alpha (bravo\n[c]harlie)\ndelta"
      ("dd")
      "alpha (bravo)\n[d]elta"))
  (ert-info ("Can delete unbalanced region")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dte")
      "alpha ([e] delta) echo")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie delta) echo"
      ("dfe")
      "alpha ([ ]delta) echo"))
  (ert-info ("Deleting populates registers correctly")
    (evil-cp-test-buffer
      "[a]lpha bravo charlie delta echo"
      ("dw...")
      "[e]cho"
      ("o" [escape] "\"4p" "\"3p" "\"2p" "\"1p")
      "alpha bravo charlie delta[ ]"))
  (ert-info ("Can delete word")
    (evil-cp-test-buffer
      "([T]his)"
      ("dw")
      "()"))
  (ert-info ("Can delete word with count")
    (evil-cp-test-buffer
      "([T]his)"
      ("2dw")
      "()"))
  (ert-info ("Can delete WORD")
    (evil-cp-test-buffer
      "([T]his)"
      ("dW")
      "()"))
  (ert-info ("Can delete WORD with count")
    (evil-cp-test-buffer
      "([T]his)"
      ("2dW")
      "()"))
  (ert-info ("Can delete 2 WORDs with count")
    (evil-cp-test-buffer
      "([T]his that)"
      ("2dW")
      "()"))
  (ert-info ("Can delete word backward")
    (evil-cp-test-buffer
      "(f[o]o)"
      ("db")
      "(oo)"))
  (ert-info ("Can delete WORD backward")
    (evil-cp-test-buffer
      "(fo[o])"
      ("dB")
      "(o)"))
  (ert-info ("Can delete backward to bol")
    ;; These look the same, but in the past, gave different results
    ;; TODO - should this stop at the opening paren?
    (evil-cp-test-buffer
      "(foo (bar[ ]baz))"
      ("d^")
      "(( baz))")
    (evil-cp-test-buffer
      "(alpha (bravo[ ]charlie))"
      ("d^")
      "(( charlie))"))
  (ert-info ("Can delete line containing escaped parens")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
      "(alpha\n(b[r]avo ?\\))\ncharlie)"
      (emacs-lisp-mode)
      (evil-cleverparens-mode t)
      ("dd")
      "(alpha\ncharlie)"))
  (ert-info ("Can delete unbalanced line containing escaped parens")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
      "(alpha\n(b[r]avo ?\\)\ncharlie)\ndelta)"
      (emacs-lisp-mode)
      (evil-cleverparens-mode t)
      ("dd")
      "(alpha\n (charlie)\n delta)")))

;; (alpha[ ]bravo) charlie
;; charlie (bravo[ ]alpha)
;; should d$ on l1 correspond with d^ on l2?

(ert-deftest evil-cp-delete-line-test ()
  (ert-info ("Can delete rest of balanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie) delta\necho"
      ("D")
      "alpha[ ]\necho")
    (evil-cp-test-buffer
      "(alpha [b]ravo (charlie)\ndelta)"
      ("D")
      "(alpha[ ]\ndelta)"))
  (ert-info ("Can delete rest of unbalanced line")
    (evil-cp-test-buffer
      "alpha [b]ravo (charlie\ndelta) echo\nfoxtrot"
      ("D")
      "alpha [ ]echo\nfoxtrot") ;; TODO is this desired?
    (evil-cp-test-buffer
      "(Test [ ]        )"
      ("D")
      "(Test )"))
  (ert-info ("Can delete empty form on closing delimiter")
    (evil-cp-test-buffer
      "(alpha ([)] bravo)"
      ("D")
      "(alpha[ ]bravo)"))
  (ert-info ("Can delete one-line comments")
    (evil-cp-test-buffer
      "([+] ;;This is a comment
1
1)"
      ("D")
      "(
1
1)"))
  (ert-info ("Can delete entire form spanning more lines")
    (evil-cp-test-buffer
      "[(]let [foo (bar baz)
           qux 1
           quux (+ 1 2)]
       (dwim foo qux quux))"
      ("D")
      ""))
  (ert-info ("Preserves delimeters when inside them")
    (evil-cp-test-buffer
      "(foo \"[b]ar baz\"
           quux)"
      ("D")
      "(foo \"\"
           quux)")
    (evil-cp-test-buffer
      "(alpha {bravo[ ]charlie}
           delta)"
      ("D")
      "(alpha {bravo}
           delta)")
    (evil-cp-test-buffer
      "(alpha ${bravo[ ]charlie}
           delta)"
      ("D")
      "(alpha ${bravo}
           delta)"))
  (ert-info ("Preserves rest of line outside the current form")
    (evil-cp-test-buffer
      "([f]oo bar)     ; Important comment"
      ("D")
      "([)]     ; Important comment"))
  (ert-info ("Can delete rest of unbalanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie\ndelta) echo\nfoxtrot"
      ("D")
      "[ ]echo\nfoxtrot")))

(ert-deftest evil-cp-change-test ()
  (ert-info ("Can change word and keep spacing")
    (evil-cp-test-buffer
      "alpha [b]ravo charlie"
      ("cw" "delta")
      "alpha delta[] charlie"))
  (ert-info ("Can change up to end of symbol")
    (evil-cp-test-buffer
      "(((alpha b[r]avo-charlie)))"
      ("cE" "eta-gamma")
      "(((alpha beta-gamma[])))"))
  (ert-info ("Can change inner symbol")
    (evil-cp-test-buffer
      "(((alpha b[r]avo-charlie)))"
      ("ciW" "beta-gamma")
      "(((alpha beta-gamma[])))"))
  (ert-info ("Can change a symbol")
    (evil-cp-test-buffer
      "(((alpha b[r]avo-charlie)))"
      ("caW" "-beta")
      "(((alpha-beta[])))"))
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
      "alpha (zulu[] delta) echo"))
  (ert-info ("Preserves indent")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "
(let ((foo \"bar\"))
  ([c]oncat foo \"baz\"))"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     ("cc")
     "
(let ((foo \"bar\"))
  )")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
     "
(let ((foo \"bar\"))
  ([l]et ((asdf \"ghkj\"))
    (concat foo asdf)))"
     (emacs-lisp-mode)
     (evil-cleverparens-mode t)
     ("cc" "alpha")
     "
(let ((foo \"bar\"))
  (alpha[]
    (concat foo asdf)))")))

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
      "alpha zulu echo\nfoxtrot"))
  (ert-info ("Changes whole line when balanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie delta)\necho"
      ("C" "zulu")
      "zuluecho")) ;; TODO definitely not desired (should act like evil C)
  (ert-info ("Changes whole line when unbalanced line in visual state")
    (evil-cp-test-buffer
      "alpha <brav[o]> (charlie\ndelta) echo\nfoxtrot"
      ("C" "zulu")
      "zulu echo\nfoxtrot")))

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
      "(zulu[]\ndelta) echo\nfoxtrot")))

(ert-deftest evil-cp-forward-sexp-test ()
  (ert-info ("Can move past eol")
    (let (evil-move-beyond-eol)
      (evil-test-buffer ;; As we need to enable evil-cp later anyway
       "([(]alpha bravo)\n(charlie delta))"
       (evil-cleverparens-mode t)
       (evil-cp-set-additional-movement-keys)
       (setq evil-move-beyond-eol nil) ;; Should be nil anyway.
       ("L")
       "((alpha bravo[)]\n(charlie delta))"
       ("L")
       "((alpha bravo)\n(charlie delta)[)]"))))

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
      "(alpha (bravo (charlie delta) echo[)] foxtrot)"))
  (ert-info ("Don't move if not in a sexp")
    (evil-cp-test-buffer
      "alpha[ ]bravo"
      (evil-cp-set-additional-movement-keys)
      (")")
      "alpha[ ]bravo"))
  (ert-info ("Move if on quote")
    (evil-cp-test-buffer
      "(alpha (bravo \"char[l]ie\" delta) echo)"
      (evil-cp-set-additional-movement-keys)
      (")")
      "(alpha (bravo \"charlie[\"] delta) echo)"
      (")")
      "(alpha (bravo \"charlie\" delta[)] echo)")))

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
      "(alpha (bravo ([c]harlie delta) echo) foxtrot)")))
  (ert-info ("Can skip sexp prefix when moving forward")
    (let (sp-sexp-prefix)
      (evil-test-buffer ;; As we need to enable evil-cp later anyway
        "(alpha (b[r]avo x(charlie delta) echo) foxtrot)"
        (emacs-lisp-mode)
        (evil-cleverparens-mode t)
        (setq sp-sexp-prefix '((emacs-lisp-mode regexp "x")))
        ("W")
        "(alpha (bravo x([c]harlie delta) echo) foxtrot)"
        ("B")
        "(alpha ([b]ravo x(charlie delta) echo) foxtrot)"))))

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
      "alpha bravo (cha[r]lie) delta echo"))
  (ert-info ("Stay on closing paren when barfing from it")
    (evil-cp-test-buffer
      "(alpha)\n(bravo charlie delta[)]"
      ("<")
      "(alpha)\n(bravo charlie[)] delta")))

(ert-deftest evil-cp->-test ()
  (ert-info ("Can do regular forward slurp")
    (evil-cp-test-buffer
      "alpha bravo (charlie [d]elta echo) foxtrot golf"
      (">")
      "alpha bravo (charlie [d]elta echo foxtrot) golf"))
  (ert-info ("Can slurp escaped parens")
    (evil-test-buffer ;; As we need to enable evil-cp later anyway
      "(alpha[)] ?\\) bravo"
      (emacs-lisp-mode)
      (evil-cleverparens-mode t)
      (">")
      "(alpha ?\\)) bravo"))
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
  (ert-info ("Can substitute with nothing")
    (evil-cp-test-buffer
      "([x])"
      ("s")
      "()"))
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
  (let ((window-system t))
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
  (let ((window-system t))
    (ert-info ("Is unavailable under terminal")
      (evil-cp-test-buffer
        "(alpha (bravo [c]harlie) delta)"
        (evil-cp-set-additional-bindings)
        (setq window-system nil)
        ("\M-O") ;; same as "\M-o" since "\M-O" is unavailable
        "(alpha (bravo charlie)\n[]delta)"))))

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
  (let ((window-system t))
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
  (let ((window-system t))
    (ert-info ("Is unavailable under terminal")
      (evil-cp-test-buffer
        "(alpha [b]ravo)"
        (evil-cp-set-additional-bindings)
        (setq window-system nil)
        ("\M-[")
        "(alpha [b]ravo)"))))

(ert-deftest evil-cp-wrap-previous-square-test ()
  (let ((window-system t))
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
  (let ((window-system t))
    (ert-info ("Is unavailable under terminal")
      (evil-cp-test-buffer
        "(alpha [b]ravo)"
        (evil-cp-set-additional-bindings)
        (setq window-system nil)
        ("\M-]")
        "(alpha [b]ravo)"))))

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
