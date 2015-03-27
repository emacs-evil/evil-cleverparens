;;; evil-cleverparens.el --- Evil friendly minor-mode for editing lisp.
;;
;; Copyright (C) 2015 Olli Piepponen
;;
;; Author: Olli Piepponen <opieppo@gmail.com>
;; Mantainer: Olli Piepponen <opieppo@gmail.com>
;; Keywords: smartparens, parentheses, evil
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;; Version: 0.0.1

;; URL: https://github.com/luxbock/evil-cleverparens

;; Package-Requires: ((evil "0.0.0")
;;                    (paredit "1")
;;                    (paxedit "1.1.4")
;;                    (drag-stuff "0.1.0")
;;                    (smartparens "1.6.1"))


;;; TODOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add note about copying to the evil-sp funs
;; TODO: Single quotes are interpreted as opening parens in Emacs Lisp

;;; Code:

(require 'evil)
(require 'evil-surround)
(require 'paredit)
(require 'paxedit)
(require 'drag-stuff)
(require 'smartparens)

;;;###autoload
(define-minor-mode evil-cleverparens-mode
  "Minor mode for setting up Evil with smartparens in a single buffer"
  :keymap '()
  :lighter "ecp"
  :init-value nil
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)
    (setq drag-stuff-by-symbol-p t)))

;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup evil-cleverparens nil
  "`evil-mode' for handling your parentheses with a mix of `smartparens' and `paredit'"
  :group 'smartparens)

(defcustom evil-cleverparens-threshold 2500
  "If the region being operated on is larger than this we cop out.

Quite a bit of work gets done to ensure the region being worked
is in an safe state, so this lets us sarifice safety for a snappy
editor on slower computers.

Even on a large computer you shouldn't set this too high or your
computer will freeze when copying large files out of Emacs.

This is a feature copied from `evil-smartparens'."
  :group 'evil-smartparens
  :type 'string)

(defcustom evil-cleverparens-balance-yanked-region t
  "Determines how to handle yanking a region containing
  unbalanced expressions. If this value is non-nil, a yanked
  region containing missing parentheses will include the missing
  parens appended to the end."
 :group 'evil-cleverparens
 :type 'boolean)

(defvar evil-cp--override nil
  "Should the next command skip region checks?")

;;; Overriding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--override ()
  (prog1 (or evil-cp--override
             (evil-cp--region-too-expensive-to-check))
    (setq evil-cp--override nil)))

(defun evil-cp-override ()
  (interactive)
  (setq evil-cp--override t))

(defun evil-cp--region-too-expensive-to-check ()
  "When it takes prohobitively long to check region we cop out."
  (when (region-active-p)
    (> (abs (- (region-beginning) (region-end)))
       evil-cleverparens-threshold)))

;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro evil-cp--if-balanced-yank (then else)
  "Helper macro for doing different things depending on the value
of `evil-cleverparens-balance-yanked-region'."
  (assert (and (listp then) (listp else))
          nil "Both `THEN' and `ELSE' must be lists.")
  `(if evil-cleverparens-balance-yanked-region
       (progn
         ,then)
     (progn
       ,else)))

(defun evil-cp--looking-at-opening-p (&optional pos)
  "Predicate that returns true if point is looking at an opening
paren as defined by smartparens for the major mode in question."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (sp--get-opening-regexp))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-closing-p (&optional pos)
  "Predicate that returns true if point is looking at an closing
paren as defined by smartparens for the major mode in question."
  (save-excursion
    (when pos (goto-char pos))
    (sp--looking-at-p (sp--get-closing-regexp))))

(defun evil-cp--looking-at-paren-p (&optional pos)
  "Predicate that returns true if point is looking at a paren
paren as defined by smartparens for the major mode in question."
  (save-excursion
    (when pos (goto-char pos))
    (sp--looking-at-p (sp--get-allowed-regexp))))

(defun evil-cp--looking-at-opening-quote (&optional pos)
  "Predicate for checking if point is on a opening string delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (looking-at (sp--get-stringlike-regexp))
         (progn
           (forward-char)
           (nth 3 (syntax-ppss))))))

(defun evil-cp--looking-at-closing-quote (&optional pos)
  "Predicate for checking if point is on a closing delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (looking-at (sp--get-stringlike-regexp))
         (not (paredit-in-string-escape-p))
         (progn
           (backward-char)
           (nth 3 (syntax-ppss))))))

(defmacro evil-cp--guard-point (&rest body)
  `(if (or (evil-cp--looking-at-opening-p)
           (evil-cp--looking-at-opening-quote))
       (save-excursion
         (forward-char 1)
         ,@body)
     ,@body))

(defun evil-cp--inside-sexp-p (&optional pos)
  "Predicate for checking if point is inside a sexp."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--looking-at-opening-p) (forward-char))
    (not (zerop (nth 0 (syntax-ppss))))))

(defun evil-cp--inside-string-p (&optional pos)
  "Predicate that returns true if point is inside a string."
  (save-excursion
    (when pos (goto-char pos))
    (let ((string-ppss (nth 3 (syntax-ppss))))
      (or string-ppss
          (progn
            (forward-char)
            (nth 3 (syntax-ppss)))))))

(defun evil-cp--inside-form-p (&optional pos)
  (or (evil-cp--inside-sexp-p pos)
      (evil-cp--inside-string-p pos)))

(defun evil-cp--top-level-sexp-p ()
  "Predicate that returns true if point is inside a top-level sexp."
  (eq (nth 0 (syntax-ppss)) 1))

(defun evil-cp--top-level-form-p ()
  "Predicate that returns true if point is inside a top-level sexp."
  (or (and (eq (nth 0 (syntax-ppss)) 1) (not (sp-point-in-string)))
      (and (nth 3 (syntax-ppss))
           (eq (nth 0 (syntax-ppss)) 0))))

(defun evil-cp--string-bounds (&optional pos)
  "Returns the location of the beginning and ending positions for
a string form. Accepts an optional argument `POS' for moving the
point to that location."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (or (sp-get-quoted-string-bounds)
          (progn
            (forward-char)
            (sp-get-quoted-string-bounds))))))

(defun evil-cp--matching-paren-pos (&optional pos)
  "Finds the location of the matching paren for where point is
located. Also works for strings. Takes an optional `POS' argument
for temporarily moving the point to before proceeding.
  Assumes that the parentheses in the buffer are balanced."
  (save-excursion
    (cond ((evil-cp--looking-at-opening-p)
           (progn
             (sp-forward-sexp 1)
             (backward-char)
             (point)))

          ((evil-cp--looking-at-closing-p)
           (progn
             (forward-char 1)
             (sp-backward-sexp 1)
             (point)))

          ((looking-at "\"")
           (let* ((bounds (evil-cp--string-bounds)))
             (if (= (car bounds) (point))
                 (1- (cdr bounds))
               (car bounds)))))))

(defun evil-cp--text-balanced-p (text)
  "Checks if the string `TEXT' is balanced or not."
  (with-temp-buffer
    (insert text)
    (sp-region-ok-p (point-min) (point-max))))

(defun evil-cp--balanced-block-p (beg end)
  "Checks whether the block defined by `BEG' and `END' contains
balanced parentheses."
  (let ((region (evil-yank-rectangle beg end)))
    (with-temp-buffer
      (insert region)
      (sp-region-ok-p (point-min) (point-max)))))

(defun evil-cp--fail ()
  "Error out with a friendly message."
  (error "Can't find a safe region to act on."))

;;; Text Objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The symbol text object already exists at "o"

(defun evil-cp--get-form-range (&optional pos)
  "Returns a tuple of start/end positions for the surrounding sexp."
  (when (evil-cp--inside-form-p)
    (save-excursion
      (evil-cp--guard-point
       (let ((pos (or pos (point))))
         (let* ((obj (if (sp-point-in-string pos)
                         (sp-get-string t)
                       (sp-get-enclosing-sexp))))
           (list (sp-get obj :beg)
                 (sp-get obj :end))))))))

(evil-define-text-object evil-cp-a-form (count &optional beg end type)
  "Smartparens sexp object."
  (let ((range (evil-cp--get-form-range)))
    (if (not range)
        (error "No surrounding form found.")
      (progn
        (apply 'evil-range
               (append range '(:expanded t)))))))

(evil-define-text-object evil-cp-inner-form (count &optional beg end type)
  "Smartparens inner sexp object."
  (let ((range (evil-cp--get-form-range)))
    (if (not range)
        (error "No surrounding form found.")
      (let ((beg (car range))
            (end (cadr range)))
        (evil-range (1+ beg) (1- end) :expanded t)))))

(evil-define-text-object evil-cp-a-comment (count &optional beg end type)
  (let ((bounds (sp-get-comment-bounds)))
    (if (not bounds)
        (error "Not inside a comment.")
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (evil-range beg end :expanded t)))))

(evil-define-text-object evil-cp-inner-comment (count &optional beg end type)
  (let ((bounds (sp-get-comment-bounds)))
    (if (not bounds)
        (error "Not inside a comment.")
      (let ((beg (save-excursion
                   (goto-char (car bounds))
                   (forward-word 1)
                   (forward-word -1)
                   (point)))
            (end (save-excursion
                   (goto-char (cdr bounds))
                   (evil-end-of-line)
                   (point))))
        (evil-range beg end 'block :expanded t)))))


(evil-define-text-object evil-cp-a-defun (count &optional beg end type)
  (if (evil-cp--inside-sexp-p)
      (let ((bounds
             (save-excursion
               (beginning-of-defun)
               (sp-get (sp-get-sexp) (list :beg :end)))))
        (evil-range (car bounds) (cadr bounds) :expanded t))
    (error "Not inside a sexp.")))

(evil-define-text-object evil-cp-inner-defun (count &optional beg end type)
  (if (evil-cp--inside-sexp-p)
      (let ((bounds
             (save-excursion
               (when (evil-cp--inside-sexp-p)
                 (beginning-of-defun)
                 (sp-get (sp-get-sexp) (list :beg :end-in))))))
        (evil-range (1+ (car bounds)) (1- (cadr bounds)) :expanded t))
    (error "Not inside a sexp.")))

;;; Evil Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--splice-form ()
  "Evil friendly version of splice that takes care of the
location of point, which also works for strings. Unlike
`sp-splice-sexp', this one doesn't perform clean-up for empty
forms."
  (evil-cp--guard-point
   (-when-let (ok (sp-get-enclosing-sexp 1))
     (if (equal ";" (sp-get ok :prefix))
         (sp-get ok
           (goto-char :beg)
           (-when-let (enc (sp-get-enclosing-sexp 1))
             (sp--unwrap-sexp enc t)))
       (sp--unwrap-sexp ok t)))))

(defun evil-cp-delete-or-splice-region (beg end)
  "Deletes a region of text by splicing parens and deleting other
chars. Does not splice strings unless the whole expression is
contained within the region."
  (let ((chars-left (- end beg)))
    (while (> chars-left 0)
      (cond ((evil-cp--looking-at-opening-p)
             (let ((other-paren (evil-cp--matching-paren-pos)))
               (if (< (point) other-paren end)
                   (let* ((range (sp-get (sp-get-sexp) (list :beg :end)))
                          (char-count (- (cadr range) (car range))))
                     (delete-char char-count)
                     (setq chars-left (- chars-left char-count)))
                 (evil-cp--splice-form)
                 (cl-decf chars-left))))

            ((evil-cp--looking-at-opening-quote)
             (let ((other-quote (evil-cp--matching-paren-pos)))
               (message "otherq: %d, end: %d" other-quote end)
               (if (< (point) other-quote end)
                   (let* ((range (sp-get (sp-get-string) (list :beg :end)))
                          (char-count (- (cadr range) (car range))))
                     (delete-char char-count)
                     (setq chars-left (- chars-left char-count)))
                 (forward-char)
                 (cl-decf chars-left))))

            ((evil-cp--looking-at-paren-p)
             (evil-cp--splice-form)
             (cl-decf chars-left))

            ((looking-at (sp--get-stringlike-regexp))
             (forward-char)
             (cl-decf chars-left))

            (t
             (delete-char 1)
             (cl-decf chars-left))))))

(evil-define-operator evil-cp-delete-char-or-splice (beg end type register yank-handler)
  "Deletes the region by splicing parentheses / string delimiters
and deleting other characters. Can be overriden by
`evil-cp-override' in visual-mode."
  :motion evil-forward-char
  (interactive "<R><x>")
  (cond ((or (evil-cp--balanced-block-p beg end)
             (evil-cp--override))
         (evil-delete-char beg end type register))

        ((eq type 'block)
         (evil-cp--yank-rectangle beg end register yank-handler)
         (evil-apply-on-block #'evil-cp-delete-or-splice-region beg end nil))

        (t
         (if evil-cleverparens-balance-yanked-region
             (evil-cp-yank beg end type register yank-handler)
           (evil-yank beg end type register yank-handler))
         (evil-cp-delete-or-splice-region beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original from:
;; http://emacs.stackexchange.com/questions/777/closing-all-pending-parenthesis
(defun evil-cp--insert-missing-parentheses (backp)
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (if (not backp)
                       (backward-up-list)
                     (progn
                       (forward-char)
                       (up-list)
                       (backward-char)))
                   (let* ((syntax (syntax-after (point)))
                          (head   (car syntax)))
                     (cond
                      ((eq head (if backp 5 4))
                       (setq closing (cons (cdr syntax) closing)))
                      ((memq head '(7 8)) ;; TODO: Not sure what this even does?
                       (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (when backp (beginning-of-buffer))
    (apply #'insert (nreverse closing))))

(defun evil-cp--close-missing-parens (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-max))
    (evil-cp--insert-missing-parentheses nil)
    ;; TODO: This is far from optimal
    (when (not (sp-region-ok-p (point-min) (point-max)))
      (goto-char (point-min))
      (evil-cp--insert-missing-parentheses t))
    (buffer-string)))

(defun evil-cp--region-has-unbalanced-string (beg end)
  (not (evenp (count-matches (sp--get-stringlike-regexp) beg end))))

(defun evil-cp--yank-characters
    (beg end &optional register yank-handler add-parens-p)
  "Yank characters while keeping parentheses balanced. The
optional `ADD-PARENS-P' arg determines how to handle the missing
parentheses: if nil, the non-balanced parens are
ignored. Otherwise they are added to the start/beginning of the
region."
  (unless (evil-cp--region-has-unbalanced-string beg end)
    (let ((text (string-trim (filter-buffer-substring beg end))))
      (when (not (evil-cp--text-balanced-p text))
        (setq text (evil-cp--close-missing-parens text)))
      (when yank-handler
        (setq text (propertize text 'yank-handler (list yank-handler))))
      (when register
        (evil-set-register register text))
      (when evil-was-yanked-without-register
        (evil-set-register ?0 text)) ; "0 register contains last yanked text
      (unless (eq register ?_)
        (kill-new text)))))

(defun evil-cp--yank-rectangle (beg end &optional register yank-handler)
  "Saves the rectangle defined by BEG and END into the kill-ring,
  while keeping the parentheses of the region balanced."
  (let ((lines (list nil)))
    (evil-apply-on-rectangle #'extract-rectangle-line beg end lines)
    (setq lines (nreverse (cdr lines)))
    (let* ((yank-handler (list (or yank-handler #'evil-yank-block-handler)
                               lines
                               nil
                               'evil-delete-yanked-rectangle))
           (joined (mapconcat #'identity lines "\n"))
           (text (if (evil-cp--text-balanced-p joined)
                     joined
                   (evil-cp--close-missing-parens joined)))
           (text (propertize text 'yank-handler yank-handler)))
      (when register (evil-set-register register text))
      (when evil-was-yanked-without-register (evil-set-register ?0 text))
      (unless (eq register ?_)
        (kill-new text)))))

(defun evil-cp--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun evil-cp--safe-yank (beg end &optional type yank-handler)
  (condition-case nil
      (let ((new-beg (evil-cp--new-beginning beg end))
            (new-end (evil-cp--new-ending beg end)))
        (if (and (= new-end end)
                 (= new-beg beg))
            (evil-yank beg end type yank-handler)
          (evil-yank new-beg new-end 'inclusive yank-handler)))
    (error (let* ((beg (evil-cp--new-beginning beg end :shrink))
                  (end (evil-cp--new-ending beg end)))
             (evil-yank beg end type yank-handler)))))

(defun evil-cp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG.

Copied from `evil-smartparens'."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-sp--point-after 'sp-backward-up-sexp)
             (point-max)))

    (let ((region (string-trim (buffer-substring-no-properties beg end))))
      (unless (string-blank-p region)
        (cond ((sp-point-in-empty-sexp)
               ;; expand region if we're in an empty sexp
               (setf end (save-excursion (sp-backward-up-sexp) (point))))

              ;; reduce region if it's unbalanced due to selecting too much
              (t (while (not (or (sp-region-ok-p beg end)
                                 (= beg end)))
                   (cl-incf beg)))))
      (when (= beg end)
        (evil-sp--fail)))
    beg))

(defun evil-cp--new-ending (beg end &optional no-error)
  "Find the largest safe region delimited by BEG END.

Copied from `evil-smartparens'."
  (let ((region (string-trim (buffer-substring-no-properties beg end))))
    (unless (string-blank-p region)
      (cond ((sp-point-in-empty-sexp)
             ;; expand region if we're in an empty sexp
             (setf end (save-excursion (sp-up-sexp) (point))))

            ;; reduce region if it's unbalanced due to selecting too much
            (t (while (not (or (sp-region-ok-p beg end)
                               (= beg end)))
                 (cl-decf end))))))
  (if (and (not no-error) (= beg end))
      (evil-cp--fail)
    end))

(defun evil-cp--safe-yank (beg end type register yank-handler)
  "Copied from `evil-smartparens'."
  (condition-case nil
      (let ((new-beg (evil-cp--new-beginning beg end))
            (new-end (evil-cp--new-ending beg end)))
        (if (and (= new-end end)
                 (= new-beg beg))
            ((evil-yank beg end type register yank-handler))
          (evil-yank new-beg new-end 'inclusive yank-handler)))
    (error (let* ((beg (evil-cp--new-beginning beg end :shrink))
                  (end (evil-cp--new-ending beg end)))
             ((evil-yank beg end type register yank-handler))))))

(evil-define-operator evil-cp-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring while
respecting parentheses."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register)))
        (safep (sp-region-ok-p beg end)))
    (cond
     ;; block which is balanced
     ((and (eq type 'block) (evil-cp--balanced-block-p beg end))
      (evil-yank-rectangle beg end register yank-handler))

     ;; unbalanced block, don't add parens
     ((and (eq type 'block)
           (not evil-cleverparens-balance-yanked-region))
      (evil-cp--fail))

     ;; unbalanced block, add parens
     ((eq type 'block)
      (evil-cp--yank-rectangle beg end register yank-handler))

     ;; safe region
     ((or safep (evil-cp--override))
      (evil-yank beg end type register yank-handler))

     (evil-cleverparens-balance-yanked-region
      (when (= (char-after end) 32) (setq end (1- end)))
      (evil-cp--yank-characters beg end register yank-handler))

     (t
      (evil-cp--safe-yank beg end type yank-handler)))))

(evil-define-operator evil-cp-yank-line (beg end type register)
  "Acts like `paredit-copy-sexp-as-kill'."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (cond
   ((evil-visual-state-p)
    (let ((beg (point-at-bol))
          (end (if (eq type 'line)
                   (1- end)
                 (save-excursion
                   (goto-char end)
                   (point-at-eol)))))
      (evil-exit-visual-state)
      (evil-cp--yank-characters beg end type register)))

   ;; Copied and modified from paredit.el
   ((paredit-in-string-p)
    (save-excursion
      (when (paredit-in-string-escape-p)
        (backward-char))
      (evil-yank-characters (point)
                            (min (point-at-eol)
                                 (cdr (paredit-string-start+end-points)))
                            register)))

   ((paredit-in-comment-p)
    (evil-yank-characters (point) (point-at-eol) register))

   ((save-excursion (paredit-skip-whitespace t (point-at-eol))
                    (or (eolp) (eq (char-after) ?\; )))
    (save-excursion
      (if (paredit-in-char-p)
          (backward-char)))
    (evil-yank-characters (point) (point-at-eol) register))

   (t
    (save-excursion
      (if (paredit-in-char-p)
          (backward-char 2))
      (let ((beginning (point))
            (eol (point-at-eol)))
        (let ((end-of-list-p (paredit-forward-sexps-to-kill beginning eol)))
          (if end-of-list-p (progn (up-list) (backward-char)))
          (evil-yank-characters beginning
                                (cond (kill-whole-line
                                       (or (save-excursion
                                             (paredit-skip-whitespace t)
                                             (and (not (eq (char-after) ?\; ))
                                                  (point)))
                                           (point-at-eol)))
                                      ((and (not end-of-list-p)
                                            (eq (point-at-eol) eol))
                                       eol)
                                      (t
                                       (point)))
                                register)))))))

;; smartparens, but should only be active inside strings.
;; TODO: Ask Fuco about this.
(defun evil-cp--delete-characters (beg end)
  "Deletes everything except unbalanced parentheses / string
delimiters in the region defined by `BEG' and `END'."
  (goto-char beg)
  (while (< (point) end)
    (cond
     ;; opening
     ((or (evil-cp--looking-at-opening-p)
          (evil-cp--looking-at-opening-quote))
      (let ((matching-pos (evil-cp--matching-paren-pos)))
        (if (<= (point) matching-pos end)
            (progn
              (evil-cp--splice-form)
              (setq end (- end 2)))
          (forward-char 1))))

     ;; closing that has survived
     ((or (evil-cp--looking-at-closing-p)
          (evil-cp--looking-at-closing-quote))
      (forward-char 1))

     ;; character
     (t
      (delete-char 1)
      (setq end (1- end)))))
  (backward-char))

(defun evil-cp--first-non-blank-non-paren ()
  "Like `evil-first-non-blank' but also skips opening parentheses."
  (evil-first-non-blank)
  (while (or (evil-cp--looking-at-opening-p)
             (>= (point) (point-at-eol)))
    (forward-char)))

(evil-define-operator evil-cp-delete (beg end type register yank-handler)
  :move-point nil
  (interactive "<R><x><y>")
  (let ((safep (sp-region-ok-p beg end)))
    (evil-cp-yank beg end type register yank-handler)
    (cond ((or (= beg end)
               (evil-cp--override)
               (and (eq type 'block) (evil-cp--balanced-block-p beg end))
               (and safep (not (eq type 'block))))
           (evil-delete beg end type register yank-handler))

          ((eq type 'line)
           (save-excursion
             (evil-cp--delete-characters
              (+ beg (sp-forward-whitespace t)) (1- end)))
           (when (not (or (evil-cp--looking-at-closing-p)
                          (evil-cp--looking-at-closing-quote)))
             (evil-join (point-at-bol) (point-at-bol 2)))
           (evil-cp--first-non-blank-non-paren))

          (t (evil-cp--delete-characters beg end)))))

(evil-define-operator evil-cp-delete-line (beg end type register yank-handler)
  :motion nil
  :keep-visual t
  :move-point nil
  (interactive "<R><x>")
  (cond ((evil-visual-state-p)
         ;; Not sure what this should do in visual-state
         (let ((safep (sp-region-ok-p beg end)))
           (if (not safep)
               ((evil-cp--fail))
             (evil-delete-line beg end type register yank-handler))))

        ((paredit-in-string-p)
         (save-excursion
           (when (paredit-in-string-escape-p)
             (backward-char))
           (let ((beg (point))
                 (end (min (point-at-eol)
                           (cdr (paredit-string-start+end-points)))))
             (evil-yank-characters beg end register)
             (delete-region beg end))))

        ((paredit-in-comment-p)
         (let ((beg (point))
               (end (point-at-eol)))
           (evil-yank-characters beg end register)
           (delete-region beg end)))

        ((save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (or (eolp) (eq (char-after) ?\; )))
         (save-excursion
           (when (paredit-in-char-p)
             (backward-char)))
         (let ((beg (point))
               (end (point-at-eol)))
           (evil-yank-characters (point) (point-at-eol) register)
           (delete-region beg end)))

        (t
         (save-excursion
           (when (paredit-in-char-p) (backward-char 2))
           (let* ((beg (point))
                  (eol (point-at-eol))
                  (end-of-list-p (paredit-forward-sexps-to-kill beg eol)))
             (when end-of-list-p (up-list) (backward-char))
             (let ((end (cond
                         (kill-whole-line
                          (or (save-excursion
                                (paredit-skip-whitespace t)
                                (and (not (eq (char-after) ?\; ))
                                     (point)))
                              (point-at-eol)))
                         ((and (not end-of-list-p)
                               (eq (point-at-eol) eol))
                          eol)
                         (t
                          (point)))))
               (evil-yank-characters beg end register)
               (delete-region beg end)))))))

(evil-define-operator evil-cp-change
  (beg end type register yank-handler delete-func)
  "Call `evil-change' while keeping parentheses balanced."
  :move-point nil
  (interactive "<R><x><y>")
  (if (or (= beg end)
          (evil-cp--override)
          (and (eq type 'block) (evil-cp--balanced-block-p beg end))
          (and (sp-region-ok-p beg end) (not (eq type 'block))))
      (evil-change beg end type register yank-handler delete-func)
    (let ((delete-func (or delete-func #'evil-cp-delete))
          (nlines (1+ (- (line-number-at-pos end)
                         (line-number-at-pos beg))))
          (opoint (save-excursion
                    (goto-char beg)
                    (line-beginning-position))))
      (cond ((eq type 'line)
             (save-excursion
               (evil-cp--delete-characters
                (+ beg (sp-forward-whitespace t)) (1- end)))
             (evil-insert 1))

            ((eq type 'block)
             (evil-cp-delete beg end type register yank-handler)
             (evil-insert 1 nlines))

            (t
             (evil-cp-delete beg end type register yank-handler)
             (forward-char)
             (evil-insert 1))))))

(evil-define-operator evil-cp-change-line (beg end type register yank-handler)
  "Change to end of line while respecting parentheses."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-cp-change beg end type register yank-handler #'evil-cp-delete-line))

(evil-define-operator evil-change-whole-line (beg end type register yank-handler)
  "Change whole line while respecting parentheses."
  ;; :motion evil-line
  (interactive "<R><x>")
  (evil-first-non-blank)
  (while (or (evil-cp--looking-at-opening-p)
             (evil-cp--looking-at-opening-quote))
    (evil-forward-char 1 nil t))
  (evil-cp-change-line beg (1- end) type register yank-handler #'evil-cp-delete-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-motion evil-cp-forward-sexp (count)
  :type exclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (sp-forward-sexp count)))

(evil-define-motion evil-cp-backward-sexp (count)
  :type exclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (sp-backward-sexp count)))

(evil-define-motion evil-cp-beginning-of-defun (count)
  :type exclusive
  (let ((count (or count 1)))
    (beginning-of-defun count)))

(evil-define-motion evil-cp-end-of-defun (count)
  :type exclusive
  (let ((count (or count 1)))
    (if (save-excursion
          (evil-cp--looking-at-closing-p))
        (forward-char 2))
    (end-of-defun count)
    (backward-char 2)))

(evil-define-motion evil-cp-next-opening (count)
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (when (evil-cp--looking-at-opening-p) (forward-char))
    (re-search-forward (sp--get-opening-regexp) nil t count)
    (backward-char)))

(evil-define-motion evil-cp-previous-opening (count)
  :type inclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (re-search-backward (sp--get-opening-regexp) nil t count)))

(evil-define-motion evil-cp-next-closing (count)
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (when (evil-cp--looking-at-closing-p) (forward-char))
    (re-search-forward (sp--get-closing-regexp) nil t count)
    (backward-char)))

(evil-define-motion evil-cp-previous-closing (count)
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (re-search-backward (sp--get-closing-regexp) nil t count)))

(evil-define-motion evil-cp-forward-symbol-begin (count)
  "Copy of `evil-forward-word-begin' using 'evil-symbol for the
movement."
  :type exclusive
  (let ((thing 'evil-symbol)
        (orig (point))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (cond ((not (evil-operator-state-p))
           (evil-forward-beginning thing count))

          ((and evil-want-change-word-to-end
                (eq evil-this-operator #'evil-change)
                (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
           (forward-thing thing count))

          (t (prog1 (evil-forward-beginning thing count)
               (when (and (> (line-beginning-position) orig)
                          (looking-back "^[[:space:]]*" (line-beginning-position)))
                 (evil-move-end-of-line 0)
                 (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                             (not (<= (line-beginning-position) orig)))
                   (evil-move-end-of-line 0))
                 (when (bolp) (forward-char))))))))

(evil-define-motion evil-cp-forward-symbol-end (count)
  "Copy of `evil-forward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (let ((thing 'evil-symbol)
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (unless (and (evil-operator-state-p)
                 (= 1 count)
                 (let ((bnd (bounds-of-thing-at-point thing)))
                   (and bnd
                        (= (car bnd) (point))
                        (= (cdr bnd) (1+ (point)))))
                 (looking-at "[[:word:]]"))
      (evil-forward-end thing count))))

(evil-define-motion evil-cp-backward-symbol-begin (count)
  "Copy of `evil-backward-word-begin' using 'evil-symbol for the
movement."
  :type exclusive
  (let ((thing 'evil-symbol))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning thing count)))

(evil-define-motion evil-cp-backward-symbol-end (count)
  "Copy of `evil-backward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (let ((thing 'evil-symbol))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-end thing count)))

(defun evil-cp-left-paren (&optional n)
  (interactive "p")
  (re-search-backward (sp--get-opening-regexp) nil t n))

(defun evil-cp-right-paren (&optional n)
  (interactive "p")
  (when (evil-cp--looking-at-closing-p)
    (forward-char))
  (re-search-forward (sp--get-closing-regexp) nil t n)
  (backward-char))


;; TODO: Insert space automatically when slurping
(defun evil-cp-< (n)
  (interactive "p")
  (let ((at (char-after)))
    (cond ((eq at ?\()
           (dotimes (_ n)
             (forward-char)
             (paredit-backward-slurp-sexp)
             (paredit-backward)
             (evil-cp-left-paren)))

          ((eq at ?\))
           (dotimes (_ n)
             (paredit-forward-barf-sexp)
             (paredit-backward)
             (re-search-backward (sp--get-closing-regexp))))

          (t (paredit-forward-barf-sexp n)))))

(defun evil-cp-> (n)
  (interactive "p")
  (let ((at (char-after)))
    (cond ((eq at ?\()
           (dotimes (_ n)
             (forward-char)
             (paredit-backward-barf-sexp)
             (paredit-forward)
             (re-search-forward (sp--get-opening-regexp))
             (backward-char)))

          ((eq at ?\))
           (dotimes (_ n)
             (paredit-forward-slurp-sexp)
             (paredit-forward)))

          (t (paredit-forward-slurp-sexp n)))))

(defun evil-cp--line-safe-p (&optional move-fn)
  (save-excursion
    (when move-fn (funcall move-fn))
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (parsed (parse-partial-sexp beg end))
           (sexps-ok-p (= (nth 0 parsed) 0))
           (in-string-p (nth 3 parsed)))
      (and sexps-ok-p (not in-string-p)))))

(defun evil-cp--transpose (this-fn)
  (save-excursion
    (when ((evil-cp--looking-at-opening-p))
      (forward-char))
    (sp-end-of-sexp)
    (funcall this-fn)))

(defun evil-cp-transpose-sexp-backward (&optional n)
  (interactive "p")
  (evil-cp--transpose 'paxedit-transpose-backward))

(defun evil-cp-transpose-sexp-forward (&optional n)
  (interactive "p")
  (evil-cp--transpose 'paxedit-transpose-forward))

(defun evil-cp--drag-stuff-up-or-down (dir &optional n)
  (assert (member dir '(:up :down)) t "`dir' has to be :up or :down")
  (let ((n (or n 1))
        (drag-fn (if (eq dir :up) 'drag-stuff-up 'drag-stuff-down)))
    (catch 'stop
      (dotimes (_ n)
        (let ((in-sexp-p (evil-cp--inside-sexp-p))
              (line-safe-p (evil-cp--line-safe-p
                            (lambda ()
                              (if (eq dir :up)
                                  (forward-line -1)
                                (forward-line 1))))))
          (cond
           (in-sexp-p
            (funcall (if (eq dir :up)
                         'evil-cp-transpose-sexp-backward
                       'evil-cp-transpose-sexp-forward)
                     1))
           (line-safe-p (funcall drag-fn 1))
           (t (throw 'stop nil))))))))

(defun evil-cp-drag-up (&optional n)
  (interactive "p")
  (evil-cp--drag-stuff-up-or-down :up n))

(defun evil-cp-drag-down (&optional n)
  (interactive "p")
  (evil-cp--drag-stuff-up-or-down :down n))

(defun evil--parens-drag-sideways (dir-fn check-oob n)
  (catch 'stop
    (dotimes (_ n))
    (if (or (looking-at " ")
            (looking-at "\"")
            (evil-cp--looking-at-paren-p))
        (throw 'stop (message "Can only drag elements."))
      (if (and (evil-cp--top-level-sexp-p)
               (string-equal (thing-at-point 'symbol t)
                             (save-excursion (funcall check-oob))))
          (throw 'stop (message "Can't move element outside a top-level sexp."))
        (funcall dir-fn 1)))))

(defun evil-cp-drag-right (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (drag-stuff-symbol-right n)
    ;; (evil--parens-drag-sideways
    ;;  'drag-stuff-symbol-right
    ;;  (lambda ()
    ;;    (end-of-defun)
    ;;    (backward-char 2)
    ;;    (while (not (thing-at-point 'symbol)) (backward-char))
    ;;    (thing-at-point 'symbol t))
    ;;  n)
    ))

(defun evil-cp-drag-left (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (drag-stuff-symbol-left n)
    ;; (evil--parens-drag-sideways
    ;;  'drag-stuff-symbol-left
    ;;  (lambda ()
    ;;    (beginning-of-defun)
    ;;    (forward-char)
    ;;    (while (not (thing-at-point 'symbol)) (forward-char))
    ;;    (thing-at-point 'symbol t))
    ;;  n)
    ))

;; (evil-define-operator evil-cp-substitute-or-change-surround (beg end type register)
;;   (if (evil-cp--looking-at-paren-p)
;;       (evil-surround-change )))

(evil-define-key 'normal evil-cleverparens-mode-map
  (kbd "H")   #'sp-backward-sexp
  (kbd "L")   #'sp-forward-sexp
  (kbd "W")   #'evil-cp-forward-symbol-begin
  (kbd "E")   #'evil-cp-forward-symbol-end
  (kbd "B")   #'evil-cp-backward-symbol-begin
  (kbd "gE")  #'evil-cp-backward-symbol-end
  (kbd "L")   #'evil-cp-forward-sexp
  (kbd "H")   #'evil-cp-backward-sexp
  (kbd "[")   #'evil-cp-previous-opening
  (kbd "]")   #'evil-cp-next-closing
  (kbd "{")   #'evil-cp-previous-closing
  (kbd "}")   #'evil-cp-next-opening
  (kbd "(")   #'evil-cp-beginning-of-defun
  (kbd ")")   #'evil-cp-end-of-defun
  (kbd "d")   #'evil-cp-delete
  (kbd "c")   #'evil-cp-change
  (kbd "y")   #'evil-cp-yank
  (kbd "D")   #'evil-cp-delete-line
  (kbd "C")   #'evil-cp-change-line
  (kbd "s")   #'evil-cp-substitute-or-change-surround
  (kbd "S")   #'evil-cp-change-whole-line
  (kbd "Y")   #'evil-cp-yank-line
  (kbd "x")   #'evil-cp-delete-char-or-splice
  (kbd "X")   #'evil-cp-delete-or-splice
  (kbd ">")   #'evil-cp->
  (kbd "<")   #'evil-cp-<
  (kbd "M-k") #'evil-cp-drag-up
  (kbd "M-j") #'evil-cp-drag-down
  (kbd "M-h") #'evil-cp-drag-left
  (kbd "M-l") #'evil-cp-drag-right
  (kbd "M-J") #'paredit-join-sexps
  (kbd "M-d") #'paxedit-kill
  (kbd "M-y") #'paxedit-copy
  (kbd "M-s") #'paredit-split-sexp
  (kbd "M-S") #'paredit-splice-sexp
  (kbd "M-r") #'paxedit-sexp-raise
  ;; TODO: evil-cp-raise-sexp that's guarded
  ;; TODO: raise-some
  ;; TODO: insert at start of sexp
  ;; TODO: insert at the end of sexp
  ;; TODO: insert start of parent sexp
  ;; TODO: insert at the end of parent sexp
  ;; TODO: paxedit-sexp-raise acts weird
  )

(evil-define-key 'visual evil-cleverparens-mode-map
  (kbd "o") #'evil-cp-override)

(evil-define-key 'motion evil-cleverparens-mode-map
  (kbd "[")   #'evil-cp-previous-opening
  (kbd "]")   #'evil-cp-next-closing
  (kbd "{")   #'evil-cp-previous-closing
  (kbd "}")   #'evil-cp-next-opening
  (kbd "L")   #'evil-cp-forward-sexp
  (kbd "H")   #'evil-cp-backward-sexp)

;; Text objects

(define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
(define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(define-key evil-outer-text-objects-map "c" #'evil-cp-a-comment)
(define-key evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
(define-key evil-outer-text-objects-map "d" #'evil-cp-a-defun)
(define-key evil-inner-text-objects-map "d" #'evil-cp-inner-defun)

(provide 'evil-cleverparens)

;;; evil-parens.el ends here
