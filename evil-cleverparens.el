;;; evil-cleverparens.el --- Evil friendly minor-mode for editing lisp.
;;
;; Copyright (C) 2015 Olli Piepponen
;;
;; Author: Olli Piepponen <opieppo@gmail.com>
;; URL: https://github.com/luxbock/evil-cleverparens
;; Keywords: cleverparens, parentheses, evil, paredit, smartparens
;; Version: 0.1.0
;; Package-Requires: ((evil "1.0") (paredit "1") (smartparens "1.6.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;;; Commentary:

;; Use Vim/evil like modal editing with lisp without screwing up the structure
;; of your code. Tries to offer useful alternatives for behavior which would
;; otherwise be destructive.

;; TODO: text-objects break when they contain strings with delimiters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'dash)
(require 'evil)
(require 'paredit)
(require 'smartparens)
(require 'subr-x)


;;; Overriding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--override ()
  "Can be used as a predicate to determine if the next operation
should default to using regular evil functions. Resets the
`evil-cp--override' variable back to nil."
  (prog1 (or evil-cp--override
             (evil-cp--region-too-expensive-to-check))
    (setq evil-cp--override nil)))

(defun evil-cp-override ()
  "Calling this function will have evil-cleverparens default to
the regular evil equivalent of the next command that gets called."
  (interactive)
  (setq evil-cp--override t))

(defun evil-cp--region-too-expensive-to-check ()
  "When it takes prohobitively long to check region we cop out.

This is a feature copied from `evil-smartparens'."
  (when (region-active-p)
    (> (abs (- (region-beginning) (region-end)))
       evil-cleverparens-threshold)))


;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--looking-at-string-delimiter-p ()
  "Predicate for checking if the point is on a string delimiter."
  (and (looking-at (sp--get-stringlike-regexp))
       (not (paredit-in-string-escape-p))))

(defvar evil-cp--pair-list
  '(("(" . ")") ("[" . "]") ("{" . "}") ("\"" . "\""))
  "List of parentheses pairs recognized by evil-cleverparens.")

(defun evil-cp--pair-for (pair pairs)
  (cond
   ((not pairs)
    (message "Pair for %s not found." pair))
   ((string= pair (caar pairs))
    (car pairs))
   (t
    (evil-cp--pair-for pair (cdr pairs)))))

(defun evil-cp-pair-for (pair)
  (evil-cp--pair-for pair evil-cp--pair-list))

(defun evil-cp--get-opening-regexp ()
  (sp--strict-regexp-opt (--map (car it) evil-cp--pair-list)))

(defun evil-cp--get-closing-regexp ()
  (sp--strict-regexp-opt (--map (cdr it) evil-cp--pair-list)))

(defun evil-cp--looking-at-opening-p (&optional pos)
  "Predicate that returns true if point is looking at an opening
parentheses as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (evil-cp--get-opening-regexp))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-closing-p (&optional pos)
  "Predicate that returns true if point is looking at an closing
paren as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (evil-cp--get-closing-regexp))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-paren-p (&optional pos)
  "Predicate that returns true if point is looking at a
parentheses as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (sp--get-allowed-regexp))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-any-delimiter (&optional pos)
  "Predicate that returns true if point is on top of a
  parentheses or a string delimiter as defined by smartparens for
  the major mode in question."
  (save-excursion
    (when pos (goto-char pos))
    (or (sp--looking-at-p (sp--get-stringlike-regexp))
        (evil-cp--looking-at-paren-p))))

(defun evil-cp--looking-at-string-opening-p (&optional pos)
  "Predicate for checking if point is on a opening string delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (evil-cp--looking-at-string-delimiter-p)
         (progn
           (forward-char)
           (nth 3 (syntax-ppss))))))

(defun evil-cp--looking-at-string-closing-p (&optional pos)
  "Predicate for checking if point is on a closing delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (evil-cp--looking-at-string-delimiter-p)
         (not (paredit-in-string-escape-p))
         (nth 3 (syntax-ppss)))))

(defun evil-cp--looking-at-any-opening-p (&optional pos)
  "Predicate to check if point (or POS) is on an opening
parentheses or a string delimiter."
  (or (evil-cp--looking-at-opening-p pos)
      (evil-cp--looking-at-string-opening-p pos)))

(defun evil-cp--looking-at-any-closing-p (&optional pos)
  "Predicate to check if point (or POS) is on an opening
parentheses or a string delimiter."
  (or (evil-cp--looking-at-closing-p pos)
      (evil-cp--looking-at-string-closing-p pos)))

(defmacro evil-cp--guard-point (&rest body)
  "Evil/Vim and Emacs have different opinions on where the point
is with respect to the visible cursor. This macro is used to make
sure that commands that are used to the Emacs view still work
when the cursor in evil is on top of an opening parentheses or a
string delimiter."
  `(if (evil-cp--looking-at-any-opening-p)
       (save-excursion
         (forward-char 1)
         ,@body)
     ,@body))

(defun evil-cp--looking-at-empty-form ()
  "A predicate for checking if the point is currently looking at
an empty form."
  (evil-cp--guard-point
   (or (sp-point-in-empty-sexp)
       (sp-point-in-empty-string))))

(defun evil-cp--inside-form-p (&optional pos)
  "Predicate for checking if point is inside a sexp."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--looking-at-opening-p) (forward-char))
    (not (zerop (nth 0 (syntax-ppss))))))

(defun evil-cp--inside-string-p (&optional pos)
  "Predicate that returns true if point is inside a string."
  (save-excursion
    (when pos (goto-char pos))
    (when (not (or (eobp) (bobp)))
      (let ((string-ppss (nth 3 (syntax-ppss))))
        (or string-ppss
            (progn
              (forward-char)
              (nth 3 (syntax-ppss))))))))

(defun evil-cp--inside-any-form-p (&optional pos)
  "Predicate that returns true if point is either inside a sexp
or a string."
  (or (evil-cp--inside-form-p pos)
      (evil-cp--inside-string-p pos)))

(defun evil-cp--top-level-form-p (&optional pos)
  "Predicate that returns true if point is inside a top-level sexp."
  (save-excursion
    (when pos (goto-char pos))
    (evil-cp--guard-point
     (let* ((ppss (syntax-ppss))
            (n0   (nth 0 ppss))
            (n3   (nth 3 ppss)))
       (or (and (eq n0 1) (not n3)) ; top-level sexp
           (and (eq n0 0) n3))))))    ; top-level string

(defun evil-cp--string-bounds (&optional pos)
  "Returns the location of the beginning and ending positions for
a string form. Accepts an optional argument POS for moving the
point to that location."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (or (sp-get-quoted-string-bounds)
          (progn
            (forward-char)
            (sp-get-quoted-string-bounds))))))

(defun evil-cp--skip-whitespace-and-comments ()
  "Skips whitespace and comments forward."
  (let ((ws-regex "[ \n\r\t]"))
    (while (or (looking-at ws-regex))
      (skip-chars-forward ws-regex)
      (when (looking-at sp-comment-char)
        (forward-line)))))

(defun evil-cp--backward-up-list (&optional ignore-strings-p)
  "Workaround for `backward-up-list' not working inside strings.
If IGNORE-STRINGS-P is t then strings are ignored when moving up.
Otherwise they are treated as lists. Returns the location of
point when the operation was successful."
  (interactive)
  (when (let ((sppss (syntax-ppss)))
          (or (not (zerop (car sppss)))
              (nth 3 sppss)))
    (if (not (in-string-p))
        (progn
          (backward-up-list)
          (point))
      (when (not (and ignore-strings-p (evil-cp--top-level-form-p)))
        (while (in-string-p)
          (backward-char))
        (when ignore-strings-p
          (backward-up-list))
        (point)))))

(defun evil-cp--up-list (&optional ignore-strings-p)
  "Workaround for `up-list' not working inside strings. If
IGNORE-STRINGS-P is t then strings are ignored when moving up.
Otherwise they are treated as lists. Returns the location of
point when the operation was successful."
  (interactive)
  (when (let ((sppss (syntax-ppss)))
          (or (not (zerop (car sppss)))
              (nth 3 sppss)))
    (if (not (in-string-p))
        (progn
          (up-list)
          (point))
      (when (not (and ignore-strings-p (evil-cp--top-level-form-p)))
        (while (in-string-p)
          (forward-char))
        (when ignore-strings-p
          (up-list))
        (point)))))

(defun evil-cp--top-level-bounds (&optional pos)
  "Returns the bounds for the top-level form point is in, or POS
if given. Note that this is different from defun-bounds, as defun
would ignore top-level forms that are on the same line."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--inside-form-p)
      (catch 'done
        (while t
          (evil-cp--backward-up-list)
          (when (evil-cp--top-level-form-p)
            (throw 'done (cons (point) (forward-list)))))))))

(defun evil-cp--beginning-of-top-level ()
  (when (evil-cp--inside-form-p)
    (goto-char (car (evil-cp--top-level-bounds)))))

(defun evil-cp--end-of-top-level ()
  (when (evil-cp--inside-form-p)
    (goto-char (cdr (evil-cp--top-level-bounds)))))

(defun evil-cp--matching-paren-pos (&optional pos)
  "Finds the location of the matching paren for where point is
located. Also works for strings. Takes an optional POS argument
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
  "Checks if the string TEXT is balanced or not."
  (with-temp-buffer
    (insert text)
    (sp-region-ok-p (point-min) (point-max))))

(defun evil-cp--balanced-block-p (beg end)
  "Checks whether the block defined by BEG and END contains
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

(defun evil-cp--sp-obj-bounds (thing)
  (sp-get thing
    (when :beg (cons :beg :end))))

(defun evil-cp--get-enclosing-bounds (&optional pos)
  "Returns a tuple of start/end positions for the surrounding sexp."
  (when (evil-cp--inside-any-form-p)
    (when pos (goto-char pos))
    (save-excursion
      (evil-cp--guard-point
       (evil-cp--sp-obj-bounds
        (if (sp-point-in-string (point))
            (sp-get-string t)
          (sp-get-enclosing-sexp)))))))

(defun evil-cp--next-sexp-bounds (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (when (or (evil-cp--looking-at-any-opening-p)
              (looking-at-p "\\b"))
      (evil-cp--movement-bounds (forward-sexp)))))

(evil-define-text-object evil-cp-a-form (count &optional beg end type)
  "Smartparens sexp object."
  (let* ((bounds (evil-cp--get-enclosing-bounds)))
    (if (not bounds)
        (error "No surrounding form found.")
      ;; I'm not sure what difference 'inclusive / 'exclusive makes here
      (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))))

(evil-define-text-object evil-cp-inner-form (count &optional beg end type)
  "Smartparens inner sexp object."
  (let ((range (evil-cp--get-enclosing-bounds)))
    (if (not range)
        (error "No surrounding form found.")
      (let ((beg (car range))
            (end (cdr range)))
        (evil-range (1+ beg) (1- end) 'inclusive :expanded t)))))

(evil-define-text-object evil-cp-a-comment (count &optional beg end type)
  "An outer comment text object as defined by `sp-get-comment-bounds'."
  (let ((bounds (sp-get-comment-bounds)))
    (if (not bounds)
        (error "Not inside a comment.")
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (evil-range beg end 'exclusive :expanded t)))))

(evil-define-text-object evil-cp-inner-comment (count &optional beg end type)
  "An inner comment text object as defined by `sp-get-comment-bounds'."
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
  "An outer text object for a top level sexp (defun)."
  (if (evil-cp--inside-form-p)
      (let ((bounds (evil-cp--top-level-bounds)))
        (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))
    (error "Not inside a sexp.")))

(evil-define-text-object evil-cp-inner-defun (count &optional beg end type)
  "An inner text object for a top level sexp (defun)."
  (if (evil-cp--inside-form-p)
      (let ((bounds (evil-cp--top-level-bounds)))
        (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'inclusive :expanded t))
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
      (cond
       ((or (sp-point-in-comment)
            (sp-point-in-string))
        (let* ((ending (cdr (or (sp-get-comment-bounds)
                                (sp-get-quoted-string-bounds))))
               (diff (- (min end ending) (point))))
          (delete-char diff)
          (setq chars-left (- chars-left diff))))

       ((evil-cp--looking-at-opening-p)
        (let ((other-paren (evil-cp--matching-paren-pos)))
          (if (< (point) other-paren end)
              (let ((char-count (sp-get (sp-get-sexp) (- :end :beg))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (evil-cp--splice-form)
            (cl-decf chars-left))))

       ((evil-cp--looking-at-string-opening-p)
        (let ((other-quote (evil-cp--matching-paren-pos)))
          (if (< (point) other-quote end)
              (let ((char-count (sp-get (sp-get-string) (- :end :beg))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (forward-char)
            (cl-decf chars-left))))

       ((evil-cp--looking-at-paren-p)
        (evil-cp--splice-form)
        (cl-decf chars-left))

       ((evil-cp--looking-at-string-delimiter-p)
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
         (if evil-cleverparens-complete-parens-in-yanked-region
             (evil-cp-yank beg end type register yank-handler)
           (evil-yank beg end type register yank-handler))
         (evil-cp-delete-or-splice-region beg end))))

;; Originally from:
;; http://emacs.stackexchange.com/questions/777/closing-all-pending-parenthesis
(defun evil-cp--insert-missing-parentheses (backp)
  "Calling this function in a buffer with unbalanced parentheses
will have the missing parentheses be inserted at the end of the
buffer if BACKP is nil and at the beginning if it is true."
  (let ((closing nil))
    ;; fix for the degenerate case of nothing but closing parens
    (when backp (save-excursion (insert " ")))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (if (not backp)
                       (backward-up-list)
                     (forward-char)
                     (up-list)
                     (backward-char))
                   (let* ((syntax (syntax-after (point)))
                          (head   (car syntax)))
                     (cond
                      ((eq head (if backp 5 4))
                       (setq closing (cons (cdr syntax) closing)))
                      ((member head '(7 8))
                       (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (when backp (goto-char (point-min)))
    (apply #'insert (nreverse closing))
    ;; see above
    (when backp (delete-char 1))))

(defun evil-cp--close-missing-parens (text)
  "Takes a text object and inserts missing parentheses first at
the end of the text, and then, if the expression is still
unbalanced, will insert the rest of the missing parens at the
beginning."
  (with-temp-buffer
    (insert text)
    (goto-char (point-max))
    (evil-cp--insert-missing-parentheses nil)
    (when (not (sp-region-ok-p (point-min) (point-max)))
      (goto-char (point-min))
      (evil-cp--insert-missing-parentheses t))
    (buffer-string)))

(defun evil-cp--region-has-unbalanced-string-p (beg end)
  "Predicate for checking if a region contains an unbalanced string."
  (not (evenp (count-matches (sp--get-stringlike-regexp) beg end))))

(defun evil-cp--yank-characters
    (beg end &optional register yank-handler add-parens-p)
  "Yank characters while keeping parentheses balanced. The
optional ADD-PARENS-P arg determines how to handle the missing
parentheses: if nil, the non-balanced parens are
ignored. Otherwise they are added to the start/beginning of the
region."
  (unless (evil-cp--region-has-unbalanced-string-p beg end)
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

(defmacro evil-cp--point-after (&rest body)
  "Return location of point after performing body."
  `(save-excursion
     ,@body
     (point)))

(defun evil-cp--swap-regions (r1 r2)
  "Transposes the regions where R1 and R2 are cons-pairs where
the car is the start and the cdr is the end of each respective
region."
  (when (and r1 r2)
    (transpose-regions (cdr-safe r1)
                       (car-safe r1)
                       (cdr-safe r2)
                       (car-safe r2))))

(defmacro evil-cp--movement-bounds (movement)
  (let ((beg (gensym))
        (end (gensym)))
    `(let ((,beg (point))
           (,end (evil-cp--point-after ,movement)))
       (when (not (= ,beg ,end))
         (cons ,beg ,end)))))

(defmacro evil-cp--safe-line-bounds (&rest body)
  "Performs the actions in BODY and checks if the line where
point lands is a safe region, in which case its bounds are
returned."
  `(save-excursion
     ,@body
     (let ((pbol (point-at-bol))
           (peol (point-at-eol)))
       (when (and (sp-region-ok-p pbol peol))
         (cons pbol peol)))))

(defun evil-cp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG.

Copied from `evil-smartparens'."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-cp--point-after
                (sp-backward-up-sexp))
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
        (evil-cp--fail)))
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

(defun evil-cp--ignoring-yank (beg end type register yank-handler)
  "This is a version of yank ignores unbalanced parentheses to
keep the region safe.

Copied from `evil-smartparens'."
  (save-excursion
    (condition-case nil
        (let ((new-beg (evil-cp--new-beginning beg end))
              (new-end (evil-cp--new-ending beg end)))
          (if (and (= new-end end)
                   (= new-beg beg))
              (evil-yank beg end type register yank-handler)
            (evil-yank new-beg new-end type register yank-handler)))
      (error (let* ((beg (evil-cp--new-beginning beg end :shrink))
                    (end (evil-cp--new-ending beg end)))
               (evil-yank beg end type register yank-handler))))))

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

     ;; unbalanced block, add parens
     ((and (eq type 'block)
           evil-cleverparens-complete-parens-in-yanked-region)
      (evil-cp--yank-rectangle beg end register yank-handler))

     ;; unbalanced block, ignoring parens
     ((eq type 'block)
      (evil-cp--fail))

     ;; balanced region, or override
     ((or safep (evil-cp--override))
      (evil-yank beg end type register yank-handler))

     ;; unbalanced line, fill parens
     ((and (eq type 'line)
           evil-cleverparens-complete-parens-in-yanked-region)
      (evil-cp--yank-characters beg end register
                                'evil-yank-line-handler))

     ((eq type 'line)
      (evil-cp--ignoring-yank beg end type register
                              'evil-yank-line-handler))

     ;; unbalanced, fill missing
     (evil-cleverparens-complete-parens-in-yanked-region
      (evil-cp--yank-characters beg end register yank-handler))

     ;; unbalanced, ignore extra
     (t
      (evil-cp--ignoring-yank beg end type register yank-handler)))))

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
          (when end-of-list-p
            (up-list)
            (backward-char))
          (evil-yank-characters
           beginning
           (cond
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
             (point)))
           register)))))))

(defun evil-cp--delete-characters (beg end)
  "Deletes everything except unbalanced parentheses / string
delimiters in the region defined by BEG and END."
  (let ((chars-left (- end beg)))
    (goto-char beg)
    (while (> chars-left 0)
      (cond
       ((or (sp-point-in-comment)
            (sp-point-in-string))
        (let* ((ending (cdr (or (sp-get-comment-bounds)
                                (sp-get-quoted-string-bounds))))
               (diff (- (min end ending) (point))))
          (delete-char diff)
          (setq chars-left (- chars-left diff))))
       ((evil-cp--looking-at-any-opening-p)
        (let ((other-end (evil-cp--matching-paren-pos)))
          ;; matching paren is in the range of the command
          (if (<= (point) other-end end)
              (let ((char-count
                     (evil-cp--guard-point
                      (sp-get (sp-get-enclosing-sexp)
                        (- :end :beg)))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (forward-char)
            (cl-decf chars-left))))
       ((evil-cp--looking-at-any-closing-p)
        (forward-char 1)
        (cl-decf chars-left))
       (t
        (delete-char 1)
        (cl-decf chars-left))))))

(defun evil-cp-first-non-blank-non-opening ()
  "Like `evil-first-non-blank' but also skips opening parentheses
and string delimiters."
  (interactive)
  (evil-first-non-blank)
  (while (and (evil-cp--looking-at-any-opening-p)
             (<= (point) (point-at-eol)))
    (forward-char)))

(evil-define-operator evil-cp-delete (beg end type register yank-handler)
  "A version of `evil-delete' that attempts to leave the region
its acting on with balanced parentheses. The behavior of
kill-ring is determined by the
`evil-cleverparens-complete-parens-in-yanked-region' variable."
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
           (goto-char beg)
           (save-excursion
             (evil-cp--delete-characters
              (+ beg
                 (save-excursion  ; skip preceding whitespace
                   (beginning-of-line)
                   (sp-forward-whitespace t)))
              (1- end)))
           (evil-cp-first-non-blank-non-opening)
           (if (evil-cp--looking-at-any-closing-p)
               (progn
                 (forward-line -1)
                 (join-line 1)
                 (forward-line 1))
             (join-line 1)))

          (t (evil-cp--delete-characters beg end)))))

(evil-define-operator evil-cp-delete-line (beg end type register yank-handler)
  "Kills the balanced expressions on the line until the eol."
  :motion nil
  :keep-visual t
  :move-point nil
  (interactive "<R><x>")
  (cond ((evil-visual-state-p)
         ;; Not sure what this should do in visual-state
         (let ((safep (sp-region-ok-p beg end)))
           (if (not safep)
               (evil-cp--fail)
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
         (when (paredit-in-char-p) (backward-char))
         ;; `kill-line' inlined from from `simple.el'
         ;; this works but is not very evilidiomatic
         (kill-region
          (point)
          (progn
            (when (eobp) (evil-signal-at-eob))
            (let ((end (save-excursion (end-of-visible-line) (point))))
              (if (or (save-excursion
                        ;; If trailing whitespace is visible,
                        ;; don't treat it as nothing.
                        (unless show-trailing-whitespace
                          (skip-chars-forward " \t" end))
                        (= (point) end))
                      (and kill-whole-line (bolp)))
                  (forward-visible-line 1)
                (goto-char end)))
            (point))))

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

(evil-define-operator evil-cp-change (beg end type register yank-handler delete-func)
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
                (+ beg
                   (save-excursion
                     (beginning-of-line)
                     (sp-forward-whitespace t)))
                (1- end)))
             (evil-cp-first-non-blank-non-opening)
             (indent-according-to-mode)
             (evil-insert 1))

            ((eq type 'block)
             (evil-cp-delete beg end type register yank-handler)
             (evil-insert 1 nlines))

            (t
             (funcall delete-func beg end type register yank-handler)
             (evil-insert 1))))))

(evil-define-operator evil-cp-change-line (beg end type register yank-handler)
  "Change to end of line while respecting parentheses."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-cp-change beg end type register yank-handler #'evil-cp-delete-line))

(evil-define-operator evil-cp-change-whole-line (beg end type register yank-handler)
  "Change whole line while respecting parentheses."
  :motion evil-line
  (interactive "<R><x>")
  (if (sp-region-ok-p beg end)
      (evil-change beg end type register yank-handler)
    (evil-cp-first-non-blank-non-opening)
    (evil-cp-change beg end type register yank-handler #'evil-cp-delete-line)))


;;; Movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-motion evil-cp-forward-sexp (count)
  "Motion for moving forward by a sexp via `sp-forward-sexp'."
  :type exclusive
  (let ((count (or count 1)))
    (sp-forward-sexp count)))

(evil-define-motion evil-cp-backward-sexp (count)
  "Motion for moving backwward by a sexp via `sp-backward-sexp'."
  :type exclusive
  (let ((count (or count 1)))
    (sp-backward-sexp count)))

(evil-define-motion evil-cp-beginning-of-defun (count)
  "Motion for moving to the beginning of a defun (i.e. a top
level sexp)."
  :type inclusive
  (let ((count (or count 1)))
    (beginning-of-defun count)))

(evil-define-motion evil-cp-end-of-defun (count)
  "Motion for moving to the end of a defun (i.e. a top level sexp)."
  :type inclusive
  (let ((count (or count 1)))
    (if (evil-cp--looking-at-closing-p)
        (forward-char 2))
    (end-of-defun count)
    (backward-char (if (eobp) 1 2))))

;; TODO: this looks ugly
(defun evil-cp--paren-navigation-helper (move-dir paren-side)
  (let ((move-fn (case move-dir
                   (:next 'forward-char)
                   (:previous 'backward-char)))
        (the-end (case move-dir
                   (:next 'point-max)
                   (:previous 'point-min)))
        (paren-p (case paren-side
                   (:opening 'evil-cp--looking-at-any-opening-p)
                   (:closing 'evil-cp--looking-at-any-closing-p)))
        (pt-orig (point))
        done-p)
    (when (funcall paren-p) (funcall move-fn))
    (while (not done-p)
      (cond
       ((= (point) (funcall the-end))
        (setq done-p t)
        (goto-char pt-orig))
       ((funcall paren-p)
        (setq done-p t))
       (t
        (funcall move-fn))))))

(evil-define-motion evil-cp-next-opening (count)
  "Motion for moving to the next open parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (i count)
      (evil-cp--paren-navigation-helper :next :opening))))

(evil-define-motion evil-cp-previous-opening (count)
  "Motion for moving to the previous open parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (i count)
      (evil-cp--paren-navigation-helper :previous :opening))))

(evil-define-motion evil-cp-next-closing (count)
  "Motion for moving to the next closing parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (i count)
      (evil-cp--paren-navigation-helper :next :closing))))

(evil-define-motion evil-cp-previous-closing (count)
  "Motion for moving to the previous closing parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (i count)
      (evil-cp--paren-navigation-helper :previous :closing))))

(evil-define-motion evil-cp-backward-up-sexp (count)
  "Motion for moving backward up to the previous level of
parentheses. Basically just wraps `sp-backward-up-sexp' as an
evil-motion."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    ;; for some reason calling `sp-backward-up-sexp' with a large `count'
    ;; doesn't move the point at all
    (dotimes (i count)
      (sp-backward-up-sexp))))

(evil-define-motion evil-cp-up-sexp (count)
  "Motion for moving up to the previous level of parenteheses.
The same as `sp-up-sexp', but leaves the point on top of the
closing paren."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (when (evil-cp--looking-at-closing-p) (forward-char))
    (dotimes (i count)
      (sp-up-sexp))
    (backward-char)))


(defun forward-evil-cp-symbol (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((pnt (point)))
         (forward-symbol cnt)
         (if (= pnt (point)) cnt 0)))
   #'forward-evil-empty-line))

;; incomplete
(defun forward-evil-cp-word (&optional count)
  "TODO: This is exactly the same as `forward-evil-word' and not used
for anything right now. It would be nice to skip delimiters on
the small-word movements as well but I couldn't figure out how to
change this to make it work. Pull requests welcome."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories evil-cjk-word-separating-categories)
             (word-combining-categories evil-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (evil-forward-chars "^[:word:]\n\r\t\f " cnt))
   #'forward-evil-empty-line))

(defun evil-cp--forward-X-begin (thing count)
  "TODO: see `forward-evil-cp-word' which is currently not
working. Could be used to implement a future
`evil-cp-forward-word-begin' the same way that
`evil-cp-forward-symbol-begin' is defined."
  (let ((orig (point)))
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

(evil-define-motion evil-cp-forward-symbol-begin (count)
  :type exclusive
  (evil-cp--forward-X-begin (if evil-cleverparens-move-skip-delimiters
                                'evil-cp-symbol
                              'evil-symbol)
                            (or count 1)))

(defun evil-cp--forward-X-end (thing count)
  (evil-signal-at-bob-or-eob count)
  (unless (and (evil-operator-state-p)
               (= 1 count)
               (let ((bnd (bounds-of-thing-at-point thing)))
                 (and bnd
                      (= (car bnd) (point))
                      (= (cdr bnd) (1+ (point)))))
               (looking-at "[[:word:]]"))
    (evil-forward-end thing count)))

(evil-define-motion evil-cp-forward-symbol-end (count)
  "Copy of `evil-forward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (evil-cp--forward-X-end (if evil-cleverparens-move-skip-delimiters
                              'evil-cp-symbol
                            'evil-symbol)
                          (or count 1)))

(evil-define-motion evil-cp-backward-symbol-begin (count)
  "Copy of `evil-backward-word-begin' using 'evil-symbol for the
movement."
  :type exclusive
  (let ((thing (if evil-cleverparens-move-skip-delimiters
                   'evil-cp-symbol
                 'evil-symbol)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning thing count)))

(evil-define-motion evil-cp-backward-symbol-begin (count)
  "Copy of `evil-backward-word-begin' using 'evil-symbol for the
movement."
  :type exclusive
  (let ((thing (if evil-cleverparens-move-skip-delimiters
                   'evil-cp-symbol
                 'evil-symbol)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning thing count)))

(evil-define-motion evil-cp-backward-symbol-end (count)
  "Copy of `evil-backward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (let ((thing (if evil-cleverparens-move-skip-delimiters
                   'evil-cp-symbol
                 'evil-symbol)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-end thing count)))

;;; Additional Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp-< (n)
  "Slurping/barfing operation that acts differently based on the points
location in the form.

When point is on the opening delimiter of the form boundary, it will
slurp the next element backwards while maintaining the location of the
point in the original delimiter.

  foo [(]bar baz) -> [(]foo bar baz)

When point is on the closing delimiter, it will barf the rightmost
element of the form forward, again maintaining the location of the point
in the original delimiter.

  (bar baz foo[)] -> (bar baz[)] foo

When point is in the middle of a form, it will act as a regular
forward-barf."
  (interactive "p")
  (cond

   ((not (evil-cp--inside-any-form-p))
    nil)

   ((evil-cp--looking-at-any-opening-p)
    (condition-case nil
        (dotimes (_ n)
          (evil-cp--guard-point (sp-backward-slurp-sexp))
          (sp-backward-sexp)
          (evil-cp-previous-opening))
      (error nil)))

   ((and (evil-cp--looking-at-closing-p)
         (sp-point-in-empty-sexp))
    nil)

   ((evil-cp--looking-at-any-closing-p)
    (condition-case nil
        (dotimes (_ n)
          (sp-forward-barf-sexp)
          (sp-backward-sexp)
          (evil-cp-previous-closing))
      (error nil)))

   (t (sp-forward-barf-sexp n))))

(defun evil-cp-> (n)
  "Slurping/barfing operation that acts differently based on the points
location in the form.

When point is on the opening delimiter of the form boundary, it will
bafr the first element of the sexp out while maintaining the location of the
point in the delimiter where the command was called from.

  [(]foo bar baz) -> foo [(]bar baz)

When point is on the closing delimiter, it will slurp the next element
forward while maintaining the location of point in the original delimiter.

  (bar baz[)] foo -> (bar baz foo[)]

When point is in the middle of a form, it will act as a regular
forward-slurp.

If you are having problems with this function, make sure that the
parentheses in your buffer are balanced overall."
  (interactive "p")
  (cond
   ((not (evil-cp--inside-any-form-p))
    nil)

   ((and (evil-cp--looking-at-any-opening-p)
         (evil-cp--looking-at-empty-form))
    nil)

   ((evil-cp--looking-at-any-opening-p)
    (condition-case nil
        (dotimes (_ n)
          (evil-cp--guard-point (sp-backward-barf-sexp))
          (sp-forward-sexp)
          ;; in case we end up with empty sexp
          (when (not (evil-cp--looking-at-empty-form))
            (evil-cp-next-opening)))
      (error nil)))

   ((evil-cp--looking-at-any-closing-p)
    (condition-case nil
        (dotimes (_ n)
          (sp-forward-slurp-sexp)
          (evil-cp-up-sexp))
      (error nil)))

   (t
    (dotimes (_ n) (sp-forward-slurp-sexp)))))


(defun evil-cp--symbol-bounds ()
  (save-excursion
    (if (looking-at-p "\\_<")
        (evil-cp--sp-obj-bounds (sp-get-symbol))
      (when (sp-point-in-symbol)
        (evil-cp--sp-obj-bounds (sp-get-symbol))))))

(defun evil-cp--cant-forward-sexp-p ()
  (save-excursion
    (condition-case nil
        (forward-sexp)
      (error t))))

(defun evil-cp--cant-backward-sexp-p ()
  (save-excursion
    (condition-case nil
        (backward-sexp)
      (error t))))

(defun evil-cp--last-symbol-of-form-p ()
  (save-excursion
    (-when-let (end (cdr (evil-cp--symbol-bounds)))
      (goto-char end)
      (evil-cp--cant-forward-sexp-p))))

(defun evil-cp--first-symbol-of-form-p ()
  (save-excursion
    (-when-let (beg (car (evil-cp--symbol-bounds)))
      (goto-char beg)
      (evil-cp--cant-backward-sexp-p))))

(defun evil-cp--first-form-of-form-p ()
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (evil-cp--guard-point
       (evil-cp-backward-up-sexp)
       (evil-cp--cant-backward-sexp-p)))))

(defun evil-cp--last-form-of-form-p ()
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (evil-cp--guard-point
       (evil-cp--up-list)
       (evil-cp--cant-forward-sexp-p)))))

(defun evil-cp--next-thing-bounds* (by-line-p)
  "Fetches the bounds for the next \"thing\" from the location of
  point. The thing may be a symbol, a form, a line or a comment
  block. When BY-LINE-P is true, defaults to swapping by line and
  only swaps by form if the next line is not safe. May move the
  point. Acts as a helper for `evil-cp--next-thing-bounds'."
  (let* ((next-form
          (save-excursion
            (evil-cp--skip-whitespace-and-comments)
            (evil-cp--movement-bounds (forward-sexp))))
         (next-line
          (or (when (not (= (point-at-eol) (point-max)))
                (save-excursion
                  (forward-line)
                  (evil-cp--comment-block-bounds)))
              (evil-cp--safe-line-bounds (forward-line)))))
    (cond
     (by-line-p (or next-line next-form))
     (evil-cleverparens-drag-ignore-lines next-form)
     ((and next-line next-form
           (<= (cdr next-form) (cdr next-line)))
      next-form)
     (t (or next-line next-form)))))

(defun evil-cp--previous-thing-bounds* (by-line-p)
  "Fetches the bounds for the previous \"thing\" from the
  location of point. The thing may be a symbol, a form, a line or
  a comment block. When BY-LINE-P is true, defaults to swapping
  by line and only swaps by form if the next line is not safe.
  May move the point. Acts as a helper for
  `evil-cp--previous-thing-bounds'."
  (let ((prev-form
         (save-excursion
           (backward-sexp)
           (when (not (bobp))
             (evil-cp--next-sexp-bounds))))
        (prev-line
         (when (not (= (point-at-bol) (point-min)))
           (or (save-excursion
                 (forward-line -1)
                 (evil-cp--comment-block-bounds))
               (evil-cp--safe-line-bounds (forward-line -1))))))
    (cond
     (by-line-p (or prev-line prev-form))
     (evil-cleverparens-drag-ignore-lines prev-form)
     ((and prev-line prev-form
           (>= (cdr prev-form) (cdr prev-line)))
      prev-form)
     (t (or prev-line prev-form)))))

(defmacro evil-cp--next-thing-bounds (&optional movement by-line-p)
  "Fetches the bounds for the next hing which may be a symbol, a
form, a line or a comment block."
  `(save-excursion
     ,movement
     (evil-cp--next-thing-bounds* ,by-line-p)))

(defmacro evil-cp--previous-thing-bounds (&optional movement by-line-p)
  "Fetches the bounds for the previous thing which may be a
symbol, a form, a line or a comment block."
  `(save-excursion
     ,movement
     (evil-cp--previous-thing-bounds* ,by-line-p)))

(defun evil-cp--swap-with-next (this-bounds &optional by-line-p)
  "Swaps the region defined by THIS-BOUNDS with the next thing
from the end of THIS-BOUNDS."
  (when this-bounds
    (let ((that-bounds (evil-cp--next-thing-bounds
                        (goto-char (cdr this-bounds))
                        by-line-p)))
      (if (not (= (point) (point-at-eol)))
          (evil-cp--swap-regions this-bounds that-bounds)
        (progn
          (backward-char)
          (evil-cp--swap-regions this-bounds that-bounds)
          (forward-char)))
      (when evil-cleverparens-indent-afterwards
        (save-excursion
          (goto-char (car this-bounds))
          (evil-cp--backward-up-list t)
          (indent-sexp))))))

(defun evil-cp--swap-with-previous (this-bounds &optional by-line-p)
  "Swaps the region defined by THIS-BOUNDS with the previous
thing from the beginning of of THIS-BOUNDS."
  (when this-bounds
    (let ((that-bounds (evil-cp--previous-thing-bounds
                        (goto-char (car this-bounds))
                        by-line-p)))
      (when that-bounds
        (if (not (= (point) (point-at-eol)))
            (evil-cp--swap-regions this-bounds that-bounds)
          (backward-char)
          (evil-cp--swap-regions this-bounds that-bounds)
          (forward-char))
        (when evil-cleverparens-indent-afterwards
          (save-excursion
            (goto-char (car that-bounds))
            (evil-cp--backward-up-list t)
            (indent-sexp)))))))

(defun evil-cp--comment-block? ()
  "Checks whether point is on a line that starts with whitespace
or a comment."
  (save-excursion
    (evil-first-non-blank)
    (looking-at-p sp-comment-char)))

(defun evil-cp--comment-block-bounds (&optional pos)
  "Gets the bounds for a comment block (i.e. a group of lines
that start with whitespace or a comment)."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--comment-block?)
      (let (beg end)
        (save-excursion
          (beginning-of-line)
          (setq beg (point))
          (while (and (not (bobp)) (evil-cp--comment-block?))
            (forward-line -1)
            (when (evil-cp--comment-block?) (setq beg (point)))))
        (save-excursion
          (end-of-line)
          (setq end (point))
          (while (and (not (eobp)) (evil-cp--comment-block?))
            (forward-line 1)
            (end-of-line)
            (when (evil-cp--comment-block?) (setq end (point)))))
        (cons beg end)))))

(evil-define-command evil-cp-drag-forward (count)
  ;; TODO: write better doc-strings
  "Drags the thing under point forward. The thing can be either a
symbol, a form, a line or a comment block. Does not mess with the
depth of expressions by slurping or barfing. If the thing under
point is at the end of a form then tries to drag its \"parent\"
thing forward instead. Maintains the location of point relative
to the thing being dragged."
  (interactive "<c>")
  ;; TODO: do something with universal-argument?
  (if (evil-visual-state-p)
      (let* ((v-range (evil-visual-range))
             (linep   (eq 'line (evil-visual-type)))
             (beg     (car v-range))
             (end     (if linep (1- (cadr v-range)) (cadr v-range))))
        ;; TODO: restore visual-state
        (when (sp-region-ok-p beg end)
          (evil-cp--swap-with-next (cons beg end) linep)))
    (let (drag-by-line-p)
      (evil-cp--swap-with-next
       (let ((sym-bounds (evil-cp--symbol-bounds)))
         (if (and sym-bounds
                  (not (sp-point-in-string-or-comment))
                  (not (evil-cp--last-symbol-of-form-p)))
             sym-bounds
           (if (evil-cp--inside-any-form-p)
               (save-excursion
                 (while (evil-cp--last-form-of-form-p)
                   (when (evil-cp--looking-at-any-opening-p)
                     (forward-char))
                   (evil-cp--up-list))
                 (evil-cp--get-enclosing-bounds))
             (setq drag-by-line-p t)
             (if (evil-cp--comment-block?)
                 (evil-cp--comment-block-bounds)
               (evil-cp--safe-line-bounds)))))
       drag-by-line-p))))

(evil-define-command evil-cp-drag-backward (count)
  "Drags the thing under point backward. The thing can be either
a symbol, a form, a line or a comment block. Does not mess with
the depth of expressions by slurping or barfing. If the thing
under point is at the beginning of a form then tries to drag its
\"parent\" thing backward instead. Maintains the location of
point relative to the thing being dragged."
  (interactive "<c>")
  (if (evil-visual-state-p)
      (let* ((v-range (evil-visual-range))
             (linep   (eq 'line (evil-visual-type)))
             (beg     (car v-range))
             (end     (if linep (1- (cadr v-range)) (cadr v-range))))
        ;; TODO: restore visual-state
        (when (sp-region-ok-p beg end)
          (evil-cp--swap-with-previous (cons beg end) linep)))
    (let (drag-by-line-p)
      (evil-cp--swap-with-previous
       (let ((sym-bounds (evil-cp--symbol-bounds)))
         (if (and sym-bounds
                  (not (sp-point-in-string-or-comment))
                  (not (evil-cp--first-symbol-of-form-p)))
             sym-bounds
           (if (evil-cp--inside-any-form-p)
               (save-excursion
                 (when (not (evil-cp--looking-at-any-opening-p))
                   (evil-cp--backward-up-list))
                 (while (evil-cp--first-form-of-form-p)
                   (evil-cp--backward-up-list))
                 (evil-cp--next-sexp-bounds))
             (setq drag-by-line-p t)
             (if (evil-cp--comment-block?)
                 (evil-cp--comment-block-bounds)
               (evil-cp--safe-line-bounds)))))
       drag-by-line-p))))


(evil-define-operator evil-cp-substitute (beg end type register)
  "Parentheses safe version of `evil-substitute'."
  :motion evil-forward-char
  (interactive "<R><x>")
  (cond
   ((evil-cp--looking-at-any-opening-p)
    (evil-append 1))
   ((evil-cp--looking-at-any-closing-p)
    (evil-insert 1))
   (t (evil-substitute beg end type register))))


(evil-define-command evil-cp-insert-at-end-of-form (count)
  "Move point COUNT times with `sp-forward-sexp' and enter insert
mode at the end of form. Using an arbitrarily large COUNT is
guaranteened to take the point to the beginning of the top level
form."
  (interactive "<c>")
  (let ((defun-end (save-excursion (end-of-defun) (point)))
        target)
    (when (not (sp-up-sexp count))
      (goto-char defun-end))
    (backward-char)
    (evil-insert 1)))

(evil-define-command evil-cp-insert-at-beginning-of-form (count)
  "Move point COUNT times with `sp-backward-up-sexp' and enter
insert mode at the beginning of the form. Using an arbitrarily
large COUNT is guaranteened to take the point to the beginning
of the top level form."
  (interactive "<c>")
  ;; TODO: fails if the sexp contains a string with an open paren
  (let ((defun-beginning
          (save-excursion
            (beginning-of-defun)
            (point)))
        target)
    (when (not (sp-backward-up-sexp count))
      (goto-char defun-beginning))
    (forward-char)
    (evil-insert 1)))


(defun evil-cp--defun-bounds ()
  (save-excursion
    (when (not (and (evil-cp--looking-at-any-opening-p)
                    (evil-cp--top-level-form-p)))
      (beginning-of-defun))
    (evil-cp--sp-obj-bounds (sp-get-sexp))))

(defun evil-cp-copy-paste-form (&optional arg)
  "Copies the surrounding form and inserts it below itself. If
called with a single prefix-argument, will copy the top-level
sexp regardless of what level the point is currently at."
  (interactive "P")
  (let* ((prefixp (equal arg '(4)))
         (bounds
          (if prefixp
              (evil-cp--defun-bounds)
            (evil-cp--get-enclosing-bounds)))
         (beg (car bounds))
         (end (cdr bounds))
         (offset (1+ (- end (point))))
         (text (buffer-substring-no-properties beg end)))
    (when (and beg end)
      (if (or prefixp (evil-cp--top-level-form-p))
          (progn
            (end-of-defun)
            (insert "\n" text "\n"))
        (when (evil-cp--looking-at-any-opening-p) (forward-char))
        (sp-up-sexp)
        (insert "\n")
        (indent-according-to-mode)
        (insert text))
      (backward-char offset))))


(evil-define-command evil-cp-open-below-form (count)
  "Move COUNT levels up from the current form and enter
insert-state. If the last form is a top-level sexp then two
newlines are inserted instead. If point is not inside a form,
inserts two newlines and enters insert-state between them.

Note thath the COUNT parameter doesn't affect the amount of
times that the inserted text gets output into the buffer, unlike
in `evil-open-below'."
  (interactive "<c>")
  (setq count (or count 1))
  (if (not (evil-cp--inside-any-form-p))
      (progn
        (insert "\n\n")
        (forward-line -1)
        (evil-insert-state))
    (sp-up-sexp count)
    (if (save-excursion
          (backward-char)
          (evil-cp--top-level-form-p))
        (insert "\n\n")
      (insert "\n"))
    (indent-according-to-mode)
    (evil-insert-state)))

(evil-define-command evil-cp-open-above-form (count)
  "Move COUNT levels backwards up from the current form and
enter insert-state. If the form is a top-level sexp then two
newlines are inserted instead.

Note thath the COUNT parameter doesn't affect the amount of
times that the inserted text gets output into the buffer, unlike
in `evil-open-below'."
  (interactive "<c>")
  (setq count (or count 1))
  (sp-backward-up-sexp count)
  (save-excursion
    (if (evil-cp--top-level-form-p)
        (insert "\n\n")
      (insert "\n"))
    (indent-according-to-mode)
    (evil-insert 1)))

(defun evil-cp--kill-sexp-range (count)
  (save-excursion
    (when (not (evil-cp--inside-any-form-p))
      (sp-forward-whitespace))
    (let* ((beg       (point))
           (end       (point))
           (enc-range (evil-cp--get-enclosing-bounds))
           (e-beg     (or (car enc-range) (point)))
           (e-end     (or (cdr enc-range) (point)))
           (n         (or count 1))
           (ok        t))
      (while (and (> n 0) ok)
        (setq ok (sp-forward-sexp 1))
        (when ok
          (sp-get ok
            (when (> :end end) (setq end :end))
            (when (and (>= :end e-end) (> beg :beg))
              (setq beg :beg))))
        (setq n (1- n)))
      (cons beg end))))


(defun evil-cp--del-characters (beg end &optional register yank-handler)
  (evil-yank-characters beg end register yank-handler)
  (delete-region beg end))

(evil-define-command evil-cp-yank-sexp (count &optional register)
  "Copies COUNT many sexps from point to the kill-ring.
Essentially a less greedy version of `evil-cp-yank-line'. When
called with \\[universal-argument], copies everything from point
to the end of the the current form."
  (interactive "<c><x>")
  (if (and (equal current-prefix-arg '(4))
           (evil-cp--inside-any-form-p))
      (sp-get (sp-get-enclosing-sexp)
        (evil-yank-characters (point) (1- :end)))
    (let* ((range (evil-cp--kill-sexp-range count))
           (beg (car range))
           (end (cdr range)))
      (evil-yank-characters (car range) (cdr range) register))))

(evil-define-command evil-cp-delete-sexp (count &optional register)
  "Kills COUNT many sexps from point. Essentially a less greedy
version of `evil-cp-delete-line'. When called with
\\[universal-argument], deletes everything from point to the end
of the the current form."
  (interactive "<c><x>")
  (if (and (equal current-prefix-arg '(4))
           (evil-cp--inside-any-form-p))
      (sp-get (sp-get-enclosing-sexp)
        (evil-cp--del-characters (point) (1- :end)))
    (let* ((range (evil-cp--kill-sexp-range count))
           (beg (car range))
           (end (cdr range)))
      (evil-cp--del-characters beg end register))))

(evil-define-command evil-cp-change-sexp (count &optional register)
  "Like `evil-cp-delete-sexp' but enters insert mode after the
operation. When called with \\[universal-argument], deletes
everything from point to the end of the the current form before
entering insert-state."
  (interactive "<c><x>")
  (evil-cp-delete-sexp count register)
  (evil-insert-state))

(defun evil-cp-top-level-yank-handler (text)
  (insert (concat "\n" (s-trim text) "\n"))
  (backward-char 1))

(evil-define-command evil-cp-yank-enclosing (count &optional register)
  "Copies the enclosing form to kill-ring. With COUNT, yanks the
nth form upwards instead. When called with a raw prefix argument,
yanks the top-level form and deletes the leftover whitespace,
while adding a yank-handler that inserts two newlines at the end
and beginning of the copied top-level form."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (if (equal current-prefix-arg '(4))
          (progn
            (beginning-of-defun)
            (sp-get (sp-get-sexp)
              (evil-yank-characters
               :beg
               :end
               register
               #'evil-cp-top-level-yank-handler)))
        (evil-cp-backward-up-sexp count)
        (evil-cp--guard-point
         (sp-get (sp-get-enclosing-sexp)
           (evil-yank-characters :beg :end register)))))))

(evil-define-command evil-cp-delete-enclosing (count &optional register)
  "Kills the enclosing form. With COUNT, kills the nth form
upwards instead. When called with a raw prefix argument, kills
the top-level form and deletes the extra whitespace."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (if (equal current-prefix-arg '(4))
        (progn
          (beginning-of-defun)
          (sp-get (sp-get-sexp)
            (evil-cp--del-characters
             :beg
             :end
             register
             #'evil-cp-top-level-yank-handler))
          (sp-backward-whitespace)
          (let ((del-me (save-excursion (sp-forward-whitespace t))))
            (delete-char (- del-me 2))
            (forward-char)))
      (evil-cp-backward-up-sexp count)
      (evil-cp--guard-point
       (sp-get (sp-get-enclosing-sexp)
         (evil-cp--del-characters :beg :end register))))))

(evil-define-command evil-cp-change-enclosing (count &optional register)
  "Calls `evil-cp-delete-enclosing' and enters insert-state."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (evil-cp-delete-enclosing count register)
    (when (equal current-prefix-arg '(4))
      (insert "\n\n")
      (backward-char 1))
    (evil-insert-state)))


(defun evil-cp-raise-form (&optional count)
  "Raises the form under point COUNT many times."
  (interactive "P")
  (let ((count (or count 1)))
    (dotimes (_ count)
      (when (evil-cp--inside-any-form-p)
        (save-excursion
          (evil-cp--guard-point
           (sp-beginning-of-sexp))
          (backward-char)
          (sp-raise-sexp))))))

(defun evil-cp--wrap-region-with-pair (pair start end)
  "Helper that inserts a pair as indicated by PAIR at positions
  START and END. Performs no safety checks on the regions."
  (-when-let (this-pair (evil-cp-pair-for pair))
    (let ((open (car this-pair))
          (close (cdr this-pair)))
      (save-excursion
        (goto-char start)
        (insert open)
        (goto-char end)
        (insert close)))))

(defun evil-cp--wrap-next (pair count)
  (let ((pt-orig (point))
        (this-pair (evil-cp-pair-for pair)))
    (when (sp-point-in-symbol)
      (sp-get (sp-get-symbol)
        (goto-char :beg)))
    (let ((start   (point))
          (open    (car this-pair))
          (close   (cdr this-pair))
          (enc-end (sp-get (sp-get-enclosing-sexp) :end)))
      (sp-forward-sexp count)
      (setq end (if enc-end (min (point) enc-end) (point)))
      (when (not (= end pt-orig))
        (goto-char start)
        (insert open)
        (goto-char (1+ end))
        (insert close)
        (goto-char start)
        (indent-region start end)
        (forward-char (1+ (- pt-orig start)))))))

(defun evil-cp--wrap-previous (pair count)
  (let ((pt-orig (point))
        (this-pair (evil-cp-pair-for pair)))
    (when (and (sp-point-in-symbol) (not (eobp))) ; bug in `sp-point-in-symbol'?
      (sp-get (sp-get-symbol)
        (goto-char :end)))
    (let ((start   (point))
          (open    (car this-pair))
          (close   (cdr this-pair))
          (enc-beg (sp-get (sp-get-enclosing-sexp) :beg)))
      (sp-backward-sexp count)
      (setq beg (if enc-beg (max (point) enc-beg) (point)))
      (when (not (= pt-orig beg))
        (goto-char beg)
        (insert open)
        (goto-char (1+ start))
        (insert close)
        (goto-char start)
        (indent-region beg start)
        (backward-char (- (point) pt-orig 1))))))

(defun evil-cp-prefix-arg-count ()
  "Gets the count for how many times the prefix argument was
invoked, i.e. for \\[universal-argument] \\[universal-argument]
it would return 2."
  (when (consp current-prefix-arg)
    (truncate (log (car current-prefix-arg) 4))))

(evil-define-command evil-cp-wrap-next-round (count)
  "Wraps the next COUNT sexps inside parentheses. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "(" count)))
    (evil-cp--wrap-next "(" count)))

(evil-define-command evil-cp-wrap-previous-round (count)
  "Wraps the previous COUNT sexps inside parentheses. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "(" count)))
    (evil-cp--wrap-previous "(" count)))

(evil-define-command evil-cp-wrap-next-square (count)
  "Wraps the next COUNT sexps inside square braces. If the point
is inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "[" count)))
    (evil-cp--wrap-next "[" count)))

(evil-define-command evil-cp-wrap-previous-square (count)
  "Wraps the previous COUNT sexps inside square braces. If the
point is inside a symbol, that symbol is treated as the first
sexp to wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "[" count)))
    (evil-cp--wrap-previous "[" count)))

(evil-define-command evil-cp-wrap-next-curly (count)
  "Wraps the next COUNT sexps inside curly braces. If the point
is inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "{" count)))
    (evil-cp--wrap-next "{" count)))

(evil-define-command evil-cp-wrap-previous-curly (count)
  "Wraps the previous COUNT sexps inside curly braces. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for
how many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-prefix-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "{" count)))
    (evil-cp--wrap-previous "{" count)))


(defun evil-cp-insert (count &optional vcount skip-empty-lines)
  "Like `evil-insert', but tries to be helpful and not annoying
by automatically inserting a space in situations where the need
for it is highly likely, and cleaning after itself in the case
where the space wasn't actually needed.

Currently this hapens in two situations:

- When point is at an open parentheses (but not in an empty
  list), a space gets inserted but the point still remains where
  it used to.

- When the point is between two closing delimiters, a space is
  inserted in front of the point.

Can be disabled by setting `evil-cleverparens-use-regular-insert'
to true."
  (interactive "p")
  (cond
   ((or (evil-visual-state-p)
        (looking-at-p "[\n\t ]+")
        (bobp)
        (eobp)
        (sp-point-in-string-or-comment)
        (evil-cp--looking-at-empty-form))
    (call-interactively 'evil-insert))
   ((and (looking-back "(")
         (not (looking-back "'("))
         (not (looking-back "#(")))
    (setq evil-cp--inserted-space-after-round-open t)
    (insert " ")
    (backward-char)
    (evil-insert count))
   ((and (evil-cp--looking-at-any-closing-p)
         (evil-cp--looking-at-any-closing-p (1- (point))))
    (setq evil-cp--inserted-space-between-closings t)
    (insert " ")
    (evil-insert count))
   (t
    (call-interactively 'evil-insert))))

(defun evil-cp-insert-exit-hook ()
  "Deletes the extra space left by `evil-cp-insert' if nothing was inserted."
  (cond
   ((and evil-cp--inserted-space-after-round-open
         (looking-at-p "[[:space:]]"))
    (when (or (not evil-current-insertion)
              (= (car evil-current-insertion)
                 (cdr evil-current-insertion)))
      (delete-char 1)))
   ((and evil-cp--inserted-space-between-closings
         (looking-back "[[:space:]]"))
    (when (or (not evil-current-insertion)
              (= (car evil-current-insertion)
                 (cdr evil-current-insertion)))
      (delete-char -1))))
  (setq evil-cp--inserted-space-after-round-open nil)
  (setq evil-cp--inserted-space-between-closings nil))


(defun evil-cp-toggle-balanced-yank (&optional forcep)
  "Toggles the setting `evil-cleverparens-complete-parens-in-yanked-region',
which determines whether or not an incomplete yanked region
should be supplemented with the missing parentheses at the end
and/or beginning."
  (interactive)
  (cond
   (forcep
    (message "Turned yank auto-balancing ON.")
    (setq evil-cleverparens-complete-parens-in-yanked-region t))
   ((not evil-cleverparens-complete-parens-in-yanked-region)
    (message "Turned yank auto-balancing ON.")
    (setq evil-cleverparens-complete-parens-in-yanked-region t))
   (t
    (message "Turned yank auto-balancing OFF.")
    (setq evil-cleverparens-complete-parens-in-yanked-region nil))))


;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup evil-cleverparens nil
  "`evil-mode' for handling your parentheses with a mix of `smartparens' and `paredit'"
  :group 'smartparens)

(defcustom evil-cleverparens-threshold 1500
  "If the region being operated on is larger than this we cop out.

   Quite a bit of work gets done to ensure the region being worked
   is in an safe state, so this lets us sarifice safety for a snappy
   editor on slower computers.

   Even on a large computer you shouldn't set this too high or your
   computer will freeze when copying large files out of Emacs.

   This is a feature copied from `evil-smartparens'."
  :group 'evil-cleverparens
  :type 'number)

(defcustom evil-cleverparens-complete-parens-in-yanked-region nil
  "Determines how to handle yanking a region containing
  unbalanced expressions. If this value is non-nil, a yanked
  region containing missing parentheses will include the missing
  parens appended to the end."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-move-skip-delimiters t
  "Determines whether parentheses and other delimiters are
  considered symbols or not. The effect this has is that when
  enabled (default), the by-symbol navigation commands happily
  travel through these delimiters. This can be handy if you are
  already used to using evil-cleverparenses parentheses navigation
  commands."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-swap-move-by-word-and-symbol nil
  "If true, the keys w, e, b, and ge will be bound to the
  evil-cleverparens movement by symbol commands, and the regular
  evil move by word commands will be bound to W, E, B and gE respectively."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-drag-ignore-lines nil
  ""
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-indent-afterwards t
  "Controls whether to automatically indent when performing
  commands that alter the structure of the surrounding code.
  Enabled by default."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-use-regular-insert nil
  "Determines whether to use `evil-insert' or `evil-cp-insert'."
  :group 'evil-cleverparens
  :type 'boolean)

(defvar evil-cp--override nil
  "Should the next command skip region checks?")

(defvar evil-cp--inserted-space-after-round-open nil
  "Helper var for `evil-cp-insert'.")

(defvar evil-cp--inserted-space-between-closings nil
  "Helper var for `evil-cp-insert'.")

(defcustom evil-cleverparens-use-additional-bindings t
  "Should additional bindings be enabled."
  :type 'boolean
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-use-additional-movement-keys t
  "Should additional movement keys, mostly those related to
  parentheses navigation, be enabled."
  :type 'boolean
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-enabled-hook nil
  "Called after `evil-cleverparens-mode' is turned on."
  :type 'hook
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-disabled-hook nil
  "Called after `evil-cleverparens-mode' is turned off."
  :type 'hook
  :group 'evil-cleverparens)

;;; Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-cp-regular-movement-keys
  '(("w"  . evil-forward-word-begin)
    ("e"  . evil-forward-word-end)
    ("b"  . evil-backward-word-begin)
    ("ge" . evil-backward-word-end)
    ("W"  . evil-cp-forward-symbol-begin)
    ("E"  . evil-cp-forward-symbol-end)
    ("B"  . evil-cp-backward-symbol-begin)
    ("gE" . evil-cp-backward-symbol-end)))

(defvar evil-cp-swapped-movement-keys
  '(("w"  . evil-cp-forward-symbol-begin)
    ("e"  . evil-cp-forward-symbol-end)
    ("b"  . evil-cp-backward-symbol-begin)
    ("ge" . evil-cp-backward-symbol-end)
    ("W"  . evil-forward-word-begin)
    ("E"  . evil-forward-word-end)
    ("B"  . evil-backward-word-begin)
    ("gE" . evil-backward-word-end)))

(defvar evil-cp-additional-movement-keys
  '(("L"   . evil-cp-forward-sexp)
    ("H"   . evil-cp-backward-sexp)
    ("M-l" . evil-cp-end-of-defun)
    ("M-h" . evil-cp-beginning-of-defun)
    ("["   . evil-cp-previous-opening)
    ("]"   . evil-cp-next-closing)
    ("{"   . evil-cp-next-opening)
    ("}"   . evil-cp-previous-closing)
    ("("   . evil-cp-backward-up-sexp)
    (")"   . evil-cp-up-sexp)))

(defvar evil-cp-regular-bindings
  '(("d"   . evil-cp-delete)
    ("c"   . evil-cp-change)
    ("y"   . evil-cp-yank)
    ("D"   . evil-cp-delete-line)
    ("C"   . evil-cp-change-line)
    ("s"   . evil-cp-substitute)
    ("S"   . evil-cp-change-whole-line)
    ("Y"   . evil-cp-yank-line)
    ("x"   . evil-cp-delete-char-or-splice)
    (">"   . evil-cp->)
    ("<"   . evil-cp-<)
    ("_"   . evil-cp-first-non-blank-non-opening)
    ("M-T" . evil-cp-toggle-balanced-yank)
    ("M-z" . evil-cp-override))
  "Alist containing the regular evil-cleverparens bindings that
  override evil's bindings in normal mode.")

(defvar evil-cp-additional-bindings
  '(("M-t" . sp-transpose-sexp)
    ("M-k" . evil-cp-drag-backward)
    ("M-j" . evil-cp-drag-forward)
    ("M-J" . sp-join-sexp)
    ("M-s" . sp-splice-sexp)
    ("M-S" . sp-split-sexp)
    ("M-R" . evil-cp-raise-form)
    ("M-r" . sp-raise-sexp)
    ("M-a" . evil-cp-insert-at-end-of-form)
    ("M-i" . evil-cp-insert-at-beginning-of-form)
    ("M-w" . evil-cp-copy-paste-form)
    ("M-y" . evil-cp-yank-sexp)
    ("M-d" . evil-cp-delete-sexp)
    ("M-c" . evil-cp-change-sexp)
    ("M-Y" . evil-cp-yank-enclosing)
    ("M-D" . evil-cp-delete-enclosing)
    ("M-C" . evil-cp-change-enclosing)
    ("M-q" . sp-indent-defun)
    ("M-o" . evil-cp-open-below-form)
    ("M-O" . evil-cp-open-above-form)
    ("M-(" . evil-cp-wrap-next-round)
    ("M-)" . evil-cp-wrap-previous-round)
    ("M-[" . evil-cp-wrap-next-square)
    ("M-]" . evil-cp-wrap-previous-square)
    ("M-{" . evil-cp-wrap-next-curly)
    ("M-}" . evil-cp-wrap-previous-curly))
  "Alist containing additional functionality for
  evil-cleverparens via a modifier key (using the meta-key by
  default). Only enabled in evil's normal mode.")

(defun evil-cp--populate-mode-bindings-for-state (bindings state addp)
  "Helper function that defines BINDINGS for the evil-state
STATE when ADDP is true. If ADDP is false, then the keys in
BINDINGS are set to nil instead, effectively disabling the keys
in question."
  (--each bindings
    (evil-define-key state evil-cleverparens-mode-map
      (read-kbd-macro (car it))
      (if addp (cdr it) nil))))

;;;###autoload
(defun evil-cp-set-movement-keys ()
  "Sets the movement keys in
`evil-cleverparens-regular-movement-keys' or
`evil-cp-swapped-movement-keys' based on the value of
`evil-cleverparens-swap-move-by-word-and-symbol'."
  (interactive)
  (let ((keys (if evil-cleverparens-swap-move-by-word-and-symbol
                  evil-cp-swapped-movement-keys
                evil-cp-regular-movement-keys)))
    (evil-cp--populate-mode-bindings-for-state keys 'normal t)))

(defun evil-cp--enable-regular-bindings ()
  "Enables the regular evil-cleverparens bindings based on
`evil-cp-regular-bindings'."
  (evil-cp--populate-mode-bindings-for-state
   evil-cp-regular-bindings
   'normal
   t)
  (if evil-cleverparens-use-regular-insert
      ;; in case we change our mind
      (progn
        (evil-define-key 'normal evil-cleverparens-mode-map
          "i" 'evil-insert)
        (remove-hook 'evil-insert-state-exit-hook
                     'evil-cp-insert-exit-hook))
    (evil-define-key 'normal evil-cleverparens-mode-map
      "i" 'evil-cp-insert)
    (add-hook 'evil-insert-state-exit-hook
              'evil-cp-insert-exit-hook)))

;;;###autoload
(defun evil-cp-set-additional-movement-keys ()
  "Sets the movement keys is `evil-cp-additional-movement-keys'
for normal, visual and operator states if
`evil-cleverparens-use-additional-movement-keys' is true."
  (interactive)
  (dolist (state '(normal visual operator))
    (evil-cp--populate-mode-bindings-for-state
     evil-cp-additional-movement-keys
     state
     evil-cleverparens-use-additional-movement-keys)))

;;;###autoload
(defun evil-cp-set-additional-bindings ()
  "Sets the movement keys is `evil-cp-additional-bindings' for
normal-state if `evil-cleverparens-use-additional-bindings' is
true."
  (interactive)
  (evil-cp--populate-mode-bindings-for-state
   evil-cp-additional-bindings
   'normal
   evil-cleverparens-use-additional-bindings))

(defun evil-cp--enable-text-objects ()
  "Enables text-objects defined in evil-cleverparens."
  (define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
  (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
  (define-key evil-outer-text-objects-map "c" #'evil-cp-a-comment)
  (define-key evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
  (define-key evil-outer-text-objects-map "d" #'evil-cp-a-defun)
  (define-key evil-inner-text-objects-map "d" #'evil-cp-inner-defun))

(defun evil-cp--enable-surround-operators ()
  "Enables the use of `evil-cp-delete' and `evil-cp-change' with
`evil-surround-mode'"
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change)))

;;;###autoload
(define-minor-mode evil-cleverparens-mode
  "Minor mode for setting up evil with smartparens and paredit
for an advanced modal structural editing experience."
  :group 'evil-cleverparens
  :keymap '()
  :lighter (" ecp"
            (:eval (if evil-cleverparens-complete-parens-in-yanked-region
                       "/b" "/i")))
  :init-value nil
  (if evil-cleverparens-mode
      (progn
        (evil-cp-set-movement-keys)
        (evil-cp--enable-regular-bindings)
        (evil-cp--enable-text-objects)
        (evil-cp-set-additional-bindings)
        (evil-cp-set-additional-movement-keys)
        (if (bound-and-true-p evil-surround-mode)
            (evil-cp--enable-surround-operators)
          (add-hook 'evil-surround-mode-hook
                    'evil-cp--enable-surround-operators))
        (run-hooks 'evil-cleverparens-enabled-hook))
    (run-hooks 'evil-cleverparens-disabled-hook)))

(provide 'evil-cleverparens)
;;; evil-cleverparens.el ends here
