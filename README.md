# evil-cleverparens

Use Vim/evil like modal editing with lisp without screwing up the structure of
your code. Tries to offer useful alternatives for behavior which would otherwise
be destructive.

## Installation

The recommended way to install is via `elpa` from
[MELPA](http://melpa.org/). The following should work:

`M-x` `package-install evil-cleverparens`

`evil-cleverparens` uses functions from both `smartparens` and
`paredit`. Neither one is required by default, but using one of them is highly
recommended, as `evil-cleverparens` doesn't provide anything for the
`insert-state`. If you are an user of `smartparens`, `smartparens-strict-mode`
is also recommended.

If you use `smartparens` with non-lispy modes you probably want to only activate
`evil-cleverparens` with certain major modes.

```emacs-lisp
(add-hook '<your-lispy-mode> #'evil-cleverparens-mode)
```

## Features

### Editing

#### Yanking

**Problem**: You yank a region of text that contains unbalanced delimiters, so now
pasting that region into your buffer will result in an unbalanced mess. 

There are three solutions to this problem:
-   Don't let the user make such mistakes by erroring out when they are issued.
-   Let the user issue the command but ignore parts of it that would result in
    misaligned structure.
-   Let the user issue the command, and fix it up by supplying the missing
    delimiters.

`evil-cleverparens` supports the latter two approaches. You can toggle the
behavior between balancing and ignoring behavior via `M-T`, or by binding
`evil-cp-override` to the key of your choice. If you are a user of the `diminish`
mode, you can see the current behavior from the mode-line indicated by `/i` or `/b`
for *ignoring* or *balancing* behavior. 

For example, if you have:

    (foo
     )

And yank the first line, the balancing behavior will store `(foo)` in your
kill-ring, whereas the ignoring behavior will store just `foo` instead. 

1.  `yy` aka line-wise yank

    Yanking the whole-line with `yy` behaves as you'd expect, with the minor
    difference that yanking unbalanced expressions won't insert the new-line that a
    regular line-wise yank would do.

2.  `Y` aka `evil-cp-yank-line`

    Acts like regular `evil-yank-line` when the line is safe to yank as it
    is. Otherwise it does what `paredit-copy-as-kill` does for any expression on
    the current line. If you have:
    
   `(foo
     bar)`
    
    With point at the beginning of the buffer, then `Y` will yank the whole sexp
    instead of just the first line.

#### Deleting

1.  `dW`

    `dW` follows the logic outlined above, and respects the ignoring / balancing
    behavior as mentioned. `evil-cleverparens` doesn't change the region of the
    `big-WORD` text object, but only makes sure that issuing such a command
    won't delete any delimiters that would leave your document
    unbalanced. Therefore my suggestion is to use `dio` and `dao` to delete by
    symbol instead.

2.  `dd`

    If the line is balanced, acts like regular evil's `dd` would do. Otherwise deletes
    the line while saving out any parentheses that would put the region out of
    balance.

    ```emacs-lisp
    (defun im-a-function ()
      (i-do-stuff 1 2 3))
    ```

    Calling `dd` on the first line of the function definition will result in:
    
    `((i-do-stuff 1 2 3))`
    
    And repeating the command would delete everything.

3.  `vd`

    Deleting in visual mode works as you would expect, with the additional bonus
    that visual-block-state is handled as well.

4.  `D`

    `evil-cp-delete-line` deletes from *point* until the end of the line, or until it
    reaches a point where deleting more would leave the line out of balance.
    
    ```clojure
    (map (fn [x] |(conj coll (rest x))) stuff)
    ```
    
    So with point at marked by `|`, pressing `D` would leave you with:

    ```clojure
    (map (fn [x]) stuff)
    ```

#### Changing

Given the behavior of `evil-cp-delete`, changing, i.e. `c` and `C` should work as you
would expect.

### Movement

#### By word / symbol

The regular movement keys, i.e. `w`, `e`, `b`, `ge`, `W`, `E`, `B` and `gE` ignore parentheses
and string delimiters. I find that when I edit lisp, it's more common for me to
want to move by symbol rather than by word, so I have provided a customizable
variable, `evil-cleverparens-swap-move-by-word-and-symbol`, which reverses the
behavior of `w`, `e`, `b` and `ge` with those of `W`, `E`, `B` and `gE`.

#### Structurally

| Key   | Behavior                                  |
|-------|-------------------------------------------|
| `H`   | Move backward by form                     |
| `L`   | Move forward by form                      |
| `M-h` | Move to the beginning of a top level form |
| `M-l` | Move to the end of the top level form     |
| `[`   | Move to the previous opening parentheses  |
| `]`   | Move to the next closing parentheses      |
| `{`   | Move to the next opening parentheses      |
| `}`   | Move to the previous closing parentheses  |
| `(`   | Move backward up a sexp.                  |
| `)`   | Move forward up a sexp.                   |

#### Others

Since `^` and `_` do the same thing in regular `evil`, `evil-cleverparens` takes over
the `_` key and binds it to `evil-cp-first-non-blank-non-opening` which, as you may
guess from the name, moves the point to the first position that's not whitespace
nor an opening delimiter.

### Text Objects

`evil-cleverparens` adds the following text objects:

#### *Form* bound to `f`

Form is either a s-expression or a string, as defined by `smartparens` for the
mode in question.

#### *Comment* bound to `c`

Selecting an *outer* comment means selecting both the comment delimiter and the
comment text, whereas selecting an *inner* comment means selecting only the text
but not the comment delimiters.

#### *Defun* bound to `d`

Selects the top-level s-expression.

### Extra

#### Slurping and Barfing

Slurping and barfing in `normal-state` is done with the `<` and `>` keys. They
do slightly different things depending on the location of the point inside the
form:

| Location of point | Command     | Effect          |
|-------------------|-------------|-----------------|
| Opening delimiter | `evil-cp->` | Barf backwards  |
| Opening delimiter | `evil-cp-<` | Slurp backwards |
| Else              | `evil-cp->` | Slurp forwards  |
| Else              | `evil-cp-<` | Slurp forwards  |

In addition if the point was on an opening/closing delimiter it will move along
with the delimiter. Otherwise the point maintains its position.

#### Splicing / Splitting

I like to think of s-expressions as forming a layer of levels that define the
[AST](http://en.wikipedia.org/wiki/Abstract_syntax_tree) of the code they
represent, kind of like this picture of rice fields in China:

![rice-fields](rice_fields.jpg)

In this analogy, *splicing* is like taking a layer and leveling it down one
step. In `evil-cleverparens` this can be done in two ways: If you are standing
in the middle of the rice field, then calling `M-s` or `sp-splice-sexp` will do
just that. If you are at the edges of the field (i.e. at the parentheses) then
calling `x` will delete both the delimiter you are looking at plus its matching
pair. If you're not looking at a delimiter, `x` works just as it would in
regular `evil`.

`((foobar))` -> `(foobar)`

To continue the rice field analogy, *splitting* is like digging a ditch to
separate two fields from each other, and it's bound to `M-S`.

`(foo bar some | foobars)` -> `(foo bar some) | (foobars)`

#### Dragging / Transposing

`evil-cleverparens` incorporates the
[drag-stuff.el](https://github.com/rejeep/drag-stuff.el) mode via `M-j` and
`M-k`. If the two lines they are acting on are both clear of obstructions, then
`evil-cleverparens` will act the same as `drag-stuff` by swapping the two lines
in question, i.e.:

```emcas-lisp
;; This is a comment |
(this-is-a-form)
```

with point represented by |, will turn into this:

```emacs-lisp
(this-is-a-form)
;; This is a comment |
```

If one of the lines is safe, but swapping it with another would disturb the
balance of the following expression, then the command teleports the safe line to
the other side of the unbalanced form:

```emacs-lisp
;; This is a comment |
(defun im-a-function ()
  (foobar))
```

```emacs-lisp
(defun im-a-function ()
  (foobar))
;; This is a comment |
```

If both lines are part of unbalanced expressions, then the `M-j` and `M-k` keys will
transpose the forms the point is located in forwards or backwards. 

```emacs-lisp
(when (region-active-p|)
    (> (abs (- (region-beginning) (region-end)))
       evil-cleverparens-threshold))
```

```emacs-lisp
(when (> (abs (- (region-beginning) (region-end)))
       evil-cleverparens-threshold)
    (region-active-p|))
```

If the point is inside a top-level form expression, then that form gets
transposed with the following top-level form, with the safe lines in between
being unaffected.

In addition to the dragging behavior, you can also use traditional transposing
with `sp-transpose-sexp` bound to `M-t`.

#### Wrapping

`evil-cleverparens` and its text objects work well with [evil-surround](https://github.com/timcharper/evil-surround).

#### Raising

`sp-raise-sexp` is bound to `M-r`.

#### Quick insert

The following keys can be used to quickly move and enter the `insert-state`
in a position relative to the location of point inside a form:

| Command | Destination                                    |
|---------|------------------------------------------------|
| `M-a`   | End of the current form                        |
| `M-i`   | Beginning of the current form                  |
| `M-o`   | Below the current form, but inside its parent  |
| `M-O`   | Before the current form, but inside its parent |

These keys give the behavior of the regular `a`, `i`, `o` and `O` keys of `evil` a lispy
feel.

# See Also

`evil-cleverparens` is not the first Emacs/evil mode that tries to make structural
editing of lisp-like languages easier. You might enjoy checking out the
following modes as well:

### [abo-abo/lispy](https://github.com/abo-abo/lispy)

Very rich in features but doesn't attempt to conform to the `vim/evil` layout of bindings.

#### [roman/evil-paredit](https://github.com/roman/evil-paredit)

Prevents the user from messing up their parentheses by erroring
out. `evil-cleverparens` originally started out as a fork of this project, with
the goal of doing something useful instead of throwing an error in situations
where it would make sense.

### [syl20bnr/evil-lisp-state](https://github.com/syl20bnr/evil-lisp-state)

As the name suggests, this project creates an additional state for editing
lisp in `evil`.

### [expez/evil-smartparens](https://github.com/expez/evil-smartparens)

Had I known of this project when starting out I would have just contributed to
it instead of writing a lot of the same functionality on my own, but by the
time I discovered it I had already so much code in place that I decided to
continue with my own version. Some of the code in `evil-cleverparens` is lifted
directly from here, and the modes work roughly the same. As far as I am aware,
the two projects are different in the following ways:
-   Deleting by line is different. In `evil-smartparens` the region to delete is
    determined in part by the location of the point, and the maximum safe
    region that this can be expanded to. `evil-cleverparens` on the other hand
    deletes everything except parentheses / string delimiters that would
    unbalance the region, and joins the next line to where the last opening
    parentheses of the deleted line existed.
-   When yanking an unbalanced region, `evil-cleverparens` gives you the option
    of choosing between ignoring (the `evil-smartparens` way) or supplementing
    the offending parentheses in kill-ring via
    `evil-cleverparens-balance-yanked-region`.

## Limitations and the Escape Hatch

Ensuring that a region is safe can be expensive. Similar to `evil-smartparens`,
`evil-cleverparens` provides a variable `evil-cleverparens-threshold` that
controls how large the region should be before defaulting to the regular and
unsafe `evil` functions. 

Another feature stolen from `evil-smartparens` is an escape hatch,
`evil-cp-override`, which is bound to `o` in `visual-state`. Pre-fixing another
command with it will make `evil-cleverparens` default to using the regular `evil`
alternatives. `r` and `R` are the same as in regular `evil` so those can be used to
fix annoying situations as well.

## Disclaimer

This is my first Emacs Lisp project more than 100 lines long, so the code is
likely ugly and likelihood of bugs is quite high. Bug reports/fixes are
welcome.
