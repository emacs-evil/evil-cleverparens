# evil-cleverparens

`evil-cleverparens` is modal-editing optimized for editing Lisp. It works under the following principles:

1) Act like Vim/evil where useful, but prevent actions that would throw the order of your parentheses and other delimiters into question.

2) Make the most out of the combination of structural and modal editing.

3) Provide but don't force additional features on the user.

## Installation

[![MELPA](http://melpa.org/packages/evil-cleverparens-badge.svg)](http://melpa.org/#/evil-cleverparens)

The recommended way to install is via `elpa` from [MELPA](http://melpa.org/). The following should work:

`M-x` `package-install evil-cleverparens`

`evil-cleverparens` uses functions from both `smartparens` and `paredit`. Neither one is required by default, but using one of them is highly recommended, as `evil-cleverparens` doesn't provide anything for the `insert-state`. If you are an user of `smartparens`, `smartparens-strict-mode` is also recommended.

To enable `evil-cleverparens` with your favorite lispy-mode use:

```emacs-lisp
(add-hook '<your-lispy-mode> #'evil-cleverparens-mode)
```

If you are using `evil` with `evil-move-cursor-back` as `t` and `evil-move-beyond-eol` as `nil` (the defaults), I would recommend putting `(setq evil-move-beyond-eol t)` in your config so that certain movement commands don't break at the end of the lines (see [issue #29](https://github.com/luxbock/evil-cleverparens/issues/29) for details).

# Features

## Text Objects

`evil-smartparens` includes evil text-objects for forms, comments and defuns. Since these may be useful in non-lisp-like modes, they have been separated out into their own file and can be used with:

```emacs-lisp
(require 'evil-cleverparens-text-objects).
```

in which case you need to give them key-bindings on your own.

When used with `evil-cleverparens` we have:

#### *Form* bound to `f`

Form is a pair-delimited range as defined by `smartparens` for your major-mode. It's much like if you combined `evil-(a|inner)-(paren|bracket|curly|double-quote)` under a single text-object.

#### *Comment* bound to `c`

Selecting an *outer* comment means selecting both the comment delimiter and the comment text, whereas selecting an *inner* comment means selecting only the text but not the comment delimiters.

#### *Defun* bound to `d`

Selects the top-level form.

## Movement

Part of embracing Lisp and structural editing is learning to love the parentheses. Vim/evil is optimized for moving around by units of text, but for friends of Lisp the parentheses are more than just text. Therefore in addition to the regular evil movement keys, the following set of commands are provided:

| Key   | Behavior                                  |
|-------|-------------------------------------------|
| `H`   | Move backward by sexp                     |
| `L`   | Move forward by sexp                      |
| `M-h` | Move to the beginning of a top-level form |
| `M-l` | Move to the end of the top-level form     |
| `(`   | Move backward up a sexp.                  |
| `)`   | Move forward up a sexp.                   |
| `[`   | Move to the previous opening parentheses  |
| `]`   | Move to the next closing parentheses      |
| `{`   | Move to the next opening parentheses      |
| `}`   | Move to the previous closing parentheses  |

To customize them see `evil-cleverparens-use-additional-movement-keys` to enable/disable them and `evil-cp-additional-movement-keys` for the keys.


Besides parentheses, the other logical unit of structural movement are symbols. Unfortunately the regular Vim/evil movement keys (`w`, `W`, `b`, `B`, `ge` and `gE`) complect this idea by not making a distinction between symbols and delimiters. This distinction is made for you when using the `evil-cp-(forward|backward)-(word|symbol)-(begin|end)` commands, which happily skip past all parentheses and other delimiters.

**TODO**: Moving by *word*'s by skipping delimiters doesn't work yet.

If you prefer the vanilla Vim way, set `evil-cleverparens-move-skip-delimiters`
to `nil`.

There's also an option to swap the `symbol` / `word` behavior of these keys controlled by: `evil-cleverparens-swap-move-by-word-and-symbol`.

## Yank / Delete / Change

When it comes to yanking (in the Vim sense) unbalanced expressions, there are two ways to handle this:

- 1) Ignore the extra parentheses of the yanked region and only store the safe parts.
- 2) Store everything and fill in the missing delimiters in kill-ring.

`evil-cleverparens` supports both of these approaches, where the former is called *ignoring* and the latter *balancing* behavior. This defaults to *ignoring* and can be toggled with `M-T`. The current behavior is indicated in the modeline via `ecp/i` for *ignoring* and `ecp/b` for *balancing*.

Because checking and fixing the safety of a region can be computationally expensive, there is a variable `evil-cleverparens-threshold` (defaults to 1500) that controls upto how many characters will be checked before giving up and defaulting the normal `evil` behavior. In practice this is rarely a problem but it is something worth being aware of. This feature is adopted from [evil-smartparens](https://github.com/expez/evil-smartparens).

If for some reason `evil-cleverparens` messes up, or you would just like to torture your program for unspecified reasons, you can skip the protective measures by prepending next command with `M-z` aka `evil-cp-override`. 

#### `yy`, `dd` and `cc`

- Should work pretty much as you would expect!
- `dd` and `cc` will leave the point to indentation or to the last unmatched opening delimiter of the line.
- `evil-cleverparens-indent-afterwards` (defaults to `t`) controls if the surrounding form should be indented after deleting or changing.

#### `Y`, `D` and `C`

- On safe lines the behavior is identical to `evil`.
- On lines with unmatched opening delimiters, the range of the operation extends all the way to the end of those forms, spanning multiple lines.
- On lines with unmatched closing delimiters, the range of operation extends from *point* to the *end-of-line*.

#### `M-y`, `M-d` and `M-c`

- These act from *point* to the end of the current or next sexp similar to how `paredit-kill-sexp` works.
- Calling them at the end of a form will act on the whole form.
- Calling them with the `universal-argument` affects everything from point until the end of the current surrounding form. 

#### `M-Y`, `M-D` and `M-C`

- Similar to the their little cousins, but act on the enclosing form, and in the case of `delete` and `change` will also perform whitespace clean-up after themselves.
- Numeric argument increases the enclosing form up by a level.
- Using a single `universal-argument` deletes the top-level form and its surrounding newlines.

## Other Vim Related Features

#### `s` and `S`

- `s` is identical to `evil`'s, but skips over delimiteres.
- `S` is the same as `C` but always starts from the beginning of the line and skips over any unmatched delimiters.

#### `x` deletes and splices

- The `x` key works the same way it does in `evil`, but when called on opening or closing delimiters of a form, it will delete both delimiters. 

#### `_`

- Since `^` and `_` in `evil` do the same thing, i.e. bring the point to the first non-blank column, `evil-cleverparens` has told `_` to bring the point to the first non-blank non-opening instead.

#### Insert

- When entering `insert-state` via `i` in a situation where the point is between a round opening parentheses and a symbol, `evil-cleverparens` will automatically insert a space and then move the cursor back for you. The rationale is that when you are in this situation, it's much more likely that you are inserting a new word at the beginning of the list rather than modifying the beginning of the current head of the list, and therefore it would be nice if the two words were already separated so that your auto-completion mode can do its thing. This behavior can be disabled by setting `evil-cleverparens-use-regular-insert` to `t`.

- `M-i` or `evil-cp-insert-at-the-beginning-of-form` and `M-a` or `evil-cp-insert-at-end-of-form` are analogous to the `evil` commands `I` and `A`, but instead of working on lines, they bring the cursor to the beginning/end of the current surrounding sexp while entering `insert-state`.

- In the same manner, `M-o` or `evil-cp-open-below-form` and `M-O` or `evil-cp-open-above-form` are analogous to `evil`'s `o` and `O`, again working on forms instead of lines.

## Additional Functionality

Most of the following functionality is controlled by the `evil-cleverparens-use-additional-bindings` setting. The keys can be customized by editing `evil-cp-additional-bindings` (needs to be re-initialized).

#### Dragging / Transposing

In addition to regular transpose, bound to `M-t` (`sp-transpose-sexp`), `evil-cleverparens` provides additional means of moving sexps around with behavior inspired by the [drag-stuff-el](https://github.com/rejeep/drag-stuff.el) mode. `evil-drag-forward` bound to `M-j` and `evil-drag-backward` bound to `M-k` attempt to drag the *thing* under point forward or backward. The depth of the thing being moved never changes, i.e. dragging is distinct from slurping or barfing.

The *thing* can be many different things depending on the location of the cursor:

- On top of a symbol acts on that symbol.
- Inside or on the delimiters of a form moves the form.
- If a symbol or a form can't be moved any further, the command acts on its surrounding form.
- Outside a form on a safe line will move the line.
- On a top-level comment will move the entire comment-block.

The behavior of the command with respect to top-level sexps and lines can be customized with `evil-cleverparens-drag-ignore-lines` (default `nil`). You can also choose not to treat connected commented lines as singular units by setting `evil-cleverparens-drag-comment-blocks` to `nil`.

#### Slurping and Barfing

`evil-cleverparens` implements slurping and barfing both forwards and backwards using only the `<` and `>` keys by making them act differently when on top of a form delimiter:

- On an opening delimiter `<` will *slurp backwards* as many times as there are preceding opening delimiters.
- On a closing delimiter `<` will *barf forwards*. If the form contains only a single sexp inside it then `evil-cleverparens` will question your intention and instead do nothing. Perhaps you meant to *splice* or *delete* instead?
- On a opening delimiter `>` will *barf backwards* with the same caveat as above.
- On a closing delimiter `>` will *slurp forwards* as many times as there are closing delimiters behind.
- When inside a form `<` will *barf* and `>` will *slurp* *forwards*.

While the specifics of the dragging, slurping and barfing behavior may seem complicated when spelt out, a lot of thought has been put to make their use feel intuitive and fluid.

#### Wrapping

While `evil-cleverparens` editing operations work well with [evil-surround](https://github.com/timcharper/evil-surround), the following specialised wrapping commands are also provided:

| Key   | Behavior                                     |
|-------|----------------------------------------------|
| `M-(` | Wrap the next sexp in round parentheses.     |
| `M-)` | Wrap the previous sexp in round parentheses. |
| `M-[` | Wrap the next sexp in square brackets.       |
| `M-]` | Wrap the previous sexp in square brackets.   |
| `M-{` | Wrap the next sexp in curly braces.          |
| `M-}` | Wrap the previous sexp in curly braces.      |

Each command can take a numeric argument to determine how many sexps to operate on maxing out at the number of sexps from the cursor until the end of the surrounding form. When called with the `universal-argument`, the operations act on the surrounding form instead. The universal argument can be called multiple times, where each consecutive call will wrap an additional expression from the surrounding form.

#### Copy and Paste

`M-w` aka `evil-cp-evil-copy-paste-form` will copy the surrounding form and insert it below with the proper indentation. If called outside a form it will do the same with the current line instead (as long as its safe). This command can be called with a numeric argument to repeat the paste operation that many times. Calling it with the `universal-argument` will copy-paste the current top-level form and insert newlines between them.

#### From Smartparens

The following commands have been lifted straight from `smartparens`:

| Key   | Behavior            |
|-------|---------------------|
| `M-q` | `sp-indent-defun`   |
| `M-J` | `sp-join-sexp`      |
| `M-s` | `sp-splice-sexp`    |
| `M-S` | `sp-split-sexp`     |
| `M-t` | `sp-transpose-sexp` |
| `M-v` | `sp-convolute-sexp` |
| `M-r` | `sp-raise-sexp`     |

`M-R` bound to `evil-cp-raise-form` acts like `sp-raise-sexp` but on the enclosing form instead of the next one.

# Notes and Tips

#### Universal Argument

Some of the commands in `evil-cleverparens` optionally accept the `universal-argument` (`C-u` in vanilla Emacs), which is not bound by default in `evil` and has no equivalent in Vim. If you wish to use these features, you must bind it to a key, but that alone only allows you to invoke it once, because calling `universal-argument` enables a transient keymap that assumes that you are using `C-u` for the next invocation. You can therefore use e.g. `<my-key> C-u C-u` to invoke it three times, or you can bind your own key like this:

```emacs-lisp
(define-key universal-argument-map (kbd <my-key>) 'universal-argument-more)
```

#### Prefixed Sexps

`evil-cleverparens` relies on `smartparens` to identify forms where the opening delimiter contains a prefix, such as quoted lists or the anonymous function literals in Clojure. If you are having problems with such prefixed forms, make sure that the variable `sp-sexp-prefix` is correctly configured for the mode you are using.

#### Hilighting Parentheses

Using [highlight-parentheses.el](https://github.com/tsdh/highlight-parentheses.el) to highlight three of the closest delimiters from the location of the point with fixed colors makes it very easy to quickly identify the range of many of the operations of `evil-cleverparens.`


# See Also

`evil-cleverparens` is not the first Emacs/evil mode that tries to make structural editing of lisp-like languages easier. You might enjoy checking out the following modes as well:

### [abo-abo/lispy](https://github.com/abo-abo/lispy)

Very rich in features but doesn't attempt to conform to the `vim`/`evil` bindings.

### [roman/evil-paredit](https://github.com/roman/evil-paredit)

Prevents the user from messing up their parentheses by erroring out. `evil-cleverparens` originally started out as a fork of this project, with the goal of doing something useful instead of throwing an error in situations where it would make sense.

### [syl20bnr/evil-lisp-state](https://github.com/syl20bnr/evil-lisp-state)

As the name suggests, this project creates an additional state for editing lisp in `evil`.

### [expez/evil-smartparens](https://github.com/expez/evil-smartparens)

Had I known of this project when starting out I would have just contributed to it instead of writing a lot of the same functionality on my own, but by the time I discovered it I had already so much code in place that I decided to continue with my own version. Some of the code in `evil-cleverparens` is lifted directly from here, and the modes work roughly the same. `evil-cleverparens` has more features and opinions, and probably more bugs :).

