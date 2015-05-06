tj-mode
-------

Highlight JavaScript with [Tern](https://github.com/marijnh/tern).

## Features

### Syntactic Highlighting
 * Keywords + `this`
 * Variable declarations and function arguments
 * Function names (incl. variables and object properties)
 * Primitive literals (strings, numbers, booleans etc)
 * Comments (inline and block)

### Extra Features :boom:
 * Syntax errors (from strict parser)
 * Undeclared variables
 * Trailing commas in array and object literals
 * Missing semicolons after statements

<img src="http://i.imgur.com/GgK5w6l.png" width="273" />

## Emacs Mode
**tj-mode** is a major mode. Apart from syntax highligting, the mode provides:

 * Syntactic `beginning-of-defun` and `end-of-defun`
 * Automatic (idle) highlighting of references to the variable at point

Bouncing indentation is borrowed from the excellent [js2-mode](https://github.com/mooz/js2-mode).

## Other Editors
Contributions of other editor plugins are very welcome.

## How to Install
Install like any other
[Tern plugin](http://ternjs.net/doc/manual.html#plugin_third_party). The
Tern plugin is called `tern-highlight`, and it's on on npm, so:

```
npm install -g tern-highlight
```

You also need to enable the plugin in your `.tern-project` files:

```JSON
{
    "plugins": {
        "highlight": {}
    }
}
```

## How to Install the Emacs Plugin
The Emacs package is not on MELPA yet, so you have to install it manually:
```Lisp
(load "~/.emacs.d/site-lisp/tj-mode.el")
(add-to-list 'auto-mode-alist '("\\.js\\'" . tj-mode))
```

## Tests
TODO

### Credits
Some of the code in `tokens.js` (the part about undefined variables) is taken from [tern-lint](https://github.com/angelozerr/tern-lint).

### P.S.
This package is *not* written by [TJ Holowaychuk](https://github.com/tj) :smile:
