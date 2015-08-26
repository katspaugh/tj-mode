tj-mode
-------

[![NPM version](https://img.shields.io/npm/v/tern-highlight.svg)](https://www.npmjs.org/package/tern-highlight)

Highlight JavaScript with [Tern](https://github.com/marijnh/tern).

## Features

### Syntactic Highlighting
 * Keywords + `this`
 * Variable declarations and function arguments
 * Function names (incl. variables and object properties)
 * Primitive literals (strings, numbers, booleans etc)
 * Comments (inline and block)

### Extra Features :boom:
 * Syntax errors
 * Trailing commas in array and object literals
 * Missing semicolons

<img src="http://i.imgur.com/GgK5w6l.png" width="273" />

## Emacs Mode
**tj-mode** is a major mode. Apart from syntax highligting, the mode provides:

 * Syntactic `beginning-of-defun` and `end-of-defun`
 * Automatic (idle) highlighting of references to the variable at point
 * Bouncing indentation (which is borrowed from the excellent
   [js2-mode](https://github.com/mooz/js2-mode)).

## Other Editors
Contributions of other editor plugins are very welcome.

## How to Install
Install like any other
[Tern plugin](http://ternjs.net/doc/manual.html#plugin_third_party). The
Tern plugin is called `tern-highlight`, and it's on npm, so:

```
npm install -g tern-highlight
```

You also need to enable the plugin in the `.tern-project` file in the
root of your project directory:

```JSON
{
    "plugins": {
        "highlight": {}
    }
}
```

## How to Install the Emacs Plugin
The Emacs package is available on MELPA. So `M-x package-install RET tj-mode` .
```Lisp
(add-to-list 'auto-mode-alist '("\\.js\\'" . tj-mode))
```

## Tests
TODO

### P.S.
This package is *not* written by [TJ Holowaychuk](https://github.com/tj) :smile:
