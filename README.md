# phpunit.el

[![License GPL 3][badge-license]][LICENSE]

* Master [![MELPA Stable](https://stable.melpa.org/packages/phpunit-badge.svg)](https://stable.melpa.org/#/phpunit) [![Circle CI](https://circleci.com/gh/nlamirault/phpunit.el/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/phpunit.el/tree/master) [![Coverage Status](https://coveralls.io/repos/nlamirault/phpunit.el/badge.svg)](https://coveralls.io/r/nlamirault/phpunit.el)
* Develop : [![Melpa Status](https://melpa.org/packages/phpunit-badge.svg)](https://melpa.org/#/phpunit) [![Circle CI](https://circleci.com/gh/nlamirault/phpunit.el/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/phpunit.el/tree/develop) [![Coverage Status](https://coveralls.io/repos/github/nlamirault/phpunit.el/badge.svg?branch=develop)](https://coveralls.io/github/nlamirault/phpunit.el?branch=develop)

Manage the [PHPUnit][] tests from Emacs (>= 24.3)

## Installation

The recommended way to install ``phpunit.el`` is via [MELPA][]:

    M-x package-install phpunit.el

or [Cask][]:

	(depends-on "phpunit.el")


## Usage

### Available commands

These functions are available :
* `phpunit-current-test`: launch unit tests for the current test in a class
* `phpunit-current-class`: launch unit tests for the current class
* `phpunit-current-project`: launch all unit tests
* `phpunit-group`: launch PHPUnit for group

You can create some key bindings with these commands:

```lisp
(define-key web-mode-map (kbd "C-t t") 'phpunit-current-test)
(define-key web-mode-map (kbd "C-t c") 'phpunit-current-class)
(define-key web-mode-map (kbd "C-t p") 'phpunit-current-project)
```

or use the minor mode :

```lisp
(add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode))
```



### Configuration

The following configuration variables are available:

```lisp
(setq phpunit-configuration-file "phpunit.xml")
(setq phpunit-root-directory "./")
```

## Development

### Cask

``phpunit.el`` use [Cask](https://github.com/cask/cask) for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask

### Testing

* Launch unit tests from shell

        $ make clean test

* Using [overseer][] :

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>C-c , t</kbd>   | launch unit tests from buffer
<kbd>C-c , b</kbd>   | launch unit tests
<kbd>C-c , g</kbd>   | launch unit tests with tag (find, regexp, ...)

* Tips:

If you want to launch a single unit test, add a specify tag :

```lisp
(ert-deftest test-foobar ()
  :tags '(current)
  ```

And launch it using : <kbd>C-c , g</kbd> and specify tag : *current*


## Support / Contribute

See [here](CONTRIBUTING.md)



## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>


[badge-license]: https://img.shields.io/badge/license-GPL_2-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/phpunit.el/blob/master/LICENSE
[Issue tracker]: https://github.com/nlamirault/phpunit.el/issues

[PHPUnit]: http://phpunit.de

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: https://melpa.org/
[Cask]: http://cask.github.io/
[Overseer]: https://github.com/tonini/overseer.el
