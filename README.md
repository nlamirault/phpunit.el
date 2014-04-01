# phpunit.el

Manage the [PHPUnit](http://phpunit.de) tests from Emacs.

## Installation

The recommended way to install ``phpunit.el`` is via [MELPA](http://melpa.milkbox.net/):

    M-x package-install phpunit.el

or [Cask](https://github.com/cask/cask):

	(depends-on "phpunit.el")


## Usage

### Available commands

2 functions are available :
* `phpunit-current-class`: launch unit tests for the current class
* `phpunit-current-project`: launch all unit tests


## Development

### Cask

``phpunit.el`` use [Cask](https://github.com/cask/cask) for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

Launch unit tests :

    $ make clean test


## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
