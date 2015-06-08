# phpunit.el ChangeLog

## Version 0.5.0 (IN PROGRESS)

- [#P5][] : Add error navigation after a failed test (Thanks Ryckes)

## Version 0.4.0 (03/02/2015)

- Update unit tests configuration for [overseer][]
- [#P4][]: Variables for phpunit's directory and filename (Ahmad N. Raja)
- Update [TravisCI][] and [Drone.io][] for continuous integration
- Add code coverage using [undercover][]

## Version 0.3.0 (10/24/2014)

- [#P2][]: Remove unnecessary "depends-on" from Cask
- `FIX` Add Package-Requires line to display dependencies
- Update documentation

## Version 0.2.0 (09/04/2014)

- Launch PHPUnit on current test
- Add optional arguments to PHPUnit
- Use `defcustom` instead of `defvar` for customizable variables
  (Thanks to [Syohex](https://github.com/syohex))


## Version 0.1.0 (04/04/2014)

- Launch current class tests
- Launch all tests from current projects


[#P2]: https://github.com/nlamirault/phpunit.el/pull/2
[#P4]: https://github.com/nlamirault/phpunit.el/pull/4
[#P5]: https://github.com/nlamirault/phpunit.el/pull/5

[TravisCI]: https://travis-ci.org/nlamirault/emacs-travis
[Drone.io]: https://drone.io/github.com/nlamirault/emacs-travis
[overseer]: https://github.com/tonini/overseer.el
[undercover]: https://github.com/sviridov/undercover.el
