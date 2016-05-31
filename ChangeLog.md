# phpunit.el ChangeLog

## Version 0.9.0 (05/31/2016)

- #PR19: Some problem fixes (Tramp, phpunit-get-root-directory,
  Use phpunit -c option when set configuration file, ...) (thanks zonuexe)
- Remove keybinding from phpunit-helm

## Version 0.8.0 (05/12/2016)

- #PR16: Create a minor mode (thanks eric-hansen)
- #PR12: Using the phpunit installed by Composer (thanks wangchen)

## Version 0.7.0 (09/10/2015)

- #PR10: Set stty columns before running the phpunit command (Thanks Ryckes)

## Version 0.6.0 (08/27/2015)

- #PR9: Exact Class Names and Naming Conventions (Thanks muddletoes)

## Version 0.5.0 (08/23/2015)

- #PR7 : `php-get-current-class` matches style convention (Thanks muddletoes)
- #PR5 : Add error navigation after a failed test (Thanks Ryckes)

## Version 0.4.0 (03/02/2015)

- Update unit tests configuration for [overseer][]
- #PR4: Variables for phpunit's directory and filename (Ahmad N. Raja)
- Update [TravisCI][] and [Drone.io][] for continuous integration
- Add code coverage using [undercover][]

## Version 0.3.0 (10/24/2014)

- #PR2: Remove unnecessary "depends-on" from Cask
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


[TravisCI]: https://travis-ci.org/nlamirault/emacs-travis
[Drone.io]: https://drone.io/github.com/nlamirault/emacs-travis
[overseer]: https://github.com/tonini/overseer.el
[undercover]: https://github.com/sviridov/undercover.el
