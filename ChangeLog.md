# phpunit.el ChangeLog

## Version 0.17.1 (12/08/2018)

- [#60](https://github.com/nlamirault/phpunit.el/pull/60): Hotfix: Path to `phpunit` could not be expanded

## Version 0.17.0 (09/08/2018)

- [#54](https://github.com/nlamirault/phpunit.el/pull/54): Fix warnings
- [#55](https://github.com/nlamirault/phpunit.el/pull/55): Integrate .dir-locals.el
- [#56](https://github.com/nlamirault/phpunit.el/pull/56): Fix `Makefile` for test
- [#57](https://github.com/nlamirault/phpunit.el/pull/57): Rename variable names
- [#58](https://github.com/nlamirault/phpunit.el/pull/58): Fix for spaces in project path (thanks @jflearn)

## Version 0.16.0 (27/11/2017)

- [#52](https://github.com/nlamirault/phpunit.el/pull/52): windows system prohibits the use of stty (tszg)
- [#49](https://github.com/nlamirault/phpunit.el/pull/49): Add option to hide compilation buffer if all tests pass (thanks mallt)
- [#48](https://github.com/nlamirault/phpunit.el/pull/48): Support colorize output (thanks zonuexe)
- [#46](https://github.com/nlamirault/phpunit.el/pull/46): Add custom variables :tag (thanks zonuexe)
- [#45](https://github.com/nlamirault/phpunit.el/pull/45): Add path to current test file for phpunit-current-test (thanks landakram)
- [#44](https://github.com/nlamirault/phpunit.el/pull/44): Add ability to specify a bootstrap file (thanks landakram)

## Version 0.15.0 (02/11/2017)

- [#43](https://github.com/nlamirault/phpunit.el/pull/42): Rename test-helper to phpunit-test-helper (thanks zonuexe)
- [#41](https://github.com/nlamirault/phpunit.el/pull/41): Remove bundled ert.el (thanks tarsius)

## Version 0.14.0 (12/19/2016)

- [#39](https://github.com/nlamirault/phpunit.el/pull/39): Support custom phpunit command (thanks zonuexe)
- Modify phpunit detection algorithm
- [#36](https://github.com/nlamirault/phpunit.el/pull/36): Modify custom group (thanks zonuexe)

## Version 0.13.0 (03/09/2016)

- [#34](https://github.com/nlamirault/phpunit.el/pull/34): Fix test phpunit-get-class (thanks zonuexe)
- [#33](https://github.com/nlamirault/phpunit.el/pull/33): Fix phpunit-current-test (thanks zonuexe)
- [#30](https://github.com/nlamirault/phpunit.el/pull/30): Modify phpunit command priority (thanks zonuexe)
- [#29](https://github.com/nlamirault/phpunit.el/pull/29): Refactor customize (thanks zonuexe)

## Version 0.12.0 (08/08/2016)

- `FIX` unit tests if *phpunit* executable exists.
- `FIX` Unit tests launcher using *overseer*
- Update documentation for unit testing
- Setup code coverage badges for *master* and *develop* branches

## Version 0.11.0 (08/08/2016)

- [#25](https://github.com/nlamirault/phpunit.el/pull/25): Add phpunit-group (Thanks zonuexe)

## Version 0.10.0 (08/07/2016)

- [#23](https://github.com/nlamirault/phpunit.el/pull/23): Better regexp using rx (Thanks zonuexe)
- [#22](https://github.com/nlamirault/phpunit.el/pull/22): Simplify phpunit-current-class (Thanks zonuexe)
- [#21](https://github.com/nlamirault/phpunit.el/pull/21): Fix for Emacs 25.1 (Thanks zonuexe)

## Version 0.9.0 (05/31/2016)

- [#19](https://github.com/nlamirault/phpunit.el/pull/19): Some problem fixes (Tramp, phpunit-get-root-directory,
  Use phpunit -c option when set configuration file, ...) (thanks zonuexe)
- Remove keybinding from phpunit-helm

## Version 0.8.0 (05/12/2016)

- [#16](https://github.com/nlamirault/phpunit.el/pull/16): Create a minor mode (thanks eric-hansen)
- [#12](https://github.com/nlamirault/phpunit.el/pull/12): Using the phpunit installed by Composer (thanks wangchen)

## Version 0.7.0 (09/10/2015)

- [#10](https://github.com/nlamirault/phpunit.el/pull/10): Set stty columns before running the phpunit command (Thanks Ryckes)

## Version 0.6.0 (08/27/2015)

- [#9](https://github.com/nlamirault/phpunit.el/pull/9): Exact Class Names and Naming Conventions (Thanks muddletoes)

## Version 0.5.0 (08/23/2015)

- [#7](https://github.com/nlamirault/phpunit.el/pull/7) : `php-get-current-class` matches style convention (Thanks muddletoes)
- [#5](https://github.com/nlamirault/phpunit.el/pull/5) : Add error navigation after a failed test (Thanks Ryckes)

## Version 0.4.0 (03/02/2015)

- Update unit tests configuration for [overseer][]
- [#4](https://github.com/nlamirault/phpunit.el/pull/4): Variables for phpunit's directory and filename (Ahmad N. Raja)
- Update [TravisCI][] and [Drone.io][] for continuous integration
- Add code coverage using [undercover][]

## Version 0.3.0 (10/24/2014)

- [#2](https://github.com/nlamirault/phpunit.el/pull/2): Remove unnecessary "depends-on" from Cask
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
