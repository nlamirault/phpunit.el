# Copyright (C) 2014-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

APP = phpunit

export EMACS ?= $(shell which emacs)
EMACSFLAGS = -L .
CASK = cask
VAGRANT = vagrant

VERSION=$(shell \
        grep Version phpunit.el \
        |awk -F':' '{print $$2}' \
	|sed -e "s/[^0-9.]//g")

PACKAGE_FOLDER=$(APP)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)    : initialize development environment"
	@echo -e "$(WARN_COLOR)- build$(NO_COLOR)   : build project"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)    : launch unit tests"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR)   : cleanup"
	@echo -e "$(WARN_COLOR)- package$(NO_COLOR) : packaging"

.PHONY: init
init:
	@echo -e "$(OK_COLOR)[$(APP)] Initialize environment$(NO_COLOR)"
	@$(CASK) --dev install

elpa:
	@echo -e "$(OK_COLOR)[$(APP)] Build$(NO_COLOR)"
	@$(CASK) install
	@$(CASK) update
	@touch $@

.PHONY: build
build : elpa $(OBJECTS)

.PHONY: test
test: build vendor/bin/phpunit
	@echo -e "$(OK_COLOR)[$(APP)] Unit tests$(NO_COLOR)"
	@$(CASK) emacs --batch -L . -L test -l phpunit-test -l test-helper \
		-l phpunit-version-test -f ert-run-tests-batch

.PHONY: virtual-test
virtual-test: check-env
	@$(VAGRANT) up
	@$(VAGRANT) ssh -c "source /tmp/.emacs-gotest.rc && make -C /vagrant EMACS=$(EMACS) clean init test"

.PHONY: virtual-clean
virtual-clean:
	@$(VAGRANT) destroy

.PHONY: clean
clean :
	@echo -e "$(OK_COLOR)[$(APP)] Cleanup$(NO_COLOR)"
	@rm -fr $(OBJECTS) elpa vendor $(APP)-pkg.el $(APP)-pkg.elc $(ARCHIVE).gz

reset : clean
	@rm -rf .cask

pkg-file:
	$(CASK) pkg-file

pkg-el: pkg-file
	$(CASK) package

package: clean pkg-el
	@echo -e "$(OK_COLOR)[$(APP)] Packaging$(NO_COLOR)"
	cp dist/$(ARCHIVE) .
	gzip $(ARCHIVE)
	rm -fr dist

vendor/bin/phpunit:
	(test -d "vendor/bin" || mkdir -p "vendor/bin") && touch "vendor/bin/phpunit"

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
