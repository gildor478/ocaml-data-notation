################################################################################
#  ocaml-data-notation: Store data using OCaml notation                        #
#                                                                              #
#  Copyright (C) 2009-2011, OCamlCore SARL                                     #
#  Copyright (C) 2013, Sylvain Le Gall                                         #
#                                                                              #
#  This library is free software; you can redistribute it and/or modify it     #
#  under the terms of the GNU Lesser General Public License as published by    #
#  the Free Software Foundation; either version 2.1 of the License, or (at     #
#  your option) any later version, with the OCaml static compilation           #
#  exception.                                                                  #
#                                                                              #
#  This library is distributed in the hope that it will be useful, but         #
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  #
#  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          #
#  details.                                                                    #
#                                                                              #
#  You should have received a copy of the GNU Lesser General Public License    #
#  along with this library; if not, write to the Free Software Foundation,     #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               #
################################################################################

default: test

# OASIS_START
# DO NOT EDIT (digest: 7b2408909643717852b95f994b273fee)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

# Precommit target
#  Check style of code.
PRECOMMIT_ARGS= \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

precommit:
	@if command -v OCamlPrecommit > /dev/null; then \
	  OCamlPrecommit $(PRECOMMIT_ARGS); \
	else \
	  echo "Skipping precommit checks.";\
	fi

precommit-full:
	OCamlPrecommit --full $(PRECOMMIT_ARGS)

test: precommit

.PHONY: precommit

# Fix perms target
#  Fix missing permission for darcs VCS files.
fix-perms:
	# TODO: chmod +x doc-dist.sh

.PHONY: fix-perms

# Headache target
#  Fix license header of file.

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name .git -prune -false \
	  -o -name .svn -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: headache

# Deploy target
#  Deploy/release the software.

deploy: headache fix-perms
	# TODO: create a plugin to create documentation.
	# oasis doc-dist
	admin-gallu-deploy --verbose \
	  --forge_upload --forge_group odn --forge_package ocaml-data-notation \
	  --forge_user gildor-admin
	# TODO: when oasis doc-dist will work, re-add.
	#  --forge_extra_file "dist/ocaml-data-notation-doc-$(shell oasis query version).tar.gz"
	# TODO: create a plugin to send announcement.
	# oasis announce
	admin-gallu-oasis-increment \
	  --setup_run --setup_args "-setup-update dynamic" --use_vcs

.PHONY: deploy
