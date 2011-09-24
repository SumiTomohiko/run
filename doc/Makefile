# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    = -q
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = .

# Internal variables.
ALLSPHINXOPTS   = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: clean html man

all: html

clean:
	-rm -rf html man

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

man:
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) $(BUILDDIR)/man
