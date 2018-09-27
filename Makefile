# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: docs check clean

docs:
	R -q -e 'library("roxygen2"); roxygenise(".")'

build: docs
	cd ..;\
	R CMD build MRFtools

check: build
	cd ..;\
	R CMD check MRFtools_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran MRFtools_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL MRFtools_$(PKGVERS).tar.gz

move: check
	cp ../gratia.Rcheck/MRFtools-Ex.Rout ./tests/Examples/MRFtools-Ex.Rout.save

clean:
	cd ..;\
	rm -r MRFtools.Rcheck/
