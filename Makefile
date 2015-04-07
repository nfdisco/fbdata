SHELL 	:= /bin/sh
PACKAGE	:= fbdata
doc:
	Rscript -e \
	"require(roxygen2); require(methods); roxygen2::roxygenise()"
	R CMD Rd2pdf --force --batch --no-preview --internals \
		-o ./inst/doc/$(PACKAGE).pdf .

check:
	cd ..; R CMD check -o /tmp $(PACKAGE)

install:
	R CMD INSTALL .

uninstall:
	R CMD REMOVE $(PACKAGE)

.PHONY: doc check install uninstall
