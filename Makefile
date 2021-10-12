all: clean initc docs test check

clean:
	rm -rf man/*
	rm -rf docs/*
	rm -rf inst/doc/*

docs: man readme vigns site

data: inst/extdata/world_behrman_1km_rast.tif inst/extdata/EXAMPLE_SPECIES.zip inst/testdata/SIMULATED_SPECIES.zip

inst/extdata/world_behrman_1km_rast.tif:
	R --slave -e "source('inst/scripts/world-behrman-1km-rast.R')"

inst/testdata/SIMULATED_SPECIES.zip:
	R --slave -e "source('inst/scripts/test-data.R')"

inst/extdata/EXAMPLE_SPECIES.zip:
	R --slave -e "source('inst/scripts/range-data.R')"

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

vigns:
	R --slave -e "devtools::build_vignettes()"
	cp -R doc inst/

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/

wbcheck:
	R --slave -e "devtools::check_win_devel()"
	cp -R doc inst/

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

build:
	R --slave -e "devtools::build()"
	cp -R doc inst/

install:
	R --slave -e "devtools::install_local('../aoh')"

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

urlcheck:
	R --slave -e "urlchecker::url_check()"

examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

.PHONY: initc vigns clean data docs readme site test check checkwb build  install man spellcheck examples
