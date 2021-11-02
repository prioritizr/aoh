all: clean initc docs test check

clean:
	rm -rf man/*
	rm -rf docs/*
	rm -rf inst/doc/*

docs: man readme vigns site

data: inst/extdata/world_behrman_1km_rast.tif inst/extdata/EXAMPLE_SPECIES.zip inst/testdata/SIMULATED_SPECIES.zip

inst/extdata/world_behrman_1km_rast.tif: inst/scripts/world-behrman-1km-rast.R
	R --slave -e "source('inst/scripts/world-behrman-1km-rast.R')"

inst/testdata/SIMULATED_SPECIES.zip: inst/scripts/test-data.R
	R --slave -e "source('inst/scripts/test-data.R')"

inst/extdata/EXAMPLE_SPECIES.zip: inst/scripts/range-data.R
	R --slave -e "source('inst/scripts/range-data.R')"

prep_habitat_data: inst/scripts/preprocess-habitat-data.R
	cd inst/scripts && R CMD BATCH --no-restore --no-save preprocess-habitat-data.R

prep_elevation_data: inst/scripts/preprocess-elevation-data.R
	cd inst/scripts && R CMD BATCH --no-restore --no-save preprocess-elevation-data.R

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

vigns:
	R --slave -e "devtools::build_vignettes()"

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

wbcheck:
	R --slave -e "devtools::check_win_devel()"

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "remotes::install_local('.', upgrade = 'never', force = TRUE)"

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

urlcheck:
	R --slave -e "urlchecker::url_check()"

examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

.PHONY: initc vigns clean data docs readme site test check checkwb build  install man spellcheck examples
