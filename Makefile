all: test check
	echo "Done!"

# admin
build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "remotes::install_local('.', upgrade = 'never', force = TRUE)"

clean:
	rm -rf man/*
	rm -rf docs/*
	rm -rf inst/doc/*

# create built-in data
data: inst/extdata/world_behrman_1km_rast.tif inst/extdata/EXAMPLE_SPECIES.zip inst/testdata/SIMULATED_SPECIES.zip

inst/extdata/world_behrman_1km_rast.tif: inst/scripts/world-behrman-1km-rast.R
	R --slave -e "source('inst/scripts/world-behrman-1km-rast.R')"

inst/testdata/SIMULATED_SPECIES.zip: inst/scripts/test-data.R
	R --slave -e "source('inst/scripts/test-data.R')"

inst/extdata/EXAMPLE_SPECIES.zip: inst/scripts/range-data.R
	R --slave -e "source('inst/scripts/range-data.R')"

# preprocess datasets
prep_habitat_data: inst/scripts/preprocess-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/preprocess-habitat-data.R

prep_elevation_data: inst/scripts/preprocess-elevation-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/preprocess-elevation-data.R

# process aoh data
aoh_amphibians:
	R CMD BATCH --no-restore --no-save '--args amphibians' inst/scripts/aoh-data.R

aoh_birds:
	R CMD BATCH --no-restore --no-save '--args birds' inst/scripts/aoh-data.R

aoh_mammals:
	R CMD BATCH --no-restore --no-save '--args mammals' inst/scripts/aoh-data.R

aoh_reptiles:
	R CMD BATCH --no-restore --no-save '--args reptiles' inst/scripts/aoh-data.R

# documentation
docs: man readme vigns site

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

# checks
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

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

urlcheck:
	R --slave -e "urlchecker::url_check()"

examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

.PHONY: initc vigns clean data docs readme site test check checkwb build  install man spellcheck examples prep_habitat_data prep_elevation_data aoh_reptiles aoh_mammals aoh_birds aoh_amphibians
