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
data: inst/extdata/EXAMPLE_SPECIES.zip inst/testdata/SIMULATED_SPECIES.zip data/data/iucn_habitat_data.rda data/data/crosswalk_jung.rda

inst/testdata/SIMULATED_SPECIES.zip: inst/scripts/test-data.R
	R --slave -e "source('inst/scripts/test-data.R')"

inst/extdata/EXAMPLE_SPECIES.zip: inst/scripts/example-data.R
	R --slave -e "source('inst/scripts/example-data.R')"

data/iucn_habitat_data.rda: inst/scripts/builtin-data.R data-raw/iucn-habitat-data.csv
	R CMD BATCH --no-restore --no-save inst/scripts/builtin-data.R

data/crosswalk_jung_data.rda: inst/scripts/builtin-data.R data-raw/crosswalk-jung-data.csv
	R CMD BATCH --no-restore --no-save inst/scripts/builtin-data.R

data/crosswalk_lumbierres_data.rda: inst/scripts/builtin-data.R data-raw/crosswalk-lumbierres-data.csv
	R CMD BATCH --no-restore --no-save inst/scripts/builtin-data.R

# preprocess datasets
prep_jung_habitat_data: inst/scripts/jung-habitat-data.R inst/scripts/lumbierres-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/jung-habitat-data.R

prep_lumbierres_habitat_data: inst/scripts/jung-lumbierres-data.R inst/scripts/lumbierres-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/lumbierres-habitat-data.R

# process aoh data
aoh_global_data: aoh_amphibians aoh_mammals aoh_reptiles

aoh_amphibians:
	R CMD BATCH --no-restore --no-save '--args amphibians' inst/scripts/aoh-data.R aoh-data-amphibians.Rout

aoh_birds:
	R CMD BATCH --no-restore --no-save '--args birds' inst/scripts/aoh-data.R aoh-data-birds.Rout

aoh_mammals:
	R CMD BATCH --no-restore --no-save '--args mammals' inst/scripts/aoh-data.R aoh-data-mammals.Rout

aoh_reptiles:
	R CMD BATCH --no-restore --no-save '--args reptiles' inst/scripts/aoh-data.R aoh-data-reptiles.Rout

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
	cp -R doc inst/

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

.PHONY: initc vigns clean data docs readme site test check checkwb build  install man spellcheck examples prep_habitat_data prep_elevation_data aoh_reptiles aoh_mammals aoh_birds aoh_amphibians aoh_global_data
