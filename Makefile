all: test check
	echo "Done!"

# admin
build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "remotes::install_local('.', upgrade = 'never', force = TRUE)"

clean:
	rm -rf docs/*
	rm -rf inst/doc/*

# create built-in data
data: inst/extdata/EXAMPLE_SPECIES.zip inst/testdata/SIMULATED_SPECIES.zip builtin_data

inst/testdata/SIMULATED_SPECIES.zip: inst/scripts/test-data.R
	R --slave -e "source('inst/scripts/test-data.R')"

inst/extdata/EXAMPLE_SPECIES.zip: inst/scripts/example-data.R
	R --slave -e "source('inst/scripts/example-data.R')"

builtin_data: inst/scripts/builtin-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/builtin-data.R

# preprocess datasets
prep_jung_lvl1_habitat_data: inst/scripts/jung-lvl1-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/jung-lvl1-habitat-data.R

prep_jung_lvl2_habitat_data: inst/scripts/jung-lvl2-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/jung-lvl2-habitat-data.R

prep_jung_plvl1_habitat_data: inst/scripts/jung-plvl1-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/jung-plvl1-habitat-data.R

prep_lumbierres_habitat_data: inst/scripts/lumbierres-habitat-data.R
	R CMD BATCH --no-restore --no-save inst/scripts/lumbierres-habitat-data.R

# process aoh data
aoh_global_data: aoh_amphibians aoh_mammals aoh_reptiles aoh_birds

aoh_amphibians:
	R CMD BATCH --no-restore --no-save '--args amphibians' inst/scripts/aoh-data.R aoh-data-amphibians.Rout

aoh_birds:
	R CMD BATCH --no-restore --no-save '--args birds-part-1' inst/scripts/aoh-data.R aoh-data-birds-part-1.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-2' inst/scripts/aoh-data.R aoh-data-birds-part-2.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-3' inst/scripts/aoh-data.R aoh-data-birds-part-3.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-4' inst/scripts/aoh-data.R aoh-data-birds-part-4.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-5' inst/scripts/aoh-data.R aoh-data-birds-part-5.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-6' inst/scripts/aoh-data.R aoh-data-birds-part-6.Rout

aoh_mammals:
	R CMD BATCH --no-restore --no-save '--args mammals' inst/scripts/aoh-data.R aoh-data-mammals.Rout

aoh_reptiles:
	R CMD BATCH --no-restore --no-save '--args reptiles' inst/scripts/aoh-data.R aoh-data-reptiles.Rout

# process fraction coverage data
frc_global_data: frc_amphibians frc_mammals frc_reptiles frc_birds

frc_amphibians:
	R CMD BATCH --no-restore --no-save '--args amphibians' inst/scripts/frc-data.R frc-data-amphibians.Rout

frc_birds:
	R CMD BATCH --no-restore --no-save '--args birds-part-1' inst/scripts/frc-data.R frc-data-birds-part-1.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-2' inst/scripts/frc-data.R frc-data-birds-part-2.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-3' inst/scripts/frc-data.R frc-data-birds-part-3.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-4' inst/scripts/frc-data.R frc-data-birds-part-4.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-5' inst/scripts/frc-data.R frc-data-birds-part-5.Rout
	R CMD BATCH --no-restore --no-save '--args birds-part-6' inst/scripts/frc-data.R frc-data-birds-part-6.Rout

frc_mammals:
	R CMD BATCH --no-restore --no-save '--args mammals' inst/scripts/frc-data.R frc-data-mammals.Rout

frc_reptiles:
	R CMD BATCH --no-restore --no-save '--args reptiles' inst/scripts/frc-data.R frc-data-reptiles.Rout

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
	R --slave -e "devtools::check(remote = TRUE, build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

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
	echo "\n===== EXAMPLES =====\n" > examples.log 2>&1
	R --slave -e "devtools::run_examples(run_dontrun = TRUE, run_donttest = TRUE);warnings()" >> examples.log 2>&1
	rm -f Rplots.pdf

purl_vigns:
	R --slave -e "lapply(dir('vignettes', '^.*\\\\.Rmd$$'), function(x) knitr::purl(file.path('vignettes', x), gsub('.Rmd', '.R', x, fixed = TRUE)))"
	rm -f Rplots.pdf

.PHONY: initc vigns clean data docs readme site test check checkwb build  purl_vigns install man spellcheck examples prep_habitat_data prep_elevation_data aoh_reptiles aoh_mammals aoh_birds aoh_amphibians aoh_global_data frc_reptiles frc_mammals frc_birds frc_amphibians frc_global_data
