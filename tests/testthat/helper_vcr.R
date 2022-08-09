# initialize vcr
if (require(vcr)) {
  invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures"),
    filter_sensitive_data = list(
      "<<<IUCN_REDLIST_KEY>>>" = Sys.getenv("IUCN_REDLIST_KEY")
    )
  ))
  vcr::check_cassette_names()
}
