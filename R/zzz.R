.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.2.1 ('summer-paddling').\nHave you loaded {nicheROVER} or {SIBER}? If not, please do so.")
}
utils::globalVariables(c("sample_name", "sample_number", "metric", "x", "y",
                         "V1", "V2",
                         "d15n", "d13c", ".", "community", "group",
                         "community_group", "est",
                         "species_a", "species_b", "isotope",
                         "post_sample", "mu_est", "ellipse_name"))
