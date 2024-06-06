.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.2.0 ('spring-running').\nHave you loaded {nicheROVER} or {SIBER}? If not, please do so.")
}
utils::globalVariables(c("sample_name", "sample_number", "metric", "x", "y",
                         "V1", "V2", ".Random.seed",
                         "d15n", "d13c", ".",
                         "species_a", "species_b", "isotope",
                         "post_sample", "mu_est", "ellipse_name"))
