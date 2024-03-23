utils::globalVariables(c("sample_name", "sample_number", "metric", "x", "y",
                         "d15n", "d13c", ".",
                         "species_a", "species_b", "isotope",
                         "post_sample", "mu_est", "ellipse_name"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.1.0 ('good-boy-colt').\nHave you loaded {nicheROVER}? If not, please do so.")
}

