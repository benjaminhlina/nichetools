utils::globalVariables(c("sample_name", "sample_number", "metric", "x", "y",
                         "d15n", "d13c"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.1.0 ('good-boy-colt').\nHave you loaded {nicheROVER}? If not, please do so.")
}

