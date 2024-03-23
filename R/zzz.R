
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  assign(".sample.name", "sample_name", envir = .pkgglobalenv)
  assign(".sample.number", "sample_number", envir = .pkgglobalenv)
  assign(".x_x", "x", envir = .pkgglobalenv)
  assign(".y_y", "y", envir = .pkgglobalenv)
  # assign(".sample.name", "sample_name", envir = .pkgglobalenv)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.1.0 ('good-boy-colt').\nHave you loaded {nicheROVER}? If not, please do so.")
}

