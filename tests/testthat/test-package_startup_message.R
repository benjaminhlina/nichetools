
test_that("test package startup message", {
expect_message(library(nichetools),
               regexp = "version 0.1.0 ('good-boy-colt')")
expect_message(library(nichetools),
               regexp = "Have you loaded {nicheROVER}? If not, please do so.")
})
