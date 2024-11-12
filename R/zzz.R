.onAttach <- function(libname, pkgname) {
  packageStartupMessage("version 0.3.3 ('let-the-snow-fly').\nHave you loaded {nicheROVER} or {SIBER}? If not, please do so.")
}
utils::globalVariables(c("sample_name", "sample_number", "metric", "x", "y",
                         "V1", "V2",
                         "d15n", "d13c", ".", "community", "group",
                         "community_group", "est",
                         "c_1", "c_2", "cg_1", "cg_2", "compare",
                         "g_1", "g_2", "compare_1", "compare_2",
                         "community_name", "group_name",
                         "area.1", "area.2", "area1", "area2",
                         "overlap", "area_1", "area_2",
                         "niche_overlap_perc", "niche_overlap",
                         "species_a", "species_b", "isotope",
                         "post_sample", "mu_est", "ellipse_name",
                         "iso_a", "iso_ab", "iso_b", "iso_combos"
                         ))
