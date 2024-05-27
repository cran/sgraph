
test_sigma_from_igraph = function() {
    sig <- sigma_from_igraph(igraph = lesMis)
    expect_true(TRUE)
}
test_that('sigma_from_igraph', test_sigma_from_igraph())
