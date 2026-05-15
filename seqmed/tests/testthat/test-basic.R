test_that("dummy.data generates valid model lists", {
    dd <- dummy.data()
    expect_type(dd, "list")
    expect_named(dd, c("l.par", "l.mo"))
    expect_length(dd$l.mo, 12)
    # Each element is a list of glm models
    expect_true(all(vapply(dd$l.mo[[1]], inherits, logical(1), "glm")))
})

test_that("mo.med returns estimates with CI", {
    dd <- dummy.data()
    res <- mo.med(dd$l.mo[[1]], int.boot = 20L, s.seed = 1L,
                  b.parallel = FALSE)
    expect_true(is.matrix(res))
    expect_equal(nrow(res), 3)
    expect_true("te.y" %in% colnames(res))
})

test_that("sim.med returns estimates with CI", {
    dd <- dummy.data()
    res <- sim.med(dd$l.mo[[1]], int.sims = 20L, s.seed = 1L,
                   b.parallel = FALSE)
    expect_true(is.matrix(res))
    expect_equal(nrow(res), 3)
})
