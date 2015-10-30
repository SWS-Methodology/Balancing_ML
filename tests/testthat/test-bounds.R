context("Check bounds are respected")

test_that("bounds are respected", {
    expect_equal(balancing(param1 = c(10, 5), param2 = c(1, 1),
                           sign = c(-1, 1), lbounds = c(10, 0)),
                 c(10, 10))
    expect_equal(balancing(param1 = c(10, 5), param2 = c(1, 1),
                           sign = c(-1, 1), ubounds = c(Inf, 5)),
                 c(5, 5))
    # Can't balance this case because of constraints
    expect_error(balancing(param1 = c(10, 5), param2 = c(1, 1), sign = c(-1, 1),
                           ubounds = c(Inf, 5), lbounds = c(10, 0)))
    expect_equal(balancing(param1 = c(10, 5), param2 = c(100, 1),
                           sign = c(-1, 1), lbounds = c(9, 0)),
                 c(9, 9))
})
