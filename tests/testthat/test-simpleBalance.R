context("Check simple balances")

test_that("simple balances work as expected", {
    expect_equal(balancing(param1 = c(0, 10), param2 = c(5, 5), sign = c(-1, 1)),
                 c(5, 5))
    expect_equal(balancing(param1 = c(0, 10), param2 = c(5, 0), sign = c(-1, 1)),
                 c(10, 10))
    expect_equal(balancing(param1 = c(5, 5, 5), param2 = c(100, 100, 100),
                           sign = c(-1, -1, 1)),
                 c(5 - 5/3, 5 - 5/3, 5 + 5/3))
    expect_equal(balancing(param1 = c(5, 5, 5), param2 = c(.001, .001, .001),
                           sign = c(-1, -1, 1)),
                 c(5 - 5/3, 5 - 5/3, 5 + 5/3))
    expect_equal(balancing(param1 = c(2, 4, 6, 8, 12), param2 = c(1, 1, 1, 1, 1),
                           sign = c(-1, -1, 1, 1, 1)),
                 c(2+4, 4+4, 6-4, 8-4, 12-4))
})
