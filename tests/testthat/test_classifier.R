library(testthat)

test_that("the classifier_test works with all inputs",{
   x <- matrix(c(runif(100, 0, 1),
                 runif(100, -1, 1)),
               ncol = 2)
   y <- matrix(c(runif(100, 0, 3),
                 runif(100, -1, 1)),
               ncol = 2)
   set.seed(22)
   test <- classifier_test(x, y)
   set.seed(22)
   test2 <- classifier_test(list(x, y))
   expect_is(test, class = "list")
   expect_equal(test, test2)
   x <- matrix(c(runif(100, 0, 1),
                 runif(100, -1, 1)),
               ncol = 1)
   y <- matrix(c(runif(100, 0, 3),
                 runif(100, -1, 1)),
               ncol = 1)
   set.seed(22)
   test <- classifier_test(x, y)
   set.seed(22)
   test2 <- classifier_test(list(x, y))
   expect_is(test, class = "list")
   expect_equal(test, test2)
   x <- matrix(c(runif(200, 0, 1),
                 runif(200, -1, 1)),
               ncol = 2)
   y <- matrix(c(runif(100, 0, 3),
                 runif(100, -1, 1)),
               ncol = 2)
   set.seed(22)
   test <- classifier_test(x, y)
   set.seed(22)
   test2 <- classifier_test(list(x, y))
   expect_is(test, class = "list")
   expect_equal(test, test2)
})