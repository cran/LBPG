test_that("test dLBPG", {
  fx<-dLBPG(5.7,1.5,2.5)
  expect_equal(fx,dLBPG(5.7,1.5,2.5))
})
