test_that("test pLBPG", {
  Fx<-pLBPG(0.5,1.5,2.5)
  expect_equal(Fx,pLBPG(0.5,1.5,2.5))
})
