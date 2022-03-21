test_that("test qLBPG", {
  Qx<-qLBPG(0.5,1.5,2.5)
  expect_equal(Qx,qLBPG(0.5,1.5,2.5))
})
