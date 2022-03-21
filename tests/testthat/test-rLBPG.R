test_that("test rLBPG", {
  x<-rLBPG(50,1.5,1)
  y<-x
  expect_equal(x,y)
})
