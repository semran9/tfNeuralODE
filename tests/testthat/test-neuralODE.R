skip_if_no_tf <- function() {
  have_tf <- reticulate::py_module_available("tensorflow")
  if (!have_tf)
    skip("tensorflow not available for testing")
}
test_that("Euler solver and Euler update works", {
  skip_if_no_tf()
  ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.matrix(du))
  }
  y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
  x<- euler_step(ode_fun,  dt = 0.25,
                 state = list(1.0, y))
  expect_equal(as.numeric(x[[2]][,1]), 1.8, tolerance = 0.01)
})

test_that("Rk4 solver works", {
  skip_if_no_tf()
  ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.matrix(du))
  }
  y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
  x<- rk4_step(ode_fun,  dt = 0.25,
                 state = list(1.0, y))
  expect_equal(as.numeric(x[[2]][,1]), -0.125438, tolerance = 0.01)
})

test_that("Forward works", {
  skip_if_no_tf()
  ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.matrix(du))
  }
  y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
  x<- forward(ode_fun, inputs =  y,  tsteps = c(0, 0.25))
  expect_equal(as.numeric(x[1,][1]), -0.125438, tolerance = 0.01)
})
test_that("Backward works", {
  skip_if_no_tf()
  ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.matrix(du))
  }
  y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
  x<- forward(ode_fun, inputs =  y,  tsteps = c(0, 0.25))
  expect_equal(as.numeric(x[1,][1]), -0.125438, tolerance = 0.01)
})

