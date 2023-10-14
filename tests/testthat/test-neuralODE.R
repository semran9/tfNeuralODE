test_that("Euler solver and Euler update works", {
  tensor_ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.vector(du))
  }
  x<- euler_step(tensor_ode_fun,  dt = 0.25,
               state = list(1.0, (c(2., 0.))))
  expect_equal(x[[1]], 1.25)
})

test_that("Rk4 solver works", {
  tensor_ode_fun<- function(u){
    r = u ^ 3
    true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
    du <- r %*% (true_A)
    return(as.vector(du))
  }
  x<- euler_step(tensor_ode_fun,  dt = 0.25,
                 state = list(1.0, (c(2., 0.))))
  expect_equal(x[[1]], 1.25)
})
