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

  W = tf$cast(rbind(c(-0.1, 1.0), c(-0.2, -0.1)), dtype = tf$float32)
  optimizer = tf$keras$optimizers$legacy$SGD(learning_rate=1e-2, momentum=0.95)

  OdeModel(keras$Model) %py_class% {
    call <- function(inputs) {
      tf$matmul(inputs, W)
    }
  }
  model<- OdeModel()
  y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
  N_target = tf$cast(t(c(0., 0.5)), dtype = tf$float32)
  pred<- forward(model, inputs =  y,  tsteps = c(0, 0.25))
  with(tf$GradientTape() %as% tape, {
    tape$watch(pred)
    loss = tf$reduce_sum((N_target - pred) ^ 2)
  })
  dLoss = tape$gradient(loss, pred)
  list_w = backward(model, c(0, 0.25), pred, output_gradients = dLoss)
  expect_equal(as.numeric(list_w[[1]][,1]), 2, tolerance = 0.05)
})

