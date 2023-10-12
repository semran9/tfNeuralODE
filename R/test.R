library(keras)
library(tensorflow)

# constants
data_size = 1000
batch_time = 20 # this seems to works the best ...
niters = 3000
batch_size = 16

#model making
inputs<- layer_input(shape = c(2))
outputs<- inputs %>%

  # Adds a densely-connected layer with 64 units to the model:
  layer_dense(units = 50, activation = 'tanh') %>%

  # Add a softmax layer with 2 output units:
  layer_dense(units = 2, activation = 'linear')

model<- keras_model(inputs, outputs)

# more constants
t_grid <- seq(0, 25, by = 25/data_size)
true_y0 = c(2., 0.)
true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))

# solving a spiral ode
library(deSolve)
spiral<- lsode(true_y0, t_grid, spiral_f)
trueODEfunc<- function(du, u, p, t){
  true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
  du <- (u^3) %*% true_A
  return(list(du))
}

# solved ode output
prob_trueode <- lsode(func = trueODEfunc, y = true_y0, times = tsteps)

# plotting results
plot(prob_trueode[,2], prob_trueode[,3])

#model test
forward(model, tf$cast(true_y0, dtype = tf$float32), tsteps)

# batching function
get_batch<- function(prob_trueode, tsteps){
  starts = sample(seq(1, data_size - batch_time), size = batch_size, replace = FALSE)
  times_y0<- tsteps[starts]
  times_yN<- tsteps[starts+batch_time]
  # batch_y0 = true_y[starts] # (batch_size, 2) -> initial point
  # batch_yN = true_y[starts + batch_time]
  batch_y0 <- as.matrix(prob_trueode[starts,])
  batch_yN <- as.matrix(prob_trueode[starts + batch_time,])
  batch_y0 <- tf$cast((batch_y0), dtype = tf$float32)
  batch_yN <- tf$cast((batch_yN), dtype = tf$float32)
  return(list(batch_y0, times_y0, batch_yN, times_yN))
}

# training neural ode
for(i in 1:niters){
  inp = get_batch(prob_trueode[,2:3], tsteps)
  pred = forward(model, inputs = inp[[1]], inp[[2]])
  with(tf$GradientTape() %as% tape, {
    tape$watch(pred)
    loss = tf$reduce_mean(tf$abs(pred - inp[[3]]))
  })
  dLoss = tape$gradient(loss, pred)
  list_w = backward(model, inp[[2]], pred, output_gradients = dLoss)
  optimizer$apply_gradients(list_w[[3]], model$weights)
}
pred_y = forward(model = model, inputs = tf$cast(prob_trueode[2:3], dtype = tf$float32), tsteps = tsteps)
