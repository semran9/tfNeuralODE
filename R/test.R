data_size = 1000
batch_time = 20 # this seems to works the best ...
niters = 3000
batch_size = 16
library(keras)
model <- keras_model_sequential()
library(tensorflow)
model %>%

  # Adds a densely-connected layer with 64 units to the model:
  layer_dense(units = 2, activation = 'relu') %>%

  # Add another:
  layer_dense(units = 64, activation = 'relu') %>%

  # Add a softmax layer with 10 output units:
  layer_dense(units = 2, activation = 'linear')


t_grid <- seq(0, 25, by = 25/data_size)
true_y0 = c(2., 0.)
true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
forward(true_y0, )
library(deSolve)
spiral<- lsode(true_y0, t_grid, spiral_f)
trueODEfunc<- function(du, u, p, t){
  true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
  du <- (u^3) %*% true_A
  return(list(du))
}
prob_trueode <- lsode(func = trueODEfunc, y = true_y0, times = tsteps)

plot(prob_trueode[,2], prob_trueode[,3])

forward(model, true_y0, tsteps)
