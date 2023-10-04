library(tensorflow)
library(keras)

# forward_dynamics <- function(state, model) {
#   # Used in solver _state == (time, tensor)
#   return(list(1.0, model(inputs = state)))
# }
# f_d<- list(1.0, model(inputs = state))
#
#
# append_state <- function(states, state) {
#   tensors <- state[[2]]
#   states <- c(states, tensors)
#   return(states)
# }


forward <- function(model, inputs, tsteps, return_states = TRUE) {
  # Define the forward dynamics function
  states <- list()
  inputs_tensor<- tf$cast(as.matrix(inputs), dtype = tf$float32)
  # Start building the forward computation

  t0 <- tf$cast(tsteps[1], dtype = tf$float32)
  state <- list(t0, inputs_tensor)
  states<- c(states, list(state[[2]]))
  delta_t <- tsteps[2:(length(tsteps))] - tsteps[1:(length(tsteps)-1)]

  # Iterate over time intervals
  for (dt in delta_t) {
    state <- rk4_step(f = model, state = state, dt = dt)
    states<- c(states, list(state[[2]]))
  }

  outputs <- state[[2]]

  if ((return_states)) {
    return(list(outputs, states))
  } else {
    return(outputs)
  }
}
helper_func_back<- function(w){
  return(tf$zeros_like(w))
}

backward <- function(model, t, outputs, output_gradients = NULL) {
  grad_weights <- lapply(model$trainable_variables, helper_func_back)

  t0 <- tf$cast(tsteps[length(tsteps)], dtype = tf$float32)
  delta_t <- tsteps[2:(length(tsteps))] - tsteps[1:(length(tsteps)-1)]

  if (is.null(output_gradients)) {
    output_gradients <- tf$zeros_like(outputs)
  }

  state <- list(t0, outputs, output_gradients)
  state <- list_append(state, grad_weights)

  for (dt in rev(delta_t)) {
    state <- rk4_step(backward_dynamics, dt = -dt, state = state)
  }

  inputs <- state[[2]]
  dLdInputs <- state[[3]]
  dLdWeights <- state[4:(length(state))]

  return(list(inputs, dLdInputs, dLdWeights))
}

backward_dynamics<- function(model, state){
  t = state[1]
  ht = state[2]
  at = -state[3]
  g = tf$GradientTape
  g.watch(ht)
  ht_new = model(inputs=list(t, ht))

  gradients = g$gradient(
    target=ht_new, sources=list(ht) + model$weights,
    output_gradients=at
  )

  return(list(1.0, ht_new, gradients))
}

plot(prob_trueode[,2], prob_trueode[,3])
optimizer = optimizer_adam(learning_rate = 1e-4)
