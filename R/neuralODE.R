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


forward <- function(model, inputs, tsteps, return_states = FALSE) {
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
    next
  }

  outputs <- state[[2]]

  if (return_states == TRUE) {
    return(list(outputs, states))
  }

  return(outputs)
}
helper_func_back<- function(w){
  return(tf$zeros_like(w))
}

backward <- function(model, tsteps, outputs, output_gradients = NULL) {
  grad_weights <- lapply(model$weights, helper_func_back)

  t0 <- tf$cast(tsteps[length(tsteps)], dtype = tf$float32)
  delta_t <- tsteps[2:(length(tsteps))] - tsteps[1:(length(tsteps)-1)]

  if (is.null(output_gradients)) {
    output_gradients <- tf$zeros_like(outputs)
  }

  state <- c(t0, outputs, output_gradients, grad_weights)

  for (dt in rev(delta_t)) {
    state <- rk4_step_backwards(backward_dynamics, dt = -dt, state = state,
                                model = model)
  }

  inputs <- state[[2]]
  dLdInputs <- state[[3]]
  dLdWeights <- state[4:length(state)]

  return(list(inputs, dLdInputs, dLdWeights))
}

backward_dynamics<- function(state, model){
  t = state[[1]]
  ht = state[[2]]
  at = -state[[3]]
  ht_tensor = tf$cast((as.matrix(ht)), dtype = tf$float32)
  with(tf$GradientTape() %as% g, {
    g$watch(ht_tensor)
    ht_new = model(ht_tensor)
  })
  weights = model$weights
  gradients = g$gradient(
    target=ht_new, sources= c(ht_tensor, weights),
    output_gradients=at
  )

  return(c(1, ht_new, gradients))
}



rk4_step_backwards<- function(backward_dynamics, dt, state, model){
  k1 <- backward_dynamics(state, model)
  k2 <- backward_dynamics(euler_update(h_list = state,
                                                 dh_list = k1, dt / 2),
                          model)
  k3 = backward_dynamics(euler_update(state, k2, dt / 2), model)
  k4 = backward_dynamics(euler_update(state, k3, dt), model)
  output = list()
  for(i in 1:length(state)){
    value<- state[[i]] + dt *
      (k1[[i]] + 2 * k2[[i]] + 2 * k3[[i]] + k4[[i]]) / 6
    output<- c(output, value)
  }
  return(output)
}

