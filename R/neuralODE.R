library(tensorflow)
library(keras)

forward_dynamics <- function(state, model) {
  # Used in solver _state == (time, tensor)
  return(list(1.0, model(inputs = state)))
}
f_d<- list(1.0, model(inputs = state))


append_state <- function(states, state) {
  tensors <- state[[2]]
  states <- c(states, tensors)
  return(states)
}



forward <- function(model, inputs, tsteps, return_states = FALSE) {
  # Define the forward dynamics function
  states <- list()
  inputs_tensor<- tf$cast(as.matrix(inputs), dtype = tf$float32)
  # Start building the forward computation

  t0 <- tf$cast(t[[1]], dtype = tf$float32)
  state <- list(t0, inputs_tensor)
  states<- c(states, list(state[[2]]))
  delta_t <- tsteps[2:(length(tsteps))] - tsteps[1:(length(tsteps)-1)]

  # Iterate over time intervals
  #for (t in 1:(time_len - 1)) {
  # z0 <- ode_solve(f = model, z0 = z0, t0 = tsteps[i_t], t1 = tsteps[i_t + 1])
  #  z[i_t + 1, , ] <- z0
  # }
  for (dt in delta_t) {
    state <- euler_step(f = model, state = state, dt = dt)
    states<- c(states, list(state[[2]]))
  }

  outputs <- states[[length(states)]][[2]]

  if ((return_states)) {
    return(list(outputs, states))
  } else {
    return(outputs)
  }
}
backward <- function(model, t, outputs, output_gradients = NULL) {
  grad_weights <- lapply(self$model$trainable_variables(), function(w) {
    tf$zeros_like(w)
  })

  t0 <- tf$constant(t[length(t)]) %>% tf$to_float()

  if (is.null(output_gradients)) {
    output_gradients <- tf$zeros_like(outputs)
  }

  state <- list(t0, outputs, output_gradients)
  state <- list_append(state, grad_weights)

  for (dt in rev(self$deltas_t)) {
    state <- self$solver(self$backward_dynamics, dt = -tf$to_float(dt), state = state)
  }

  inputs <- state[[2]]
  dLdInputs <- state[[3]]
  dLdWeights <- state[4:(length(state))]

  return(list(inputs, dLdInputs, dLdWeights))
}

