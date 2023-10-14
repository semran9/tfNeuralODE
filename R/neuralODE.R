#' Forward pass of the Neural ODE network
#' @param model A keras neural network that defines the Neural ODE.
#' @param inputs Matrix or vector inputs to the neural network.
#' @param tsteps A vector of each time step upon which the Neural ODE is solved to get to the final solution.
#' @param return_states A boolean which dictates whether the intermediary states between the input and the final solution are returned.
#' @import tensorflow
#' @import keras
#' @return solution of the forward pass of Neural ODE
#' @export
#' @examples
#' # example code
#' \dontrun{
#' library(tensorflow)
#' library(keras)
#'
#' OdeModel(keras$Model) %py_class% {
#'  initialize <- function() {
#'    super$initialize()
#'    self$block_1 <- layer_dense(units = 50, activation = 'tanh')
#'    self$block_2 <- layer_dense(units = 2, activation = 'linear')
#'  }
#'
#'  call <- function(inputs) {
#'    x<- inputs ^ 3
#'    x <- self$block_1(x)
#'    self$block_2(x)
#'  }
#' }
#' tsteps <- seq(0, 2.5, by = 2.5/10)
#' true_y0 = t(c(2., 0.))
#' model<- OdeModel()
#' forward(model, true_y0, tsteps)
#' }
#'

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
    state <- rk4_step(func = model, state = state, dt = dt)
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
  return(tensorflow::tf$zeros_like(w))
}

#' Backward pass of the Neural ODE
#' @param model A keras neural network that defines the Neural ODE.
#' @param tsteps A vector of each time step upon which the Neural ODE is solved to get to the final solution.
#' @param outputs The tensor outputs of the forward pass of the Neural ODE.
#' @param output_gradients The tensor gradients of the loss function.
#' @import tensorflow
#' @return The model input at the last time step.
#' @return The gradient of loss with respect to the inputs for use with the Adjoint Method.
#' @return The gradients of loss the neural ODE.
#' @export
#' @examples
#' # example code
#'

backward <- function(model, tsteps, outputs, output_gradients = NULL) {
  grad_weights <- lapply(model$weights, helper_func_back)

  t0 <- tensorflow::tf$cast(tsteps[length(tsteps)], dtype = tensorflow::tf$float32)
  delta_t <- tsteps[2:(length(tsteps))] - tsteps[1:(length(tsteps)-1)]

  if (is.null(output_gradients)) {
    output_gradients <- tensorflow::tf$zeros_like(outputs)
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

#' Solve the backwards dynamics of the Neural ODE
#' @param state The current state of the differential equation
#' @param model The neural network that defines the Neural ODE.
#' @returns Returns a list of the number 1, the new backwards state of the differential equation and the gradients calculated for the network.
#' @import tensorflow

backward_dynamics<- function(state, model){
  t = state[[1]]
  ht = state[[2]]
  at = -state[[3]]
  ht_tensor = tensorflow::tf$cast((as.matrix(ht)), dtype = tensorflow::tf$float32)
  with(tensorflow::tf$GradientTape() %as% g, {
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

#' Custom RK4 solver for solving the backward pass of the Neural ODE.
#' @param backward_dynamics The backward dynamics function for the Neural ODE.
#' @param dt The time step to solve the ODE on.
#' @param state The current state of the differential equation.
#' @param model The neural network that defines the Neural ODE.

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

