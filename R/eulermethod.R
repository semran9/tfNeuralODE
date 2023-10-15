#' A Euler method state updater.
#' @param h_list The initial state of the ODE.
#' @param dh_list description
#' @param dt The time step to update the initial state with.
#' @returns The updated state of the ODE.
#' @keywords internal
euler_update <- function(h_list, dh_list, dt) {
  output = list()
  for(i in 1:length(h_list)){
    h <- h_list[[i]]
    dh<- dh_list[[i]]
    output<- c(output, h + (dt * dh))
  }
  return(output)
}
#' A function to employ the Euler Method to solve an ODE.
#' @param func The derivative function.
#' @param dt The time step for the Euler solver.
#' @param state A list that defines the current state of the ODE, with one entry
#' being a number, and the other being a tensor which describes the function state.
#' @returns A list that describes the updated state of the ODE.
#' @keywords internal
euler_step <- function(func, dt, state) {
  dh <- list(1.0, func(state[[2]]))
  updated_state <- euler_update(state, dh, dt)
  return((updated_state))
}

# update = euler_step(tensor_ode_fun,  dt = 0.025, state = list(tf$cast(1.0, dtype = tf$float32), tf$cast(t(as.matrix(true_y0)), dtype = tf$float32)))
# tensor_ode_fun<- function(u){
#   r = u ^ 3
#   r = tf$cast((r), dtype = tf$float32)
#   true_A = tf$cast(rbind(c(-0.1, 2.0), c(-2.0, -0.1)), dtype = tf$float32)
#   du <- tf$matmul(r, true_A)
#   return(du)
# }
# tensor_ode_fun(list(tf$cast(0.0, dtype = tf$float32), tf$cast(t(as.matrix(true_y0)), dtype = tf$float32)))

# forward(tensor_ode_fun, tf$cast(t(as.matrix(true_y0)), dtype = tf$float32), tsteps)
