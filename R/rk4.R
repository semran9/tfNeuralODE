#' Runge Kutta solver for ordinary differential equations
#' @param func The function to be numerically integrated.
#' @param dt Time step.
#' @param state A list describing the state of the function, with the first element
#' being 1, and the second being a tensor that represents state
#' @returns A list containing a new time and the numerical integration of of the
#' function across the time step to the new time.
#' @examples
#' # example code
#'
#'
rk4_step<- function(func, dt, state){
  k1 <- list(1.0, func(state[[2]]))
  k2 <- list(1.0, func(euler_update(h_list = state, dh_list = k1, dt / 2)[[2]]))
  k3 = list(1.0, func(euler_update(state, k2, dt / 2)[[2]]))
  k4 = list(1.0, func(euler_update(state, k3, dt)[[2]]))
  output = list()
  for(i in 1:length(state)){
    value<- state[[i]] + dt * (k1[[i]] + 2 * k2[[i]] + 2 * k3[[i]] + k4[[i]]) / 6
    output<- c(output, value)
  }
  return(output)
}
