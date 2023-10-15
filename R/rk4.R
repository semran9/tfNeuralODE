#' Runge Kutta solver for ordinary differential equations
#' @param func The function to be numerically integrated.
#' @param dt Time step.
#' @param state A list describing the state of the function, with the first element
#' being 1, and the second being a tensor that represents state
#' @returns A list containing a new time and the numerical integration of of the
#' function across the time step to the new time.
#' @examples
#' # example code
#' ode_fun<- function(u){
#'   r = u ^ 3
#'   true_A = rbind(c(-0.1, 2.0), c(-2.0, -0.1))
#'   du <- r %*% (true_A)
#'   return(as.matrix(du))
#' }
#' y<- tf$cast(t(as.matrix(c(2, 0))), dtype = tf$float32)
#' x<- rk4_step(ode_fun,  dt = 0.25,
#'             state = list(1.0, y))
#' x
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
