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
update = rk4_step(model,  dt = 0.025, state = list(tf$cast(1.0, dtype = tf$float32), tf$cast((as.matrix(true_y0)), dtype = tf$float32)))
z = euler_update(list(tf$cast(0.0, dtype = tf$float32), tf$cast(t(as.matrix(true_y0)), dtype = tf$float32)), z, 0.025 / 2)
z = tensor_ode_fun(list(tf$cast(0.0, dtype = tf$float32), tf$cast(t(as.matrix(true_y0)), dtype = tf$float32)))
z
