library(torch)

euler_update <- function(h_list, dh_list, dt) {
  output = list()
  for(i in length(h_list)){
    h <- h_list[[i]]
    dh<- dh_list[[i]]
    output<- append(output, h + dt * dh)
  }
  return(output)
}

euler_step <- function(func, dt, state) {
  derivatives <- func(test)
  updated_state <- euler_update(state, derivatives, dt)
  return(updated_state)
}
