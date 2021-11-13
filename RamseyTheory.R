main <- function(){
  set_init_state() -> init_state
  set_param(init_state) -> param
  calculate_con_kap(param, init_state) -> con_kap
  plot_phase_diagram(con_kap)
  return(con_kap)
}

set_init_state <- function(){
  init_Yield <- rnorm(n=1, mean=420, sd=70)
  init_Consumption <- rnorm(n=1, mean=300, sd=50)
  init_Kapital <- rnorm(n=1, mean=3000, sd=500)
  init_wage <- rnorm(n=1, mean=300, sd=50)
  init_rental <- (init_Yield - init_wage) / init_Kapital
  init_Aggression <- 1
  
  init_state <- c(init_Yield, init_Consumption, init_Kapital, init_wage, init_rental, init_Aggression)
  return(init_state)
}

set_param <- function(init_state){
  alpha <- 1 - init_state[4] / init_state[1]
  beta <- 1 / (1 + init_state[5])
  delta <- 0.25
  Labor <- ((init_state[3] ^ -alpha) * init_state[1] / init_state[6]) ^ (1 / (1 - alpha))
  Time <- 20
  
  param <- c(alpha, beta, delta, Labor, Time)
  return(param)
}


calculate_con_kap <- function(param, init_state){
  Yield <- rep(0,param[5])
  Consumption <- rep(0, param[5])
  Kapital <- rep(0, param[5])
  Time <- 1:param[5]
  
  Yield[1] <- init_state[1]
  Consumption[1] <- init_state[2]
  Kapital[1] <- init_state[3]
  
  for (i in 2:param[5]) {
    Yield[i] <- init_state[6] * Yield[i-1] ^ param[1] * param[4] ^ (1 - param[1])
    Consumption[i] <- Consumption[i-1] * param[2] *(init_state[5] - param[3] + 1)
    Kapital[i] <- Yield[i-1] + (1 - param[3]) * Kapital[i-1] - Consumption[i-1]
  }
  
  Y <- data.frame(Time, Yield, Consumption, Kapital)
  return(Y)
}

plot_phase_diagram <- function(con_kap){
  plot(Consumption ~ Kapital, data = con_kap, type = "o")
}

main()
