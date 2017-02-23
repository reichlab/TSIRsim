

#' Simple SIR simulation
#'
#' @param pop integer, population
#' @param r.init initial value for R state  
#' @param z.init initial value for Z (infected) state
#' @param beta transmission parameter in model
#' @param p.obs probability of an individual in Z being observed
#' @param ntimes number of times to simulate
#'
#' @return A list with the following items
#' \itemize{
#'  \item{"obs"}{observed time series of infected Z class}
#'  \item{"pop"}{The assumed fixed population size}
#'  \item{"r.init"}{The assumed initial value for the R class}
#'  \item{"z.init"}{The assumed initial value for the Z class}
#'  \item{"beta"}{The transmission parameter}
#'  \item{"p.obs"}{The assumed parameter for reporting probability}
#' }
#' @export
#'
#' @examples
#'   sample.z = sim_tsir_simple(3.5e6, 0, 10, 1.5, 0.2)
#'   plot(sample.z$obs)
#'   sum(sample.z$obs)/sample.z$pop

sim_tsir_simple = function(pop, r.init, z.init, beta, p.obs, ntimes=52) {
  s = z = obs.z = numeric(length=ntimes)

  s[1] = pop - r.init - z.init
  z[1] = rbinom(1, s[1], beta * z.init / pop)
  for (t in 2:ntimes) {
    s[t] = s[t-1] - z[t-1]
    z[t] = rbinom(1, s[t], beta * z[t-1] / pop)
    obs.z[t] = rbinom(1, z[t], p.obs)
  }
  return(list(obs=obs.z, pop=pop, r.init=r.init, 
      z.init=z.init, beta=beta, p.obs=p.obs))
}


