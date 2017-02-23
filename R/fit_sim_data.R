#' Fits TSIR model to simulated data
#'
#' @param data data.sim object from sim_tsir_simple(ish)
#' @param n.sim number of samples you will end up with from the MCMC
#'
#' @return not sure yet
#' @export
#'
#' @examples
fit_sim_data = function(data, n.sim) {
  require(dplyr)
  require(tidyr)
  require(rjags)
  require(stringr)
  require(TSIRsim)
  
  jags.data = list(
    T = length(sim_data$obs),
    pop = pop, 
    obs.inf = sim_data$obs
  )
  jags.init = list(
    Z = rep(1000, jags.data$T)
  )
  this.thin = 10
  this.model = jags.model('../models/core_tsir_v1.txt',
      data=jags.data, inits=jags.init,
      n.chains=1, n.adapt=1e4, quiet=F)
  p.inf.samples = coda.samples(this.model, 
      variable.names=c('p_Z', 'Z', 'S', 'p_obs',
        'samp.obs.inf', 'beta', 'r'),
      n.iter=n.sim*this.thin, thin=this.thin)[[1]] %>%
    as.data.table() %>%
    mutate(sim = 1:n()) %>%
    gather(var, value, -sim) %>% 
    mutate(
      t = ifelse(str_detect(var, '\\['), 
        as.numeric(str_replace(str_replace(str_extract(var, 
        '\\[.+\\]'), '\\[', ''), '\\]', '')), 
        NA),
      var = ifelse(str_detect(var, '\\['), 
        str_replace(str_extract(var, '.+\\['), '\\[', ''), var)) %>%
    arrange(t)
  
  return(p.inf.samples)
}
