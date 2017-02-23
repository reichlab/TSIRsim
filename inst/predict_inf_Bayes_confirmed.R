require(rjags)
require(data.table)
require(dplyr)
require(tidyr)
require(stringr)
require(MMWRweek)
require(TSIRsim)

read_arbo_data = function(filename, times) {
  arbo = fread(filename) %>%
    mutate(yr_wk = paste(case_year, wk2char(week_of_onset), sep='-')) %>%
    rename(var = case_type, value = cases) %>%
    dplyr::select(-case_year, -week_of_onset) %>%
    left_join(times)
  return(arbo)
}

sample_zikv_conf = function(arbo.file, pop, n.sim, times, time.range) {
  
  arbo.data = read_arbo_data(arbo.file, times) %>%
    spread(var, value) %>%
    arrange(date) %>%
    mutate(t = 1:n()) %>%
    filter(time.range[1] <= time & time <= time.range[2])
  
  jags.data = list(
      T = dim(arbo.data)[1],
      pop = pop, 
      obs.conf.zikv = arbo.data$confirmed_cases_zika
  )
  jags.init = list(
    lambda_Z_init = 10000,
    Z = rep(1000, jags.data$T)
  )
  this.thin = 10
  this.model = jags.model('functions/bayes_confirmed_v1.txt',
      data=jags.data, inits=jags.init,
      n.chains=1, n.adapt=1e4, quiet=F)
  p.inf.samples = coda.samples(this.model, 
      variable.names=c('p_Z', 'Z', 'p_test', 'sens',
          'samp.conf.zikv', 'lambda', 'beta', 'r'),
      n.iter=n.sim*this.thin, thin=this.thin)[[1]] %>%
    as.data.table() %>%
    mutate(sim = 1:n()) %>%
    gather(var, value, -sim) %>% 
    mutate(
      t = ifelse(str_detect(var, '\\['), 
        as.numeric(str_replace(str_replace(str_extract(var, '\\[.+\\]'), '\\[', ''), '\\]', '')), 
        NA),
      var = ifelse(str_detect(var, '\\['), 
          str_replace(str_extract(var, '.+\\['), '\\[', ''), var)) %>%
    left_join(select(arbo.data, t, yr_wk)) %>%
    left_join(times) %>%
    arrange(time)
  
  return(p.inf.samples)
}

generate_estimates_conf <- function(arbo.file, pop, n.sim, times, time.range) {
  z = sample_zikv_conf(arbo.file, pop, n.sim, times, time.range) %>%
    select(-t, -date, -yr, -wk, -time) %>% 
    filter(var %in% c('p_Z', 'Z')) %>%
    spread(var, value) %>%
    rename(p_inf.conf = p_Z, pred_zikv.conf = Z) %>%
    left_join(times)
  return(z)
}

test_sample_zikv_conf = function() {
  arbo.file='data/PR_ArboV_2017-01-23.csv'
  pop=0.5e6#3.5e6; 
  n.sim=1000; times=create.time.ref(); time.range=c(51, 102)
  
  z = sample_zikv_conf(arbo.file, pop, n.sim, times, time.range)
  
  obs.conf = read_arbo_data(arbo.file, times) %>%
    filter(var == 'confirmed_cases_zika')

  this.var = 'Z' #'samp.conf.zikv'
  this.ylim = range(filter(z, var == this.var)[['value']])
  par(mfrow=c(1, 1))
  plot(filter(z, sim == 1 & var == this.var) %>% select(date, value), 
      type='l', ylim=this.ylim, col=adjustcolor('black', 0.05))
  for (i in 2:n.sim) {
    lines(filter(z, sim == i & var == this.var) %>% select(date, value),
        col=adjustcolor('black', 0.05))
  }
#  points(select(obs.conf, date, value), col='red')
  
  filter(z, var %in% c('p_Z')) %>%
    group_by(sim, var) %>%
    summarize(total = sum(value)) %>%
    ungroup() %>%
    dplyr::select(total) %>%
    summary()

  par(mfcol=c(2, 2))
  n.breaks = 100
  hist(runif(1e5, 0.001, 0.10), xlim=c(0, 0.1), breaks=n.breaks)
  hist(filter(z, var == 'p_test')[['value']], xlim=c(0, 0.1), breaks=n.breaks)
  hist(runif(1e5, 0.85, 0.99), xlim=c(0, 1), breaks=n.breaks)
  hist(filter(z, var == 'sens')[['value']], xlim=c(0, 1), breaks=n.breaks)
  
  par(mfcol=c(2, 1))
  n.breaks = 100
  hist(filter(z, var == 'r')[['value']], breaks=n.breaks)
  hist(filter(z, var == 'beta')[['value']], breaks=n.breaks)
  
}




