#' Funçao para rodar modelo SSM no jags
#'
#' @returns resultado do modelo de espaço de estados
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' rodar_ssm()
#' @import tidyverse
#' @import R2jags

rodar_ssm <- function(monitora, pop) {

  # taxas de encontro observadas
  y <- calc_tx_encontro(monitora, pop) %>%
    select(ano, ea, taxa_encontro)

  # sequencia de anos
  seq_years <- min(y$ano):max(y$ano)

  # transformar ano e ea em ranking
  y <- y %>% left_join(tibble(ano = min(y$ano):max(y$ano),
                              ano_rank = dense_rank(ano)), by = "ano") %>%
    mutate(ano = ano_rank,
           ea = dense_rank(ea)) %>%
    select(-ano_rank) #%>%  print()

  # template para remover valores imputados, se necessario
  y_template <- criar_template (monitora, pop) %>%
    as.character() %>%
    str_replace("1", "yes") %>%
    replace(is.na(.), "no")

  # definir numero de anos, sitios etc
  year <- 1:max(y$ano)
  nyears <- length(year)
  nsites <- length(unique(y$ea))
  nobs <- nrow(y)

  # juntar os dados
  jags.data <- list(y = y, nobs = nobs,
                    nsites = nsites,
                    year = year, nyears = nyears)


  # Parametros a monitorar
  parameters <- c("N_est", "r_mean", "cv")


  # definicoes MCMC
  ni <- 25000
  nt <- 50
  nb <- 5000
  nc <- 3

  # especificar modelo na linguagem JAGS
  sink(here("ssm.jags"))
  cat("

  model {

  ## Likelihood

  # modelo de observacao
  # cada observacao resulta do tamanho da populacao + ruido aleatorio
  for(i in 1:nobs) {
    y[i,3] ~ dnorm(exp(logN_est[year[y[i,1]], y[i,2]]), tau_obs)
    }

  # modelo de estado
  # cada populacao resulta da populacao no ano anterior * taxa de
  # crescimento + flutuacoes estocasticas
  for (year in 1:(nyears-1)){
  for(j in 1:nsites) {
    #r[year,j] ~ dnorm(0, tau_proc)
    r[year,j] ~ dnorm(mean_r, tau_proc)
    logN_est[year+1,j] <- logN_est[year,j] + r[year,j]
    }
  }

  # priors
  #for(j in 1:nsites) {
  #logN_est[1,j] ~ dunif(0, 2)  # populacao inicial
  #mean_r[j] ~ dnorm(1, 0.001)  # taxa de crescimento medio
  #}

  # prior alternativo para populacao inicial:
  # para cada trilha, N = N_medio_UC + variacao estocastica
  # para cada trilha, r = r_medio + variacao estocastica (random walk)
  for(j in 1:nsites) {
    eps_pop_site[j] ~ dnorm(0, tau_site) # variacao estocastica no N entre trilhas
    logN_est[1,j] <- logN_media_UC_ano_1 + eps_pop_site[j] # populacao inicial de cada trilha
  }
  mean_r ~ dnorm(1, 0.001)
  logN_media_UC_ano_1 ~ dnorm(1.5, 0.001)
  tau_site <- 1/(sd_eps_site*sd_eps_site)
  sd_eps_site ~ dunif(0,3)

  sigma_proc ~ dunif(0, 1) # sd do processo de estado
  tau_proc <- pow(sigma_proc, -2)

  sigma_obs ~ dunif(0, 1)  # sd do processo de observacao
  tau_obs <- pow(sigma_obs, -2)

  # parametros derivados
  for(i in 1:nyears) {
    N_est[i] <- exp(mean(logN_est[i,])) # populacao a cada ano (media das trilhas)
  }
  #r_mean <- mean(r) # r media geral
  r_mean <- mean_r

  cv <- sd(N_est)/mean(N_est)

  # avaliar ajuste do modelo usando discrepancia do Chi-quadrado

  # computar estatistica de ajuste E para os dados observados
  #eval[i,j] <- logN_est[i,j]   	# valores esperados
  #E[i,j] <- pow((y[i,j] - eval[i,t]),2) / (eval[i,t] + 0.5)

  # gerar dados replicados e computar estatistica de ajuste para eles
  #y.new[i,t] ~ dbin(p[i,t], k[i,t])
  #E.new[i,t] <- pow((y.new[i,t] - eval[i,t]),2) / (eval[i,t] + 0.5)

  }
  ",fill = TRUE)
  sink()

  # chamar o JAGS a partir do R
  ssm <- jags(jags.data, inits=NULL,
              parameters, here("ssm.jags"),
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

  # checar resultados
  #print(ssm, digits = 2)

  ssm_results_temp <- tibble(populacao = pop,
                             r_mean025 = quantile(ssm$BUGSoutput$sims.list$r_mean, probs = 0.025),
                             r_mean10 = quantile(ssm$BUGSoutput$sims.list$r_mean, probs = 0.1),
                             r_mean50 = quantile(ssm$BUGSoutput$sims.list$r_mean, probs = 0.5),
                             r_mean90 = quantile(ssm$BUGSoutput$sims.list$r_mean, probs = 0.9),
                             r_mean975 = quantile(ssm$BUGSoutput$sims.list$r_mean, probs = 0.975),
                             prob_aumentando =  length(which(ssm$BUGSoutput$sims.list$r_mean > 0))/nrow(ssm$BUGSoutput$sims.list$r_mean),
                             prob_diminuindo =  length(which(ssm$BUGSoutput$sims.list$r_mean < 0))/nrow(ssm$BUGSoutput$sims.list$r_mean),
                             cv_lower = quantile(ssm$BUGSoutput$sims.list$cv, probs = 0.025),
                             cv_mean = quantile(ssm$BUGSoutput$sims.list$cv, probs = 0.5),
                             cv_upper = quantile(ssm$BUGSoutput$sims.list$cv, probs = 0.975),
                             rhat_medio = mean(ssm$BUGSoutput$summary[,"Rhat"]),
                             rhat_max = max(ssm$BUGSoutput$summary[,"Rhat"]) )

  est_abund_temp <- tibble(populacao = pop,
                           ano = as.numeric(seq_years),
                           sampled = as.character(y_template),
                           lower025 = apply(ssm$BUGSoutput$sims.list$N_est, 2, quantile, probs = 0.1),
                           lower10 = quantile(ssm$BUGSoutput$sims.list$N_est, probs = 0.1),
                           mediana = apply(ssm$BUGSoutput$sims.list$N_est, 2, quantile, probs = 0.5),
                           upper90 = quantile(ssm$BUGSoutput$sims.list$N_est, probs = 0.9),
                           upper975 = apply(ssm$BUGSoutput$sims.list$N_est, 2, quantile, probs = 0.975))
  #est_abund_temp[which(est_abund_temp$sampled == "no"), 2:ncol(est_abund_temp)] <- NA

  # assign
  assign("ssm", ssm, envir = .GlobalEnv)
  assign("ssm_results_temp", ssm_results_temp, envir = .GlobalEnv)
  assign("est_abund_temp", est_abund_temp, envir = .GlobalEnv)

}
