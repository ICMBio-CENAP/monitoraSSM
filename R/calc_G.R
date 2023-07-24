#' Funçao para estimar media geometrica
#'
#' @returns Estimativas da media geometrica das abundâncias relativas usando bootstrap
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' calc_G()
#' @import tidyverse

# funcao para estimar media geometrica
calc_G <- function(x) {

  # tabela que recebera resultados ao final do loop
  tabela_Gmean <- tibble(ano = 2014:2022,
                         G = as.numeric(NA))
  TS <- x %>% select(-populacao)

  for(sample in 1:1000) {

    TS_sample <- slice_sample(TS,
                              n = round(nrow(TS)/1.33), # cerca de 75%
                              replace = FALSE)
    tabela_G <- tibble(ano = min(est_abund$ano):max(est_abund$ano),
                       G = as.numeric(NA))

    # indice com primeiro ano de dados de cada serie
    firstYear <- rep(NA, nrow(TS_sample))

    for(i in 1:nrow(TS_sample)) {
      firstYear[i] <- which(!is.na(TS_sample[i,1:ncol(TS_sample)]))[1]
    }
    tabela_G[1,2] <- exp(mean(log(pull(TS_sample[,1])), na.rm=TRUE))

    for(i in 1:nrow(TS_sample)) {
      for(j in 2:ncol(TS_sample)) {
        tempG <- exp(mean(log(pull(TS_sample[,j])), na.rm=TRUE))
        if(firstYear[i] == j) {
          TS_sample[i,j:ncol(TS_sample)] <- tempG*TS_sample[i,j:ncol(TS_sample)]
        }
        else {}
        tabela_G[j,2] <- exp(mean(log(pull(TS_sample[,j])), na.rm=TRUE))
      }
    }

    tabela_G
    tabela_Gmean <- bind_rows(tabela_Gmean, tabela_G) %>%
      drop_na() #%>% print()
  }
  assign("tabela_Gmean", tabela_Gmean, .GlobalEnv)

}


