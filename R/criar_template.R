#' Funçao para criar template com NAs para anos sem dados
#'
#' @param pop Populacao alvo da função
#' @returns template indicando anos sem coleta de dados
#' @export
#' @examples
#' criar_template()
#'

criar_template <- function(monitora, pop) {

  all_years <- tibble(ano =
                        seq(from = monitora %>%
                              filter(cnuc %in% cnuc[populacao == pop] ) %>%
                              summarize(min(ano)) %>% pull(),
                            to = monitora %>%
                              filter(cnuc %in% cnuc[populacao == pop] ) %>%
                              summarize(max(ano)) %>% pull() ))

  sampled_years <- monitora %>%
    filter(cnuc %in% cnuc[populacao == pop] ) %>%
    distinct(ano) %>%
    mutate(sampled = ano)

  y_template <- left_join(all_years, sampled_years, by="ano") %>%
    mutate(sampled = case_when(!is.na(sampled) ~ 1) ) %>%
    pull(sampled)

  assign("y_template", y_template, envir = .GlobalEnv)

}


