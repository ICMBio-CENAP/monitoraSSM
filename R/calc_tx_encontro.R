#' Funçao para calcular taxa de encontro por visita
#'
#' @param pop Populacao alvo da função
#' @returns tibble com taxas de encontro por visita
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' calc_tx_encontro()
#' @import tidyverse

calc_tx_encontro <- function(monitora, pop) {

  esforco_visita <- monitora %>%
    filter(cnuc %in% cnuc[populacao == pop] ) %>%
    distinct(ano, data, ea, .keep_all = TRUE) %>%
    group_by(ano, data, ea) %>%
    summarize(esforco = sum(esforco, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(esforco > 0) #%>% print()

  registros_visita <- monitora %>%
    filter(populacao == pop) %>%
    group_by(ano, data, ea) %>%
    count() %>%
    mutate(n = as.numeric(n)) #%>% print()

  taxa_visita <-  esforco_visita %>%
    full_join(registros_visita, by = c("ano", "data", "ea")) %>%
    mutate(populacao = pop,
           taxa_encontro = (n/esforco)*10,
           id_amostra = paste(ea, dense_rank(data), sep = "_") ) %>%
    replace_na(list(taxa_encontro = 0)) %>%
    select(populacao, ano, ea, id_amostra, taxa_encontro )

  assign("taxa_visita", taxa_visita, envir = .GlobalEnv)

}

