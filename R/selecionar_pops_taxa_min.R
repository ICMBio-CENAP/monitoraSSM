#' funcao para selecionar populacoes com taxa de encontro medio > valor especificado
#'
#' @param taxa_min Taxa mínima aceitável. Default = 0.5
#' @returns Dados filtrados
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' selecionar_pops_taxa_min()
#' @import tidyverse

# funcao para selecionar populacoes com taxa de encontro medio > 0.5
selecionar_pops_taxa_min <- function(monitora, taxa_min = 0.5) {

  esforco <- monitora %>%
    distinct(uc, ea, data, esforco) %>%
    group_by(uc) %>%
    summarize(esforco = sum(esforco, na.rm = TRUE))

  encontros <- monitora %>%
    group_by(uc, populacao) %>%
    count() %>%
    mutate(n = as.numeric(n))

  popsToUse <-  esforco %>%
    full_join(encontros, by = "uc") %>%
    mutate(taxa_encontro = (n/esforco)*10) %>%
    replace_na(list(taxa_encontro = 0)) %>%
    select(uc, populacao, taxa_encontro) %>%
    filter(taxa_encontro >= taxa_min) %>%
    drop_na() #%>% pull(populacao)

  assign("popsToUse", popsToUse, envir = .GlobalEnv)

}

