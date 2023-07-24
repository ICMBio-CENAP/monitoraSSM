#' função para selecionar UCS com serie temporal >= 5 anos
#'
#' @param monitora Nome da base de dados
#' @returns Dados filtrados
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' selecionar_uc_serie_min()
#' @import tidyverse
#' @importFrom tidyverse %>%

selecionar_uc_serie_min <- function(monitora) {

  sitesToUse <- monitora %>%
    group_by(uc) %>%
    distinct(ano) %>%
    count() %>%
    arrange(desc(n)) %>%
    filter(n>=5) %>%
    pull(uc) # %>% print()

  monitora <- monitora %>%
    filter(uc %in% sitesToUse)

  assign("monitora", monitora, envir = .GlobalEnv)

}
