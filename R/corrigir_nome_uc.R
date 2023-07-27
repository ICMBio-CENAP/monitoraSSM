#' Funçao para corrigir nomes das UCs
#'
#' @param myData Nome do arquivo de myData
#' @returns myData com nome de UCs corrigidos
#' @export
#' @examples
#' corrigir_nome_uc()
#' @import tidyverse

corrigir_nome_uc <- function(myData) {

  # padronizar "sp"
  myData <- myData %>%
    mutate(
      uc = case_when(
        uc == "Parna da Serra da Cutia" ~ "Parna Serra da Cutia",
        uc == "Resex do Rio Ouro Preto" ~ "Resex Rio Ouro Preto",
        TRUE ~ uc))

  assign("myData", myData, envir = .GlobalEnv)

}
