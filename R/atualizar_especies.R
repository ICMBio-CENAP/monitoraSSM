#' Atualizar a taxonomia das espécies caso sejam sinônimos
#'
#' @returns resultado do modelo de espaço de estados
#' @export
#' @author
#' Jorge Mario Herrera Lopera
#' @examples
#' atualizar_especies()
#' @import tidyverse
#' @import stringr


atualizar_especies <- function(myData) {

 #load("db_sin_aves_mammals.rda") ## base de sinonimos

  # Criar um dataframe de observações
  observations <- data.frame(
    taxon_validado = unique(myData$taxon_validado),
    status = "not found",
    stringsAsFactors = FALSE
  )

  # Atualizar as observações
  observations <- observations %>%
    mutate(
      status = case_when(
        taxon_validado %in% db_sin_aves_mammals$taxon_validado ~ "updated",
        taxon_validado %in% db_sin_aves_mammals$sinonimo ~ "modified"
      )
    )

  # Atualizar myData
  myData <- myData %>%
    mutate(
      taxon_validado = case_when(
        taxon_validado %in% db_sin_aves_mammals$taxon_validado ~ taxon_validado,
        taxon_validado %in% db_sin_aves_mammals$sinonimo ~
          db_sin_aves_mammals$taxon_validado[match(taxon_validado, db_sin_aves_mammals$sinonimo)],
        TRUE ~ taxon_validado
      )
    )

  myData$genero <- word(myData$taxon_validado, start = 1, end = 1) ## atualizar genero

  # Carregar observações no ambiente global
  assign("observations", observations, envir = .GlobalEnv)

  assign("myData", myData, envir = .GlobalEnv)

  # Retornar myData
  return(myData)
}


