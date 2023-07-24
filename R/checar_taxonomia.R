#' Funçao para checar visualmente taxonomia dos dados do Monitora
#'
#' @param myData Nome do objeto. Default myData
#' @returns tabela com taxononia
#' @export
#' @examples
#' checar_taxonomia()
#' @import tidyverse

checar_taxonomia <- function(myData) {

  myData %>%
    distinct(classe, ordem, familia, genero, taxon_validado) %>%
    arrange(classe, ordem, familia, genero, taxon_validado) %>%
    print(n=Inf)

}

