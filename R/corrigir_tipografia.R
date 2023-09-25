#' Corrigir a tipografia dos níveis taxonômicos
#'
#' @returns resultado do modelo de espaço de estados
#' @export
#' @author
#' Jorge Mario Herrera Lopera
#' @examples
#' corrigir_tipografia()
#' @import tidyverse
#' @import stringr


corrigir_tipografia <- function(myData) {

  # Remplazar "cf.", "CF.", "cf", "CF" por ""
  myData$taxon_validado <- gsub("\\b(CF\\.|cf\\.|CF|cf)\\s*\\b", "", myData$taxon_validado)

  # Remplazar "sp", "SP", "SP." por "sp."
  myData$taxon_validado <- gsub("\\b(SP|sp|SP\\.)\\b", "sp", myData$taxon_validado)

  ##corrigir tipografia

    myData <- myData %>%
    mutate(
      classe = ifelse(classe != "NA", str_to_title(classe), classe),
      ordem = ifelse(ordem != "NA", str_to_title(ordem), ordem),
      familia = ifelse(familia != "NA", str_to_title(familia), familia),
      genero = ifelse(genero != "NA", str_to_title(genero), genero),
      taxon_validado = ifelse(!is.na(taxon_validado),
                              str_to_sentence(taxon_validado),
                              taxon_validado),
      nivel_taxon = case_when(
        is.na(ordem) & is.na(familia) & is.na(genero) & is.na(taxon_validado) ~ "C",
        !is.na(ordem) & is.na(familia) & is.na(genero) & is.na(taxon_validado) ~ "O",
        !is.na(ordem) & !is.na(familia) & is.na(genero) & is.na(taxon_validado) ~ "F",
        !is.na(ordem) & !is.na(familia) & !is.na(genero) & is.na(taxon_validado) ~ "G",
        TRUE ~ "E"
      )
    )
  #return(myData)
  assign("myData", myData, envir = .GlobalEnv)

}
