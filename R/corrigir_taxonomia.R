#' Funçao para limpar dados do Monitora
#'
#' @param myData Nome do arquivo de myData
#' @returns myData corrigidos e limpos
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' corrigir_taxonomia()
#' @import tidyverse

corrigir_taxonomia <- function(myData) {

  myData <- myData %>%
    # classe
    mutate(
      classe = case_when(
        is.na(classe) & ordem == "Artiodactyla" ~ "Mammalia",
        TRUE ~ classe)) %>%
    # familia
    mutate(
      familia = case_when(
        is.na(familia) & taxon_validado == "Cracidae" ~ "Cracidae",
        is.na(familia) & taxon_validado == "Tinamidae" ~ "Tinamidae",
        is.na(familia) & taxon_validado == "Felidae" ~ "Felidae",
        is.na(familia) & taxon_validado == "Procyonidae" ~ "Procyonidae",
        is.na(familia) & taxon_validado == "Dasypodidae" ~ "Dasypodidae",
        is.na(familia) & taxon_validado == "Atelidae" ~ "Atelidae",
        is.na(familia) & taxon_validado == "Callitrichidae" ~ "Callitrichidae",
        is.na(familia) & taxon_validado == "Cebidae" ~ "Cebidae",
        is.na(familia) & taxon_validado == "Pitheciidae" ~ "Pitheciidae",
        is.na(familia) & taxon_validado == "Ptheciidae" ~ "Pitheciidae",
        is.na(familia) & taxon_validado == "Dsyprocta" ~ "Dasyproctidae",
        is.na(familia) & taxon_validado == "Sciuridae" ~ "Sciuridae",
        is.na(familia) & taxon_validado == "Cervidae" ~ "Cervidae",
        is.na(familia) & taxon_validado == "Tayassuidae" ~ "Tayassuidae",
        TRUE ~ familia)) %>%
    # genero
    mutate(
      genero = case_when(
        genero == "Cracidae" ~ NA,
        genero == "Tinamidae" ~ NA,
        genero == "Felidae" ~ NA,
        genero == "Procyonidae" ~ NA,
        genero == "Dasypodidae" ~ NA,
        genero == "Atelidae" ~ NA,
        genero == "Callitrichidae" ~ NA,
        genero == "Cebidae" ~ NA,
        genero == "Pitheciidae" ~ NA,
        genero == "Primates" ~ NA,
        genero == "Ptheciidae" ~ NA,
        genero == "Rodentia" ~ NA,
        genero == "Sciuridae" ~ NA,
        genero == "Artiodactyla" ~ NA,
        genero == "Cervidae" ~ NA,
        genero == "Tayassuidae" ~ NA,
        TRUE ~ genero)) %>%
    # taxon_validado
    mutate(
      taxon_validado = case_when(
        taxon_validado == "Cracidae" ~ NA,
        taxon_validado == "Tinamidae" ~ NA,
        taxon_validado == "Felidae" ~ NA,
        taxon_validado == "Procyonidae" ~ NA,
        taxon_validado == "Dasypodidae" ~ NA,
        taxon_validado == "Procyonidae" ~ NA,
        taxon_validado == "Atelidae" ~ NA,
        taxon_validado == "Callitrichidae" ~ NA,
        taxon_validado == "Cebidae" ~ NA,
        taxon_validado == "Pitheciidae" ~ NA,
        taxon_validado == "Ptheciidae" ~ NA,
        taxon_validado == "Primates" ~ NA,
        taxon_validado == "Rodentia" ~ NA,
        taxon_validado == "Sciuridae" ~ NA,
        taxon_validado == "Artiodactyla" ~ NA,
        taxon_validado == "Cervidae" ~ NA,
        taxon_validado == "Tayassuidae" ~ NA,
        taxon_validado == "Dsyprocta fuliginosa" ~ "Dasyprocta fuliginosa",
        TRUE ~ taxon_validado)) %>%
    # nivel_taxon
    mutate(
      nivel_taxon = case_when(
        is.na(taxon_validado) & is.na(genero) & is.na(familia) ~ "O",
        is.na(taxon_validado) & is.na(genero) ~ "F",
        is.na(taxon_validado)  ~ "G",
        TRUE ~ "E"))

  # atualizar coluna populacao
  myData <- myData %>%
    mutate(populacao = paste(taxon_validado, cnuc, sep ="_"),
           populacao = str_replace(populacao, " ", "_")) %>%
    mutate(populacao = case_when(
      is.na(taxon_validado) ~ NA,
      TRUE ~ populacao))

  assign("myData", myData, envir = .GlobalEnv)

}

