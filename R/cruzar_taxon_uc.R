#' Funçao para limpar dados do Monitora
#'
#' @param myData Nome do arquivo de myData
#' @returns myData corrigidos e limpos
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' cruzar_taxon_uc()
#' @import tidyverse
#' @importFrom tidyverse %>%

cruzar_taxon_uc <- function(myData) {

    myData <- myData %>%
      dplyr::mutate(
        taxon_validado = case_when(
          uc == "Esec Niquiá" & taxon_validado == "Chiropotes sagulatus" ~ "Chiropotes chiropotes",
          uc == "Esec da Terra do Meio/Resex Iriri" & taxon_validado == "Mico sp" ~ "Mico emiliae",
          uc == "Parna da Amazônia" & taxon_validado == "Ateles chamek" ~ "Ateles marginatus",
          uc == "Parna da Amazônia" & taxon_validado == "Callicebus baptista" ~ "Callicebus sp",
          uc == "Parna da Amazônia" & taxon_validado == "Callicebus hoffmannsi" ~ "Callicebus sp",
          uc == "Parna da Amazônia" & taxon_validado == "Mico humeralifer" ~ "Mico sp",
          uc == "Parna da Serra da Mocidade" & taxon_validado == "Chiropotes chiropotes" ~ "Chiropotes sagulatus",
          uc == "Parna do Jaú" & taxon_validado == "Pauxi tuberosa" ~ "Pauxi tomentosa",
          uc == "Parna do Juruena" & taxon_validado == "Alouatta sp" ~ "Alouatta puruensis",
          uc == "Parna do Juruena" & taxon_validado == "Callicebus cinerascens" ~ "Callicebus sp",
          uc == "Parna do Juruena" & taxon_validado == "Callicebus moloch" ~ "Callicebus sp",
          uc == "Parna do Juruena" & taxon_validado == "Psophia dextralis" ~ "Psophia sp",
          uc == "Parna do Juruena" & taxon_validado == "Psophia viridis" ~ "Psophia sp",
          uc == "Rebio do Jaru" & taxon_validado == "Callicebus brunneus" ~ "Callicebus bernhardi",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Alouatta sp" ~ "Alouatta juara",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Cebus albifrons" ~ "Cebus unicolo",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Resex Tapajós-Arapiuns" & taxon_validado == "Callicebus sp" ~ "Callicebus hoffmannsi",
          uc == "Resex Tapajós-Arapiuns" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta croconota",
          taxon_validado == "Tinamus major" ~ "Tinamus sp",
          TRUE ~ taxon_validado))

    # atualizar coluna populacao
    myData <- myData %>%
      mutate(populacao = paste(taxon_validado, cnuc, sep ="_"),
             populacao = str_replace(populacao, " ", "_")) %>%
      mutate(populacao = case_when(
        is.na(taxon_validado) ~ NA,
        TRUE ~ populacao))

  assign("myData", myData, envir = .GlobalEnv)

}

