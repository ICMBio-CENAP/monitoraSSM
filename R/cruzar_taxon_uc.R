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

cruzar_taxon_uc <- function(myData) {

    myData <- myData %>%
      dplyr::mutate(
        taxon_validado = case_when(
          uc == "Esec de Niquiá" & taxon_validado == "Chiropotes sagulatus" ~ "Chiropotes chiropotes",
          uc == "Esec do Rio Acre" & taxon_validado == "Alouatta seniculus" ~ "Alouatta puruensis",
          uc == "Esec da Terra do Meio/Resex Iriri" & taxon_validado == "Mico sp" ~ "Mico emiliae",
          uc == "Flona do Tapajós" & taxon_validado == "Mico argentatus" ~ "Mico spp",
          uc == "Flona do Tapajós" & taxon_validado == "Mico sp" ~ "Mico spp",
          uc == "Flona do Tapajós" & taxon_validado == "Saimiri sp" ~ "Saimiri ustus",
          #uc == "Parna da Amazônia" & familia == "Sciuridae" ~ NA,
          uc == "Parna da Amazônia" & familia == "Sciuridae" ~ "Sciuridae",
          uc == "Parna da Amazônia" & taxon_validado == "Ateles marginatus" ~ "Ateles chamek",
          uc == "Parna da Amazônia" & taxon_validado == "Callicebus baptista" ~ "Callicebus spp",
          uc == "Parna da Amazônia" & taxon_validado == "Callicebus hoffmannsi" ~ "Callicebus spp",
          uc == "Parna da Amazônia" & taxon_validado == "Callicebus sp" ~ "Callicebus spp",
          uc == "Parna da Amazônia" & taxon_validado == "Mico sp" ~ "Mico humeralifer",
          uc == "Parna da Serra do Pardo" & taxon_validado == "Mico sp" ~ "Mico emiliae",
          uc == "Parna de Pacaás Novos" & taxon_validado == "Alouatta seniculus" ~ "Alouatta puruensis",
          uc == "Parna de Pacaás Novos" & taxon_validado == "Dasyprocta prymnolopha" ~ "Dasyprocta fuliginosa",
          uc == "Parna de Pacaás Novos" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Parna de Pacaás Novos" & taxon_validado == "Urosciurus sp" ~ "Urosciurus spadiceus",
          uc == "Parna do Cabo Orange" & taxon_validado == "Bradypus variegatus" ~ "Bradypus tridactylus",
          uc == "Parna do Jaú" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          #uc == "Parna do Jaú" & taxon_validado == "Pauxi tuberosa" ~ "Pauxi tomentosa",
          uc == "Parna do Juruena" & taxon_validado == "Alouatta sp" ~ "Alouatta puruensis",
          uc == "Parna do Juruena" & taxon_validado == "Callicebus cinerascens" ~ "Callicebus spp",
          uc == "Parna do Juruena" & taxon_validado == "Callicebus moloch" ~ "Callicebus spp",
          uc == "Parna do Juruena" & taxon_validado == "Callicebus sp" ~ "Callicebus spp",
          uc == "Parna do Juruena" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          #uc == "Parna do Juruena" & taxon_validado == "Psophia dextralis" ~ "Psophia sp",
          #uc == "Parna do Juruena" & taxon_validado == "Psophia viridis" ~ "Psophia sp",
          uc == "Parna do Juruena" & taxon_validado == "Urosciurus sp" ~ "Urosciurus spadiceus",
          #uc == "Parna do Viruá" & familia == "Sciuridae" ~ NA,
          uc == "Parna do Viruá" & familia == "Sciuridae" ~ "Sciuridae",
          uc == "Parna dos Campos Amazonicos" & taxon_validado == "Callicebus sp" ~ "Callicebus bernhardi",
          uc == "Parna Nascentes do Lago Jari" & taxon_validado == "Callicebus dubius" ~ "Callicebus caligatus",
          uc == "Parna Serra da Cutia" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Parna Serra da Mocidade" & taxon_validado == "Chiropotes sagulatus" ~ "Chiropotes chiropotes",
          uc == "Parna Serra da Mocidade" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Parna Serra da Mocidade" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Rebio do Jaru" & taxon_validado == "Callicebus bernhardi" ~ "Callicebus spp",
          uc == "Rebio do Jaru" & taxon_validado == "Callicebus brunneus" ~ "Callicebus spp",
          #uc == "Rebio do Jaru" & familia == "Sciuridae" ~ NA,
          #uc == "Rebio do Tapirapé" & familia == "Sciuridae" ~ NA,
          uc == "Rebio do Jaru" & familia == "Sciuridae" ~ "Sciuridae",
          uc == "Rebio do Tapirapé" & familia == "Sciuridae" ~ "Sciuridae",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Alouatta juara" ~ "Alouatta sp",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Cebus albifrons" ~ "Cebus unicolor",
          uc == "Resex Alto Tarauacá" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Resex Chico Mendes" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Resex Ipaú-Anilzinho" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Resex Ipaú-Anilzinho" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Resex Ipaú-Anilzinho" & taxon_validado == "Guerlinguetus sp" ~ "Guerlinguetus aestuans",
          uc == "Resex Rio Ouro Preto" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Resex Riozinho da Liberdade" & taxon_validado == "Alouatta seniculus" ~ "Alouatta puruensis",
          uc == "Resex Riozinho da Liberdade" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Resex Riozinho da Liberdade" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Resex Riozinho da Liberdade" & taxon_validado == "Myoprocta sp" ~ "Myoprocta pratti",
          uc == "Resex Tapajós-Arapiuns" & taxon_validado == "Dasyprocta croconota" ~ "Dasyprocta iacki",
          uc == "Resex Tapajós-Arapiuns" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta iacki",
          uc == "Resex Tapajós-Arapiuns" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta iacki",
          #uc == "Resex Tapajós-Arapiuns" & familia == "Sciuridae" ~ NA,
          uc == "Resex Tapajós-Arapiuns" & familia == "Sciuridae" ~ "Sciuridae",
          uc == "Resex do Cazumbá-Iracema" & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta fuliginosa",
          uc == "Resex do Cazumbá-Iracema" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta fuliginosa",
          uc == "Resex do Cazumbá-Iracema" & taxon_validado == "Tapirus kabomani" ~ "Tapirus terrestris",
          #taxon_validado == "Tinamus major" ~ "Tinamus sp",
          TRUE ~ taxon_validado),
        genero = case_when(
        #uc == "Parna da Amazônia" & familia == "Sciuridae" ~ NA,
        #uc == "Parna do Viruá" & familia == "Sciuridae" ~ NA,
        #uc == "Rebio do Jaru" & familia == "Sciuridae" ~ NA,
        #uc == "Rebio do Tapirapé" & familia == "Sciuridae" ~ NA,
        #uc == "Resex Tapajós-Arapiuns" & familia == "Sciuridae" ~ NA,
        TRUE ~ genero)
      )

    # atualizar nivel_taxon
    myData <- myData %>%
      mutate(
        nivel_taxon = case_when(
          is.na(taxon_validado) & is.na(genero) & is.na(familia) ~ "O",
          is.na(taxon_validado) & is.na(genero) ~ "F",
          is.na(taxon_validado)  ~ "G",
          #grepl(" sp", taxon_validado) ~ "G",
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

