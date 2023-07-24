#' Funçao para limpar dados do Monitora
#'
#' @param myData Nome do arquivo de myData
#' @returns myData corrigidos e limpos
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' corrigir_taxonomia()
#'

corrigir_taxonomia <- function(myData) {

  # padronizar "sp"
  myData <- myData %>%
    mutate(taxon_validado = str_replace(taxon_validado, "sp.", "sp"),
           taxon_validado = str_replace(taxon_validado, "Cf. ", "")) %>%
    # classe
    mutate(
      classe = case_when(
        classe == "aves" ~ "Aves",
        classe == "mammalia" ~ "Mammalia",
        classe == "Mammalia" & taxon_validado == "Penelope superciliaris" ~ "Aves",
        TRUE ~ classe)) %>%
    # ordem
    mutate(
      ordem = case_when(
        ordem == "GAlliformes" ~ "Galliformes",
        ordem == "Galiformes" ~ "Galliformes",
        ordem == "Rodentia" & taxon_validado == "Penelope superciliaris" ~ "Galliformes",
        ordem == "pilosa" ~ "Pilosa",
        ordem == "Primates" & taxon_validado == "Dasyprocta sp" ~ "Rodentia",
        ordem == "Pitheciidae"  ~ "Primates",
        TRUE ~ ordem)) %>%
    # familia
    mutate(
      familia = case_when(
        familia == "cracidae" ~ "Cracidae",
        is.na(familia) & taxon_validado == "Cracidae" ~ "Cracidae",
        is.na(familia) & taxon_validado == "Tinamidae" ~ "Tinamidae",
        familia == "Suidae" & taxon_validado == "Tayassu pecari" ~ "Tayassuidae",
        familia == "Procyonidae" & taxon_validado == "Eira barbara" ~ "Mustelidae",
        is.na(familia) & taxon_validado == "Didelphis marsupialis" ~ "Didelphidae",
        familia == "Caviidae" & taxon_validado == "Dasyprocta fuliginosa" ~ "Dasyproctidae",
        familia == "Caviidae" & genero == "Dinomys" ~ "Dinomyidae",
        familia == "Dasyproctidae" & taxon_validado == "Penelope superciliaris" ~ "Cracidae",
        familia == "Sciuridae" & taxon_validado == "Dasyprocta sp" ~ "Dasyproctidae",
        familia == "Sciuridae" & taxon_validado == "Myoprocta pratti" ~ "Dasyproctidae",
        familia == "Callithrichidae" ~ "Callitrichidae",
        familia == "Callithricidae" ~ "Callitrichidae",
        familia == "Odotophoridae" ~ "Odontophoridae",
        familia == "Sciuricidae" ~ "Sciuridae",
        familia == "Tinanmidae" ~ "Tinamidae",
        TRUE ~ familia)) %>%
    # genero
    mutate(
      genero = case_when(
        genero == "Crax" & taxon_validado == "Pauxi tuberosa" ~ "Pauxi",
        genero == "Colinus" & taxon_validado == "Odontophorus gujanensis" ~ "Odontophorus",
        is.na(genero) & taxon_validado == "Odontophorus gujanensis" ~ "Odontophorus",
        genero == "Nothura" & taxon_validado == "Crypturellus sp" ~ "Crypturellus",
        genero == "Rhynchotus" & taxon_validado == "Tinamus sp" ~ "Tinamus",
        is.na(genero) & taxon_validado == "Tinamus sp" ~ "Tinamus",
        genero == "Ozotocerus" & taxon_validado == "Mazama sp" ~ "Mazama",
        is.na(genero) & taxon_validado == "Mazama sp" ~ "Mazama",
        genero == "Sus" & taxon_validado == "Tayassu pecari" ~ "Tayassu",
        is.na(genero) & taxon_validado == "Tayassu pecari" ~ "Tayassu",
        genero == "Cebuella" & taxon_validado == "Saguinus niger" ~ "Saguinus",
        genero == "Microsciurus" & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta",
        genero == "Microsciurus" & taxon_validado == "Sciurillus pusillus" ~ "Sciurillus",
        genero == "Sciurillus" & taxon_validado == "Guerlinguetus aestuans" ~ "Guerlinguetus",
        genero == "Sciurillus" & taxon_validado == "Urosciurus spadiceus" ~ "Urosciurus",
        genero == "Urosciurus" & taxon_validado == "Guerlinguetus aestuans" ~ "Guerlinguetus",
        genero == "Urosciurus" & taxon_validado == "Microsciurus flaviventer" ~ "Microsciurus",
        genero == "Urosciurus" & taxon_validado == "Myoprocta pratti" ~ "Myoprocta",
        genero == "Puma" & taxon_validado == "Puma yagouaroundi" ~ "Herpailurus",
        is.na(genero) & taxon_validado == "Dasypus sp" ~ "Dasypus",
        is.na(genero) & taxon_validado == "Didelphis marsupialis" ~ "Didelphis",
        is.na(genero) & taxon_validado == "Bradypus tridactylus" ~ "Bradypus",
        is.na(genero) & taxon_validado == "Alouatta belzebul" ~ "Alouatta",
        is.na(genero) & taxon_validado == "Mico humeralifer" ~ "Mico",
        is.na(genero) & taxon_validado == "Mico melanurus" ~ "Mico",
        is.na(genero) & taxon_validado == "Saguinus niger" ~ "Saguinus",
        is.na(genero) & taxon_validado == "Saguinus sp" ~ "Saguinus",
        is.na(genero) & taxon_validado == "Dasyprocta leporina" ~ "Dasyprocta",
        is.na(genero) & taxon_validado == "Dasyprocta sp" ~ "Dasyprocta",
        is.na(genero) & taxon_validado == "Coendou sp" ~ "Coendou",
        is.na(genero) & taxon_validado == "Guerlinguetus aestuans" ~ "Guerlinguetus",
        genero == "Aloutta" ~ "Alouatta",
        genero == "Crypturelus" ~ "Crypturellus",
        genero == "M" ~ NA,
        genero == "Sciurullus" ~ "Sciurillus",
        genero == "Tanmandua" ~ "Tamandua",
        genero == "crypturellus" ~ "Crypturellus",
        genero == "tinamus" ~ "Tinamus",
        genero == "penelope" ~ "Penelope",
        TRUE ~ genero)) %>%
    # taxon_validado
    mutate(
      taxon_validado = case_when(
        taxon_validado == "Cracidae" ~ NA,
        taxon_validado == "Tinamidae" ~ NA,
        is.na(taxon_validado) & genero == "Mazama" ~ "Mazama sp",
        is.na(taxon_validado) & genero == "Ozotocerus" ~ "Ozotocerus bezoarticus",
        taxon_validado == "Cervidae" ~ NA,
        taxon_validado == "Tayassuidae" ~ NA,
        taxon_validado == "Felidae" ~ NA,
        taxon_validado == "Procyonidae" ~ NA,
        taxon_validado == "Atelidae" ~ NA,
        taxon_validado == "Callitrichidae" ~ NA,
        taxon_validado == "Cebidae" ~ NA,
        taxon_validado == "Pitheciidae" ~ NA,
        taxon_validado == "Ptheciidae" ~ NA,
        taxon_validado == "Primates" ~ NA,
        taxon_validado == "Sciuridae" ~ NA,
        is.na(taxon_validado) & genero == "Atelocynus" ~ "Atelocynus microtis",
        is.na(taxon_validado) & genero == "Pecari" ~ "Pecari tajacu",
        is.na(taxon_validado) & genero == "Tayassu" ~ "Tayassu pecari",
        is.na(taxon_validado) & genero == "Speothos" ~ "Speothos venaticus",
        is.na(taxon_validado) & genero == "Leopardus" ~ "Leopardus sp",
        is.na(taxon_validado) & genero == "Choloepus" ~ "Choloepus didactylus",
        is.na(taxon_validado) & genero == "Myrmecophaga" ~ "Myrmecophaga tridactyla",
        is.na(taxon_validado) & genero == "Tamandua" ~ "Tamandua tetradactylaa",
        is.na(taxon_validado) & genero == "Aotus" ~ "Aotus sp",
        is.na(taxon_validado) & genero == "Alouatta" ~ "Alouatta sp",
        is.na(taxon_validado) & genero == "Ateles" ~ "Ateles sp",
        is.na(taxon_validado) & genero == "Lagothrix" ~ "Lagothrix sp",
        is.na(taxon_validado) & genero == "Callimico" ~ "Callimico goeldii",
        is.na(taxon_validado) & genero == "Callithrix" ~ "Callithrix sp",
        is.na(taxon_validado) & genero == "Cebuella" ~ "Cebuella pygmaea",
        is.na(taxon_validado) & genero == "Mico" ~ "Mico sp",
        is.na(taxon_validado) & genero == "Saguinus" ~ "Saguinus sp",
        is.na(taxon_validado) & genero == "Cebus" ~ "Cebus sp",
        is.na(taxon_validado) & genero == "Saimiri" ~ "Saimiri sp",
        is.na(taxon_validado) & genero == "Sapajus" ~ "Sapajus sp",
        is.na(taxon_validado) & genero == "Cacajao" ~ "Cacajao sp",
        is.na(taxon_validado) & genero == "Callicebus" ~ "Callicebus sp",
        is.na(taxon_validado) & genero == "Chiropotes" ~ "Chiropotes sp",
        is.na(taxon_validado) & genero == "Pithecia" ~ "Pithecia sp",
        is.na(taxon_validado) & genero == "Hydrochoerus" ~ "Hydrochoerus hydrochaeris",
        is.na(taxon_validado) & genero == "Dinomys" ~ "Dinomys paca",
        is.na(taxon_validado) & genero == "Dasyprocta" ~ "Dasyprocta sp",
        is.na(taxon_validado) & genero == "Myoprocta" ~ "Myoprocta sp",
        is.na(taxon_validado) & genero == "Guerlinguetus" ~ "Guerlinguetus sp",
        is.na(taxon_validado) & genero == "Microsciurus" ~ "Microsciurus sp",
        is.na(taxon_validado) & genero == "Sciurillus" ~ "Sciurillus sp",
        is.na(taxon_validado) & genero == "Microsciurus" ~ "Microsciurus sp",
        is.na(taxon_validado) & genero == "Urosciurus" ~ "Urosciurus sp",
        is.na(taxon_validado) & genero == "Dasyprocta" ~ "Dasyprocta sp",
        taxon_validado == "brachyteles arachnoides" ~ "Brachyteles arachnoides",
        taxon_validado == "bradypus variegatus" ~ "Bradypus variegatus",
        taxon_validado == "Cf. Crax sp." ~ "Crax sp.",
        taxon_validado == "Cf. Crax globulosa" ~ "Crax sp.",
        taxon_validado == "Chiropotes chiropotes (Sagulatus)" ~ "Chiropotes sagulatus",
        taxon_validado == "Dasyprocta cf. fuliginosa" ~ "Dasyprocta fuliginosa",
        taxon_validado == "DAsyprocta cf. fuliginosa" ~ "Dasyprocta fuliginosa",
        taxon_validado == "Dsyprocta fuliginosa" ~ "Dasyprocta fuliginosa",
        taxon_validado == "Eira Barbara" ~ "Eira barbara",
        taxon_validado == "Nasua Nasua" ~ "Nasua nasua",
        taxon_validado == "Odontophorus Stellatus" ~ "Odontophorus stellatus",
        taxon_validado == "Procyon Cancrivorus" ~ "Procyon cancrivorus",
        taxon_validado == "Puma yagouaroundi" ~ "Herpailurus yagouaroundi",
        taxon_validado == "Lontra longicaudata" ~ "Lontra longicaudis",
        taxon_validado == "Myrmrcophaga tridactyla" ~ "Myrmecophaga tridactyla",
        taxon_validado == "Speothus venaticus" ~ "Speothos venaticus",
        taxon_validado == "Saimiri Ustus" ~ "Saimiri ustus",
        taxon_validado == "Tapirus sp" ~ "Tapirus terrestris",
        taxon_validado == "Tinsmidae" ~ "Tinamidae",
        taxon_validado == "Ptheciidae" ~ "Pitheciidae",
        taxon_validado == "Urosciurus spdiceus" ~ "Urosciurus spadiceus",
        taxon_validado == "O" ~ NA,
        taxon_validado == "E" ~ NA,
        taxon_validado == "F" ~ NA,
        taxon_validado == "G" ~ NA,
        TRUE ~ taxon_validado)) %>%
    # nivel_taxon
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

