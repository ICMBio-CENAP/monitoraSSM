#' Atribuir epiteto especifico com base na lista de spp previamente registradas na UC
#'
#' @returns especies atribuidas
#' @export
#' @author
#' Jorge Mario Herrera Lopera
#' @examples
#' atribuir_spp()
#' @import tidyverse
#' @import stringr


atribuir_spp <- function(myData){

  # Carregar o banco de dados "monitora"

  #load("monitora.RData")

  # Contar o número de "taxon_validado" para cada combinação exclusiva de "genero" e "cnuc".

  conteo_taxon <- monitora %>%
    group_by(cnuc, genero) %>%
    summarize(num_taxon = n_distinct(taxon_validado))

  # Filtrar combinações exclusivas com um único "taxon_validado"

  monitora.subset <- conteo_taxon %>%
    filter(num_taxon == 1) %>%
    left_join(monitora, by = c("cnuc", "genero"))

  # Selecionar as colunas necessárias

  monitora.subset <- monitora.subset %>%
    distinct(cnuc, genero, .keep_all = TRUE) %>%
    select(cnuc, genero, taxon_validado)

  # Exclua as linhas com NA e taxon_validado contendo "sp.".

  monitora.subset <- monitora.subset %>%
    filter(!is.na(taxon_validado), !str_detect(taxon_validado, "sp."))

  # Filtre as linhas de myData em que a segunda palavra de "taxon_validado" seja "sp".

  filas_a_actualizar <- myData %>%
    filter(str_split(taxon_validado, " ")[[1]][2] == "sp.")

  # Executar left_join para atualizar as linhas correspondentes

  myData_mod <- left_join(myData, monitora.subset, by = c("cnuc", "genero"))

  # Atualizar valores em "taxon_validado" somente para as linhas selecionadas

  myData_mod <- myData_mod %>%
    mutate(
      taxon_validado = ifelse(!is.na(taxon_validado.y), taxon_validado.y, taxon_validado.x)
    ) %>%
    select(-taxon_validado.y, -taxon_validado.y)

  myData_mod <- myData_mod %>% select(names(monitora))

  # Criar un data.frame "observaciones"

  observaciones <- myData %>%
    filter(str_split(taxon_validado, " ")[[1]][2] == "sp.") %>%
    left_join(
      monitora.subset %>% select(cnuc, genero, taxon_validado),
      by = c("cnuc", "genero")
    ) %>%
    anti_join(
      conteo_taxon %>% filter(num_taxon > 1) %>% select(cnuc, genero),
      by = c("cnuc", "genero")
    ) %>%
    mutate(
      taxon_validado_mod = ifelse(!is.na(taxon_validado.y), taxon_validado.y, taxon_validado.x),
      observaciones = case_when(
        !is.na(taxon_validado.y) & taxon_validado.x != taxon_validado.y ~ "assigned",
        is.na(taxon_validado.y) & !duplicated(taxon_validado.x) ~ "new record",
        is.na(taxon_validado.y) & !duplicated(taxon_validado.x) ~ "more than one species",
        TRUE ~ "more than one species"
      )
    ) %>%
    select(cnuc, genero, taxon_validado = taxon_validado.x, taxon_validado_mod, observaciones)

  # Filtrar os casos "more than one species" e adicioná-los à tabela de observações.

  observaciones_more_than_one <- conteo_taxon %>%
    filter(num_taxon > 1) %>%
    left_join(
      myData %>% select(cnuc, genero, taxon_validado),
      by = c("cnuc", "genero")
    ) %>%
    filter(!duplicated(taxon_validado)) %>%
    filter(!is.na(taxon_validado)) %>%
    mutate(
      taxon_validado_mod = taxon_validado,
      observaciones = "more than one species"
    ) %>%
    select(cnuc, genero, taxon_validado, taxon_validado_mod, observaciones)

  # Combinar las observaciones y las de "more than one species"

  observaciones <- bind_rows(observaciones, observaciones_more_than_one)

  # Cargar las bases de datos ao environment

  assign("myData", myData_mod, envir = .GlobalEnv)
  assign("observaciones", observaciones, envir = .GlobalEnv)
}


