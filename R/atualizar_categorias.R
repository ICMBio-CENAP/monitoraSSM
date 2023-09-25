#' atualizar as categorias taxonômicas superiores a gênero
#'
#' @returns resultado do modelo de espaço de estados
#' @export
#' @author
#' Jorge Mario Herrera Lopera
#' @examples
#' atualizar_categorias()
#' @import tidyverse
#' @import stringr


atualizar_categorias <- function(myData) {

  #load("db_sin_aves_mammals.rda") ## base de sinonimos

  namesmd <- colnames(myData) ## Ordenação original das colunas

  # Obter combinações únicas de db_sin_birds_mammals para cada nível

  db_sin_aves_mammals_genero <- db_sin_aves_mammals[, 3:4] %>%
    distinct(genero, .keep_all = TRUE)

  db_sin_aves_mammals_familia <- db_sin_aves_mammals[, 2:3] %>%
    distinct(familia, .keep_all = TRUE)

  db_sin_aves_mammals_ordem <- db_sin_aves_mammals[, 1:2] %>%
    distinct(ordem, .keep_all = TRUE)


  # Paso 1: Atualizar myData$familia según myData$genero

  myData <- myData %>%
    left_join(db_sin_aves_mammals_genero, by = c("genero" = "genero")) %>%
    mutate(familia = if_else(!is.na(genero), familia.y, familia.x)) %>%
    select(-starts_with("familia."))

  # Paso 2: Atualizar myData$ordem según myData$familia

  myData <- myData %>%
    left_join(db_sin_aves_mammals_familia, by = c("familia" = "familia")) %>%
    mutate(ordem = if_else(!is.na(familia), ordem.y, ordem.x)) %>%
    select(-starts_with("ordem."))

  # Paso 3: Atualizar myData$classe según myData$ordem

  myData <- myData %>%
    left_join(db_sin_aves_mammals_ordem, by = c("ordem" = "ordem")) %>%
    mutate(classe = if_else(!is.na(ordem), classe.y, classe.x)) %>%
    select(-starts_with("classe."))

  # Reordenar as colunas
  myData <- myData %>% select(all_of(namesmd))

  # salvar
  assign("myData", myData, envir = .GlobalEnv)

  # mostrar
  return(myData)
}

