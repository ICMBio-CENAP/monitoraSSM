#' Funçao para limpar myData do Monitora
#'
#' @param nome_arquivo Nome do arquivo de dados
#' @returns myData corrigidos e limpos
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' ler_dados()
#' @import tidyverse
#' @import lubridate


ler_dados <- function(nome_arquivo = "Planilha MastoAves Consolidada ate 2022.csv") {

  library(here)
  library(tidyverse)

  myData <- read_csv(here("data-raw", nome_arquivo))

  # renomear colunas
  myData <- myData %>%
    dplyr::rename(
      cnuc = "CDUC",
      uc = "Local - Nome da Unidade de Conservação",
      ea = "Número da Estação Amostral",
      nome_ea = "Nome da EA",
      esforco = "Esforço de amostragem tamanho da trilha (m)",
      data = "data da amostragem",
      hora_inicio = "horário de início  (h:mm)",
      hora_fim = "horário de término (h:mm)",
      ano = "Ano",
      classe = "Classe",
      ordem = "Ordem",
      familia = "Família",
      genero = "Gênero",
      taxon_validado = "Espécies validadas para análise do ICMBio",
      nivel_taxon = "Clasificação taxonômica (espécie. gênero. família ou ordem)",
      n_animais = "n° de animais",
      distancia = "distância (m)     do animal em relação a trilha",
      plaqueta = "marcação no transecto",
      hora_avistamento = "horário do avistamento"
    ) %>%
    select(cnuc, uc, ea, nome_ea, esforco, data, ano,
           classe, ordem, familia, genero, taxon_validado,
           nivel_taxon, n_animais, distancia, plaqueta, hora_avistamento) %>%
    # ajustar formato de data
    dplyr::mutate(
      data = as.Date(data, "%d/%m/%Y"),
      ano = year(data),
      esforco = esforco/1000,
      hora_avistamento = as.POSIXct(paste(data, hora_avistamento), format = "%Y-%m-%d %H:%M:%OS")
    )


  # adicionar esforco a todas as linhas
  temp_esforco <- myData %>%
    distinct(cnuc, ea, esforco, ano, data) %>%
    drop_na()
  myData <- myData %>%
    select(-esforco) %>%
    left_join(temp_esforco, by=c("cnuc", "ea", "ano", "data"))

  # caso ainda ocorram linhas sem esforco:
  # obter esforco medio em cada trilha...
  comprimento <- myData %>%
    distinct(uc, nome_ea, esforco) %>%
    drop_na() %>%
    group_by(uc, nome_ea) %>%
    summarize(esforco = mean(esforco))
  # ...e imputar esforco medio para as entradas sem info de esforco
  myData <- myData %>%
    rows_patch(comprimento, by = c("uc", "nome_ea"))

  # remover entradas duplicadas
  myData <- myData %>%
    distinct(uc, ea, data, taxon_validado, n_animais,
             distancia, plaqueta, hora_avistamento,
             .keep_all = TRUE)

  # criar coluna populacao
  myData <- myData %>%
    mutate(populacao = paste(taxon_validado, cnuc, sep ="_"),
           populacao = str_replace(populacao, " ", "_")) %>%
    mutate(populacao = case_when(
      is.na(taxon_validado) ~ NA,
      TRUE ~ populacao))

  # remover linhas so de NAs
  myData <-  myData %>% filter_all(any_vars(!is.na(.)))

  assign("myData", myData, envir = .GlobalEnv)

}

