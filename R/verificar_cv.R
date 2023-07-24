#' Funçao para verificar CV das taxas de encontro usando bootstrap
#'
#' @param monitora Nome da base de dados
#' @returns tabela comparando CV e esforco para todas as populacoes
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' verificar_cv()
#' @import tidyverse
#' @importFrom tidyverse %>%

verificar_cv <- function(popsToUse = c(popsToUse)) {

  # tabela que recebera resultados ao final do loop
  tabela_cv <- tibble(populacao = as.character(NA),
                      esforco = as.numeric(NA),
                      cv = as.numeric(NA))

  # loop para verificar cv para todas as populacoes
  for(i in 1:nrow(popsToUse)){

    # selecionar ano com mais dados para avaliar CV
    ano_teste <- monitora %>%
      filter(uc == pull(popsToUse[i,1])) %>%
      distinct(nome_ea, ano, data, esforco) %>%
      group_by(ano) %>%
      summarize(esforco_total = sum(esforco, na.rm = TRUE)) %>%
      filter(esforco_total == max(esforco_total)) %>%
      pull(ano)

    # calcular taxas de encontro observadas no ano selecionado
    esforco <- monitora %>%
      filter(uc == pull(popsToUse[i,1]),
             ano == ano_teste) %>%
      group_by(nome_ea, data) %>%
      summarise(esforco = sum(esforco, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(esforco > 0)

    encontros <- monitora %>%
      filter(uc == pull(popsToUse[i,1]),
             ano == ano_teste,
             populacao == pull(popsToUse[i,2])) %>%
      group_by(nome_ea, data) %>%
      count()

    taxa_encontro <- esforco %>%
      left_join(encontros, by=c("nome_ea", "data")) %>%
      mutate(taxa_encontro = (n/esforco)*10) %>%
      replace_na(list(taxa_encontro = 0)) %>%
      select(esforco, taxa_encontro) #%>% ungroup()

    # estimar o cv para cada nivel de esforco
    # usar bootstrap para calcular CV por esforco (max esforco = 50)
    # criar objeto para receber valores, 100 reps por nivel esforco
    sim_list <- tibble(esforco = as.numeric(rep(1:150, 100)),
                       taxa_media = as.numeric(NA)) %>%
      filter(esforco %in% c(1, seq(5, 150, by = 5)) ) # somente multiplos de 5
    # obter cvs por bootstrap
    for(j in 1:nrow(sim_list)) {
      sim_list[j,2] <- taxa_encontro %>%
        sample_n(size = pull(sim_list[j,1]), replace = TRUE) %>%
        summarize(media_tx_trilha = mean(taxa_encontro)) %>%
        summarize(media_tx = mean(media_tx_trilha)) %>%
        pull()
    }

    # obter cv medio por esforco
    cv_por_esforco <- sim_list %>%
      group_by(esforco) %>%
      summarise(cv =  sd(taxa_media, na.rm = TRUE) / mean(taxa_media, na.rm = TRUE)) %>%
      drop_na()

    # tabela com cvs
    cv_por_esforco <- cv_por_esforco %>%
      mutate(populacao = pull(popsToUse[i,2])) %>%
      select(populacao, esforco, cv) %>%
      filter(cv <= 0.25)

    tabela_cv <- bind_rows(tabela_cv, cv_por_esforco) %>%
      drop_na() %>%
      group_by(populacao) %>%
      filter(cv <= 0.25) %>%
      filter(esforco == min(esforco)) %>%
      print()

  }

  assign("tabela_cv", tabela_cv, envir = .GlobalEnv)
}


