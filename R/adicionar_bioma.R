#' Funçao para adicionar bioma aos dados do Monitora
#'
#' @param nome_arquivo Nome do arquivo de dados
#' @returns myData com bioma
#' @author
#' Elildo Carvalho Jr
#' @export
#' @examples
#' adicionar_bioma()
#' @import tidyverse
#' @import lubridate


adicionar_bioma <- function(myData) {

  myData <- myData %>%
    left_join(uc_table, by="cnuc") %>%
    relocate(c(sigla, biomaIBGE), .after = uc) %>%
    mutate(biomaIBGE = tolower(biomaIBGE)) %>%
    mutate(biomaIBGE = str_to_title(biomaIBGE))

    assign("myData", myData, envir = .GlobalEnv)
}

