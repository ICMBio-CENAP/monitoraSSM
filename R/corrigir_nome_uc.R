#' Funçao para corrigir nomes das UCs
#'
#' @param myData Nome do arquivo de myData
#' @returns myData com nome de UCs corrigidos
#' @export
#' @author
#' Jorge Mario Herrera Lopera
#' @examples
#' corrigir_nome_uc()
#' @import tidyverse

# contato: mario.herreralopera@gmail.com - Ilhéus - BA, setembro de 2023

corrigir_nome_uc <- function(myData) {

  myData_cor <- left_join(myData, codname %>% select(cnuc, uc_cor), by = "cnuc") ## Selecionar a coluna de código em ambos os bancos de dados.
  # Selecionar a coluna do nome corrigido
  uc_cor <- myData_cor$uc_cor
  # Substituir a coluna de nome no banco de dados original
  myData$uc <- uc_cor

  assign("myData", myData, envir = .GlobalEnv)

}
