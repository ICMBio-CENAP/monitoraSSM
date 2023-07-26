#' Funçao para plotar tendencias temporais formato barras
#'
#' @returns grafico de tendencias temporais
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_multipainel()
#' plotar_multipainel(tipo = "uc", nome_uc = "Resex Tapajós-Arapiuns")
#' plotar_multipainel(tipo = "r")
#' @import tidyverse

# plotar tendencias temporais
plotar_multipainel <- function(tipo = c("uc", "r"), nome_uc) {

  if(tipo == "uc") {
    ssm_data <- est_abund %>%
      filter(uc == nome_uc) %>%
      arrange(populacao) %>%
      mutate(populacao = paste(taxon_validado, uc, sep=" - ")) %>%
      arrange(populacao)
  }
  if(tipo == "r") {
    ssm_data <- est_abund %>%
      filter(populacao %in% (ssm_results %>%
                               filter(r_mean975 < 0) %>%
                               pull(populacao))) %>%
      group_by(populacao) %>%
      mutate(populacao = paste(taxon_validado, uc, sep=" - ")) %>%
      arrange(populacao)
  }

  ssm_data %>%
    ggplot(aes(ano, mediana)) +
    geom_line(color = "#00AFBB", size = 1, alpha=1) +
    geom_point(shape=21, fill = "white", stroke=0.5, size = 1, color="steelblue", alpha = 1) +
    geom_ribbon(aes(ymin = lower025, ymax = upper975), fill="steelblue", alpha=0.2) +
    xlab("Ano") +
    ylab("Abundância relativa") +
    facet_wrap(~ populacao, ncol=3, scales = "free") +
    scale_x_continuous(limits = c(2014, 2022)) +
    theme_classic() +
    theme(axis.title.y = element_text(size = 8))  +
    theme(axis.text.x = element_text(size = 8)) +
    theme(axis.text.y = element_text(size = 8)) +
    theme(strip.text.x = element_text(size = 9)) +
    theme(strip.background = element_blank()) +
    theme(legend.position="none")

}


