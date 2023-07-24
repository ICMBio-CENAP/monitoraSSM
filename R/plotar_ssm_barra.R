#' Funçao para plotar tendencias temporais formato barras
#'
#' @returns grafico de tendencias temporais
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_ssm_barra()
#' @import tidyverse
#' @importFrom tidyverse %>%

# plotar tendencias temporais
plotar_ssm_barra <- function() {

  est_abund_temp %>%
    ggplot(aes(ano, mediana)) +
    geom_crossbar(aes(x = as.integer(ano), y = mediana,
                      ymin = lower025, ymax = upper975),
                  width = 0.2, colour = "steelblue", fill = "steelblue", alpha = 0.25, size = 0) +
    geom_crossbar(aes(x = as.integer(ano), y = mediana, ymin = lower10, ymax = upper90),
                  width = 0.2, colour =  "steelblue", fill = "steelblue", alpha = 0.5, size = 0) +
    xlab("Ano") +
    ylab("Abundância relativa") +
    theme_classic() +
    theme(axis.title.y = element_text(size = 12))  +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(legend.position="none") +
    coord_cartesian(ylim = c(0, 1.5*max(est_abund_temp$upper975)) ,
                    xlim = c(min(est_abund_temp$ano)-0.25, max(est_abund_temp$ano)+0.25),
                    expand = FALSE)
}
