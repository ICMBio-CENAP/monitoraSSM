#' Funçao para plotar CV como funcao do esforco
#'
#' @returns Gráfico do CV da taxa de crescimento em função do esforço
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_cv()
#' @import tidyverse
#' @importFrom tidyverse %>%

# plotar tendencias temporais
plotar_cv <- function() {
  # plotar CV como funcao do esforco
  cv_por_esforco %>%
    ggplot(aes(x=esforco, y=cv)) +
    geom_point(fill = "black", alpha = 1/2, size = 1.5) +
    #geom_line(size = 0.5, alpha = 0.5) +
    geom_smooth(method = loess, se = FALSE, linewidth = 0.3, color = "black", alpha = 0.8) +
    geom_hline(yintercept = 0.20, linetype = "dashed", alpha = 0.5) +
    #geom_vline(xintercept = 150, linetype = "dashed", alpha = 0.5) +
    xlab("Esforço (km)") +
    ylab("CV") +
    theme_classic() +
    theme(axis.title.y = element_text(size = 14))  +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
}


