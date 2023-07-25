#' Funçao para plotar media geometrica
#'
#' @returns Gráfico do Living Planet Index para dados do ICMBio
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_lpi()
#' @import tidyverse

# funcao para plotar lpi icmbio
plotar_lpi <- function() {
  tibble(years = as.numeric(as.character(rownames(monitora_lpi))),
         lpi = monitora_lpi$LPI_final,
         lwr = monitora_lpi$CI_low,
         upr = monitora_lpi$CI_high) %>%
    ggplot(aes(years, lpi)) +
    geom_line(color = "#00AFBB", size = 1, alpha=1) +
    geom_point(shape=21, fill = "white", stroke=1, size = 2, color="steelblue", alpha = 1) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill="steelblue", alpha=0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha=.5) +
    xlab("Ano") +
    ylab("Índice (2014=1)") +
    theme_classic() +
    theme(axis.title.y = element_text(size = 12))  +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(legend.position="none") +
    coord_cartesian(ylim = c(0, 2))
}
