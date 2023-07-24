#' Funçao para plotar tendencias temporais formato linhas e ribbon
#'
#' @returns grafico de tendencias temporais
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_ssm_linha()
#'

# plotar tendencias temporais
plotar_ssm_linha <- function() {

  est_abund_temp %>%
    ggplot(aes(ano, mediana)) +
    geom_line(color = "#00AFBB", linewidth = 1.5, alpha=1) +
    geom_point(shape=21, fill = "steelblue", stroke=0.5, size = 2, color="steelblue", alpha = 1) +
    geom_ribbon(aes(ymin = lower025, ymax = upper975), fill="steelblue", alpha=0.2) +
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
