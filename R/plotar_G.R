#' Funçao para plotar media geometrica
#'
#' @returns Gráfico da vriação da media geometrica ao longo do tempo
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_G()
#' @import tidyverse

# funcao para estimar media geometrica
plotar_G <- function() {

  # plotar serie temporal para G
  tabela_Gmean %>%
    group_by(ano) %>%
    mutate(G = G/first(G)) %>% # escala 2014=1
    summarize(media = mean(G),
              lower10 = quantile(G, prob= 0.1),
              lower25 =  quantile(G, prob= 0.25),
              upper90 =  quantile(G, prob= 0.9),
              upper975 =  quantile(G, prob= 0.975)) %>%
    ggplot(aes(ano, media)) +
    geom_line(color = "#00AFBB", size = 1, alpha=1) +
    geom_point(shape=21, fill = "white", stroke=1, size = 2, color="steelblue", alpha = 1) +
    geom_ribbon(aes(ymin = lower10, ymax = upper90), fill="steelblue", alpha=0.2) +
    geom_ribbon(aes(ymin = lower25, ymax = upper975), fill="steelblue", alpha=0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = .5) +
    xlab("Ano") +
    ylab("Média geométrica") +
    theme_classic() +
    theme(axis.title.y = element_text(size = 12))  +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(legend.position="none") +
    coord_cartesian(ylim = c(0.5, 1.5))
}


