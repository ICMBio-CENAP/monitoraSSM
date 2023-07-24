#' Funçao para plotar taxa de crescimento r
#'
#' @returns histograma da taxa de crescimento r
#' @export
#' @author
#' Elildo Carvalho Jr
#' @examples
#' plotar_r()
#'

# plotar tendencias temporais
plotar_r <- function() {
  tibble(r = as.numeric(ssm$BUGSoutput$sims.list$r_mean)) %>%
    ggplot(aes(x=r)) +
    geom_histogram(color = "black", fill = "steelblue", alpha=0.5, bins=15) +
    geom_vline(xintercept=0, linetype='dashed', color='black', size=0.7) +
    #xlim(0,2) +
    xlab("Taxa de crescimento (r)") +
    ylab("") +
    theme_classic() +
    theme(axis.title.y = element_text(size = 12))  +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12))

}
