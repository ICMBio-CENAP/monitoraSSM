#' Funçao para rodar modelo SSM no jags
#'
#' @returns resultado do modelo de espaço de estados
#' @export
#' @examples
#' grafico_pizza()
#'

grafico_pizza <- function() {

  # classificar pops em categorias

  #nsp <- nrow(ssm_results)

  # aumento solido, verde
  solidInc <- ssm_results$r_mean025 > 0
  color1 <- ifelse(solidInc,"green", NA)
  status1 <- ifelse(solidInc,"Aumento sólido", NA)

  # aumento provável, verde claro
  likelyInc <- ssm_results$r_mean10 > 0 & ssm_results$r_mean025 < 0
  color2 <- ifelse(likelyInc,"lightgreen", NA)
  status2 <- ifelse(likelyInc,"Aumento provável", NA)

  # declínio sólido, vermelho
  solidDec <- ssm_results$r_mean975 < 0
  color3 <- ifelse(solidDec,"red", NA)
  status3 <- ifelse(solidDec,"Declínio sólido", NA)

  # declínio provável, alaranjado
  likelyDec <- ssm_results$r_mean90 < 0 & ssm_results$r_mean975 > 0
  color4 <- ifelse(likelyDec,"orange", NA)
  status4 <- ifelse(likelyDec,"Declínio provável", NA)

  # especies fora das categorias acima estao estaveis, cor azul
  color5 <- ifelse((is.na(color1) + is.na(color2) + is.na(color3) + is.na(color4) >3),"grey",NA)
  status5 <- ifelse((is.na(color1) + is.na(color2) + is.na(color3) + is.na(color4) >3),"Estável",NA)

  allConds <- cbind(color1,color2,color3,color4,color5)
  allStatus <- cbind(status1,status2,status3,status4,status5)

  finalColor <- unlist(apply(allConds, 1, function(x) x[!is.na(x)]), use.names = F)
  finalStatus <- unlist(apply(allStatus, 1, function(x) x[!is.na(x)]), use.names = F)
  ssm_results <- mutate(ssm_results, status = finalStatus)

  # plotar tendencias de todas as pops
  par(mar=c(10,4,1,1))
  ssm_results <- ssm_results %>% arrange(desc(r_mean50))
  plot(1:nrow(ssm_results), ssm_results$r_mean50,
       ylim=c(min(ssm_results$r_mean025)-0.25, max(ssm_results$r_mean975)+0.25),
       xlab='', ylab='Taxa de crescimento', xaxt='n', main = "Monitora 2014-2022",
       bg = finalColor,
       pch = 21, cex = 0.8, cex.lab=1, cex.main=0.8, las=1)
  abline(h=0, lty=2)
  arrows(1:nrow(ssm_results), y0= ssm_results$r_mean025, y1= ssm_results$r_mean975, length=0)
  axis(1, at=1:nrow(ssm_results), labels=ssm_results$especie, las=2, cex.axis=0.5)
  legend("bottomleft", legend = c("Declínio sólido", "Declínio provável",
                                  "Estável", "Aumento provável", "Aumento sólido"),
         pch = 21, pt.bg = c("red", "orange", "blue", "lightgreen","green"))
  dev.off()

  jpeg(here("results", "tendencias_todas_as_pops.jpg"), width = 800, height = 600) # Open jpeg file
  ssm_results <- ssm_results %>% arrange(desc(r_mean50))
  plot(1:nrow(ssm_results), ssm_results$r_mean50,
       ylim=c(min(ssm_results$r_mean025)-0.2, max(ssm_results$r_mean975)+0.2),
       xlab='', ylab='Taxa de crescimento', xaxt='n', main = "Monitora 2014-2022",
       #bg = finalColor,
       pch = 21, cex = 0.8, cex.lab=1, cex.main=0.8, las=1)
  abline(h=0, lty=2)
  arrows(1:nrow(ssm_results), y0= ssm_results$r_mean025, y1= ssm_results$r_mean975, length=0)
  axis(1, at=1:nrow(ssm_results), labels=ssm_results$especie, las=2, cex.axis=0.4)
  #legend("bottomleft",
  #       legend = c("Declínio sólido", "Declínio provável",
  #                  "Estável", "Aumento provável", "Aumento sólido"),
  #       pch = 21, pt.bg = c("red", "orange", "blue", "lightgreen","green"))
  dev.off()

  # grafico de pizza aumentando, declinando, estavel
  pizza <- as.data.frame(table(ssm_results$status))
  names(pizza)[1] <- "status"

  ggplot(pizza, aes(x = "", y = Freq, fill = status )) +
    coord_polar("y", start = 0) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    scale_fill_manual(values = c("green4", "lightgreen", "orange", "red","steelblue")) +
    theme_void()


}

