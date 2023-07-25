# monitoraSSM (beta)
Centro Nacional de Pesquisa e Conservação de Mamíferos Carnívoros, Instituto Chico Mendes de Conservação da Biodiversidade <br />
[Contato elildojr@gmail.com]  



### Apresentação

O **monitoraSSM** (versão beta) é um pacote para análise de dados do protocolo básico para aves e mamíferos do Programa Monitora ICMBio.

O pacote tem funções para limpeza de dados brutos, análise de tendências populacionais por meio de modelos de espaço de estados (*state-space models*), e calculo da média geométrica das abundâncias relativas, um dos índices de diversidade adotados pelo programa. O pacote também  produz diversos gráficos sumarizando os resultados.


### Instalar o pacote e exemplos

Primeiramente, instalar o pacote devtools para permitir instalação a partir do github:


```r
install.packages("devtools")
```

Instalar o pacote **monitoraSSM** a partir de nosso github:


```r
library(devtools)
# Instalar a partir do repositório online do CENAP
install_github("ICMBio-CENAP/monitoraSSM", dependencies=TRUE)

```

A partir daí o pacote pode ser carregado normalmente pela library


```r
# Load library
library(monitoraSSM)
library(tidyverse)

```

Os dados já limpos são carregados automaticamente:
```r
monitora
```

Rodar modelo de espaço de estados para uma população

```r
# rodar modelo
rodar_ssm(monitora, "Callicebus_vieirai_47")

# conferir resultados
# output do modelo do jags:
ssm

# sumário dos resultados e tendências populacionais
ssm_results_temp
est_abund_temp

# plotar tendencias temporais e taxa de crescimento r
plotar_ssm_linha()
plotar_ssm_barra()
plotar_r()


```

