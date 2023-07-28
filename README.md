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
library(here)

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

### Nota sobre o uso dos dados

A política de dados do Programa Monitora está descrita na [Instrução Normativa ICMBio 02/2022](https://www.gov.br/icmbio/pt-br/assuntos/monitoramento/IN2_2022_fev.pdf). Destaca-se que os dados provenientes da aplicação de protocolos básicos de alvos globais serão de acesso público após a etapa de validação. desta forma, os [dados de 2014 a 2018 são públicos](https://www.gov.br/icmbio/pt-br/assuntos/monitoramento/conteudo/dados/dados-alvos-globais-protocolos-basico-componente-florestal.xlsx/view), ao passo que os dados de 2019 em diante são de uso restrito.

Forma de citação dos dados: Monitora; Sampaio, A.B.; Alonso, A.C.; Iserhard, C.A.; Ribeiro, D.B.; Andrade, D.F.C.;  Carvalho Junior, E.A.R.; Buss, G.; Souza, J.M.; Ribeiro, K.T.; Mende, K.R.; Reis, M.L.; Fialho, M.S.; Marini Filho, O.J.; Galuppo, S.C.; Souza, T.C. Programa Nacional de Monitoramento da Biodiversidade - Programa Monitora, subprograma Terrestre, componente Florestal: relatório 2014-2018. Brasília, DF: ICMBio, 2021.
