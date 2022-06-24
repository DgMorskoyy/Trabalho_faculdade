#Diretório

#Descobrindo qual a pasta de trabalho que o R está "enxergando" neste momento
getwd()

#Modificando a pasta de trabalho que o R está "enxergando" neste momento
setwd("C:/Users/lucas/Downloads")

################################################################################

#Pacotes

#Instalação via CRAN (Comprehensive R Archive Network)

#Pacote necessário para a importação da base de dados
install.packages("readr")

#Pacote necessário para explorar e manipular os dados da base
install.packages("dplyr")

#Pacote necessário para dividir, aplicar e combinar dados da base
install.packages("data.table")

#Pacote necessário para criação de tabelas com uma ou mais variáveis
install.packages("expss")

#Ativando os pacotes instalados
library(readr)
library(dplyr)
library(data.table)
library(expss) #bugando o código

################################################################################

#Importando a base de dados da forma correta.

#1.Nomeamos o objeto;
#2.Utilizamos a função do pacote readr responsável por importar arquivos .txt;
#3.Preenchemos os argumentos da função. Sendo eles:
##file = o arquivo a ser importado;
##delim = define o caracter utilizado para separar as variáveis;
##na = define qual a codificação usada para dado faltante;
##locale = controla vários aspectos como decimal, enconding, entre outros;
###decimal_mark = altera o caracter que separa unidade de decimal;
##col_types = altera a classe da variável;
###col_character = altera a classe da variável para chacacter;
base_PREp = read_delim(file = "base_PREp.txt",
                       delim = " ",
                       na = c("77777","88888"),
                       locale = locale(decimal_mark = ","),
                       col_types = cols(cd_pac = col_character()))

################################################################################

#Transformando variáveis em factor.

#1.Escolhemos o objeto e a variável;
#2.Utilizamos a função factor(nativa do R) que transforma códigos em seus
#verdadeiros significados;
#3.Preenchemos os argumentos da função. Sendo eles:
##x = nome do objeto e da variável;
##levels = os códigos em ordem;
##labels = os elementos representados pelos código em ordem.
base_PREp$municipio = factor(x = base_PREp$municipio,
                             levels = c(1,2,3),
                             labels = c("Salvador","São Paulo","Belo Horizonte"))

base_PREp$descontinuou = factor(x = base_PREp$descontinuou,
                                levels = c(0,1),
                                labels = c("Não","Sim"))

base_PREp$populacao = factor(x = base_PREp$populacao,
                             levels = c(1,2),
                             labels = c("Homem que faz sexo com homem","Mulher trans"))

base_PREp$discriminacao = factor(x = base_PREp$discriminacao,
                                 levels = c(0,1),
                                 labels = c("Não","Sim"))

base_PREp$mora_familia = factor(x = base_PREp$mora_familia,
                                levels = c(0,1),
                                labels = c("Não","Sim"))

base_PREp$violencia_sexual = factor(x = base_PREp$violencia_sexual,
                                    levels = c(0,1),
                                    labels = c("Não","Sim"))

base_PREp$uso_camisinha = factor(x = base_PREp$uso_camisinha,
                                 levels = c(1,2,3),
                                 labels = c("Raramente","Ocasionalmente", "Sempre"))

base_PREp$pessoas_velhas = factor(x = base_PREp$pessoas_velhas,
                                  levels = c(0,1),
                                  labels = c("Não","Sim"))

base_PREp$sexo_grupo = factor(x = base_PREp$sexo_grupo,
                              levels = c(0,1),
                              labels = c("Não","Sim"))

base_PREp$teste_HIV = factor(x = base_PREp$teste_HIV,
                             levels = c(0,1),
                             labels = c("Não","Sim"))

base_PREp$usou_PEP = factor(x = base_PREp$usou_PEP,
                            levels = c(0,1),
                            labels = c("Não","Sim"))

################################################################################

#TÓPICO 1

#Vendo o resumo de todas as variáveis
summary(base_PREp)

################################################################################

#TÓPICO 2

#1.Nomeamos o objeto;
#2.Selecionamos o objeto que será utilizado como base;
#3.Usamos a função "select" para escolher as variáveis;
#4.Usamos a função "filter" para filtrar as linhas;

#Município Salvador
base_Salvador = base_PREp |> 
  select(municipio, discriminacao, descontinuou, populacao, teste_HIV,
         escolaridade, depressao, qualidade_sono, ansiedade) |> 
  filter(municipio == "Salvador")

#Município São Paulo
base_SP = base_PREp |> 
  select(municipio, discriminacao, descontinuou, populacao, teste_HIV,
         escolaridade, depressao, qualidade_sono, ansiedade) |> 
  filter(municipio == "São Paulo")

#Município Belo Horizonte
base_BH = base_PREp |> 
  select(municipio, discriminacao, descontinuou, populacao, teste_HIV,
         escolaridade, depressao, qualidade_sono, ansiedade) |> 
  filter(municipio == "Belo Horizonte")

################################################################################

#TÓPICO 3

base3 = base_PREp |>
  select(idade, depressao, ansiedade, qualidade_sono, risco) |> 
  group_by(idade) |> 
  summarise(depressao_media = mean(depressao, na.rm = TRUE),
            ansiedade_media = mean(ansiedade, na.rm = TRUE),
            qualidade_sono_media = mean(qualidade_sono, na.rm = TRUE),
            risco_medio = mean(risco, na.rm = TRUE))

#Aparecendo uma linha a+

################################################################################

#TÓPICO 4

base4 = base_PREp |> 
  select(discriminacao, uso_camisinha, idade, escolaridade, depressao, ansiedade,
         qualidade_sono, risco)|> 
  group_by(discriminacao, uso_camisinha) |> 
  summarise(idade_media = mean(idade, na.rm = TRUE),
            escolaridade_media = mean(escolaridade, na.rm = TRUE),
            depressao_media = mean(depressao, na.rm = TRUE),
            ansiedade_media = mean(ansiedade, na.rm = TRUE),
            qualidade_sono_media = mean(qualidade_sono, na.rm = TRUE),
            risco_medio = mean(risco, na.rm = TRUE))
#...

################################################################################

#TÓPICO 5

base_Ocasionalmente_mais_Raramente = base_PREp |>
  select(uso_camisinha, risco, sexo_grupo) |>
  filter(uso_camisinha == c("Ocasionalmente" , "Raramente"))

base_Ocasionalmente = base_PREp |>
  select(uso_camisinha, risco, sexo_grupo) |>
  filter(uso_camisinha == "Ocasionalmente")

base_Raramente = base_PREp |>
  select(uso_camisinha, risco, sexo_grupo) |>
  filter(uso_camisinha == "Raramente")


rm(base_PREp)

#Está aparecendo menos indivíduos do que deveria

################################################################################
summary(base_Salvador)
summary(base_SP)
summary(base_BH)
summary(base_PREp)
base_PREp
