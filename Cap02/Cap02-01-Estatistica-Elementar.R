# Lab - Estatística Elementar em R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Desktop/DSA/manga-business-analytics/Cap02")
getwd()


# Dataset
install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)

# Melhorando o nome do dataset
dataset <- PimaIndiansDiabetes
View(dataset)

# Dimensões e tipos dos dados
?dim
dim(dataset)
str(dataset)

# Sumário dos dados
summary(dataset)
summary(dataset$age)

# A média de uma variável é uma medida numérica da localização central dos valores dos dados. 
# É a soma de seus valores dividido pela contagem de dados.
mean(dataset$age)  
mean(dataset$age, na.rm = TRUE)  
is.na(dataset$age)

# A mediana de uma variável é o valor no meio quando os dados são classificados em ordem crescente. 
# É uma medida ordinal da localização central dos valores de dados.
median(dataset$age) 

# Existem vários quartis de uma variável. O primeiro quartil, ou quartil inferior, é o valor 
# que corta os primeiros 25% dos dados quando é classificado em ordem crescente. 
# O segundo quartil, ou mediana, é o valor que corta os primeiros 50%. 
# O terceiro quartil, ou quartil superior, é o valor que corta os primeiros 75%.
quantile(dataset$age)
quantile(dataset$age,.75)     
quantile(dataset$age, c(.32, .57, .98)) 

# O intervalo (range) de uma variável é a diferença de seus maiores e menores valores de dados. 
# É uma medida de quão distante os dados se espalham.
range(dataset$age)

# A variação interquartil de uma variável é a diferença de seus quartis superior e inferior. 
# É uma medida de quão distante a parte média dos dados se espalha.
IQR(dataset$age)

# A variância é uma medida numérica de como os valores dos dados estão dispersos em torno da média.
var(dataset$age)

# O desvio padrão de uma variável é a raiz quadrada de sua variância.
# O desvio padrão mede a dispersão dos seus dados (quão os dados estão distantes da média).
sd(dataset$age) 

# A covariância de duas variáveis x e y em um conjunto de dados mede como as duas variáveis 
# estão linearmente relacionadas. 
# Uma covariância positiva indicaria uma relação linear positiva entre as variáveis, e uma 
# covariância negativa indicaria o contrário.
cov(dataset$age, dataset$glucose)

# O coeficiente de correlação de duas variáveis em um conjunto de dados é igual a sua covariância 
# dividida pelo produto de seus desvios-padrão individuais. É uma medida normalizada de como os 
# dois estão linearmente relacionados. Os valores vão de -1 a +1. Valores próximos de zero indicadm 
# que não há correlação. Valor de -1 indica forte correlação negativa e +1 forte correlação positiva.
cor(dataset$age, dataset$glucose)
correlacoes <- cor(dataset[,1:8])
print(correlacoes)

install.packages("corrplot")
library(corrplot)
corrplot(correlacoes, method = "circle")


# Pacote para as funções skewness e kurtosis
install.packages("e1071")
library(e1071) 

# Em estatística, a assimetria (skewness) é uma medida da distorção da distribuição de 
# probabilidade de uma variável aleatória sobre sua média. Em outras palavras, a assimetria 
# informa a quantidade e a direção da inclinação (partida de simetria horizontal). 

# O valor de assimetria pode ser positivo ou negativo ou ainda indefinido. Se a assimetria é 0, 
# os dados são perfeitamente simétricos, embora seja bastante improvável para dados do mundo real. 

# Via de regra:

# Se a assimetria é menor que -1 ou maior que 1, a distribuição é altamente distorcida.
# Se a assimetria é entre -1 e -0,5 ou entre 0,5 e 1, a distribuição é enviesada moderadamente.
# Se a assimetria é entre -0,5 e 0,5, a distribuição é aproximadamente simétrica.

# A medida positiva indicaria que a média dos valores dos dados é maior do que a mediana e 
# a distribuição dos dados é desviada para a direita.
# Uma medida negativa indica que a média dos valores dos dados é menor que a mediana e a 
# distribuição dos dados é inclinada para a esquerda. 
?skewness
skewness(dataset$age)
hist(dataset$age)

skewness(dataset$pressure)
summary(dataset$pressure)
hist(dataset$pressure)

# A curtose informa a altura e a nitidez do pico central, em relação a uma curva de sino padrão.
# A distribuição normal tem kurtosis igual a zero.
kurtosis(dataset$age) 
hist(dataset$age)

kurtosis(dataset$pressure)
hist(dataset$pressure)

# Exercício
# Considere os dados de exemplo a seguir.
# Calcule as estatísticas elementares e interprete o resultado da assimetria e da curtose.
# A distribuição dos dados é normal?

# Dados de exemplo
set.seed(140)
dados <- rnorm(n = 10000, mean = 55, sd = 4.5)
View(dados)

# Transformando a variavel dados de double para dataframe
dataset <- data.frame(dados)
View(dataset)
summary(dataset)

# Renomeando a coluna
colnames(dataset) <- c("Medida")
View(dataset)

# Estatísticas elementares
mean(dataset$Medida)
median(dataset$Medida)
var(dataset$Medida)
sd(dataset$Medida)

# Moda
table(as.vector(dataset$Medida))
table(as.vector(round(dataset$Medida)))
moda <- table(as.vector(round(dataset$Medida)))
names(moda)[moda == max(moda)]

# Sumário
summary(dataset)

# Assimetria e Curtose
library(e1071)

skewness(dados)
kurtosis(dados)

# Funcao describe
library(psych)
describe(dados)

########## Assimetria ########## 

# Assimetria é o grau de afastamento de uma distribuição da unidade de assimetria.
# Uma Distribuição é Simétrica quando seus valores de Média, Mediana e Moda coincidem. 
# A Assimetria, dá, portanto, uma indicação da inclinação da distribuição.

# O coeficiente de assimetria (skewness) permite distinguir as distribuições assimétricas. 
# Um valor negativo indica que a cauda do lado esquerdo da função densidade de probabilidade 
# é maior que a do lado direito. Um valor positivo para a assimetria indica que a cauda do 
# lado direito é maior que a do lado esquerdo. Um valor nulo indica que os valores são 
# distribuídos de maneira relativamente iguais em ambos os lados da média, mas não implica 
# necessariamente, uma distribuição simétrica.

# A assimetria dos dados simulados é negativa e próxima de zero. 
# Isso conclui que os dados estão próximos de uma distribuição gaussiana (formato de sino), 
# mas ligeiramente inclinados para a esquerda. 


########## Curtose ########## 

# Curtose é  uma medida de dispersão que caracteriza o "achatamento" da curva da função 
# de distribuição. O que significa analisar um conjunto quanto à Curtose?

# Significa apenas verificar o “grau de achatamento da curva”. Ou seja, saber se a Curva de 
# Frequência que representa o conjunto é mais “afilada” ou mais “achatada” em relação a uma 
# Curva Padrão, chamada de Curva Normal! 

# Uma curva (um conjunto) poderá ser, quanto à sua Curtose: 

# Mesocúrtica: ou de curtose média! 
# Será essa a nossa Curva Normal. “Meso” lembra meio! Esta curva está no meio termo: 
# nem muito achatada, nem muito afilada;

# Platicúrtica: é a curva mais achatada. Seu desenho lembra o de um prato emborcado. 
# Então “prato” lembra “plati” e “plati” lembra “platicúrtica”;

# Leptocúrtica: é a curva mais afilada! 

# Quando se trata de Curtose, não há como extrairmos uma conclusão sobre qual será a situação 
# da distribuição – se mesocúrtica, platicúrtica ou leptocúrtica – apenas conhecendo os valores 
# da Média, Moda e Mediana. 

# Histograma
library(ggplot2)

# Plot
ggplot(dataset, aes(x = dataset$Medida), binwidth = 20) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Dados de Exemplo'))) + 
  ylab(expression(bold('Densidade')))
