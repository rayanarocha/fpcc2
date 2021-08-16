
library(arules)
library(dplyr)
library(sciplot)
library(MASS)
library(TeachingDemos)
library(rcompanion)
library(tidyverse)
library(animation)

##LENDO DATAFRAMES
df_2012 = read.csv('DADOS_NT/dados2012.csv', header = TRUE, sep = ";")
df_2013 = read.csv('DADOS_NT/dados2013.csv', header = TRUE, sep = ";")
df_2014 = read.csv('DADOS_NT/dados2014.csv', header = TRUE, sep = ";")
df_2015 = read.csv('DADOS_NT/dados2015.csv', header = TRUE, sep = ";")
df_2016 = read.csv('DADOS_NT/dados2016.csv', header = TRUE, sep = ";")
df_2017 = read.csv('DADOS_NT/dados2017.csv', header = TRUE, sep = ";")


summary(df_2012);


#CRIANDO ARQUIVOS CSV PARA AS REGRAS GERADAS
write.table(df_2012, file = "DADOS_NT/dados2012.csv", sep = ",", row.names = TRUE, col.names = NA)
write.table(df_2013, file = "DADOS_NT/dados2013.csv", sep = ",", row.names = TRUE, col.names = NA)
write.table(df_2014, file = "DADOS_NT/dados2014.csv", sep = ",", row.names = TRUE, col.names = NA)
write.table(df_2015, file = "DADOS_NT/dados2015.csv", sep = ",", row.names = TRUE, col.names = NA)
write.table(df_2016, file = "DADOS_NT/dados2016.csv", sep = ",", row.names = TRUE, col.names = NA)
write.table(df_2017, file = "DADOS_NT/dados2017.csv", sep = ",", row.names = TRUE, col.names = NA)



plot(density(df_2015$IDADE, adjust=2), lty="dashed", col="green", ylim=c(0,0.048), xlim=c(0,90), xlab="Idades", ylab="Densidade", main = "Representa??o das idades das v?timas (2015 - 2017)", lwd=2)
lines(density(df_2016$IDADE, adjust=2), lty="dashed", col="yellow", lwd=2)
lines(density(df_2017$IDADE, adjust=2), lty="dashed", col="blue", lwd=2)
curve(expr = dnorm(x,mean=mean(idades_gerais),sd= sd(idades_gerais, na.rm= FALSE)), add = TRUE, col="black")

legend(55, 0.04, legend=c("2015", "2016", "2017", "Curva Normal"),
       col=c("green", "yellow", "blue", "red"), lty=c(2,2,2,1), cex=0.8)

#CRIANDO UM VETOR COM TODAS AS IDADES

curve(expr = dnorm(x,mean=mean(idades_gerais),sd= sd(idades_gerais, na.rm= FALSE)), add = TRUE, col="red")


##CRIANDO FILTROS DE HORÁRIOS MAIS VIOLENTOS
df_geral <- read.csv('DADOS_NT/dados_att2.csv', header = TRUE, sep = ";")

filtro_2012 <- filter(df_geral, df_geral$ANO == 2012 & 
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$GENERO == "MASCULINO" & df_geral$ZONA == "Norte")

plot(filtro_2012$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia", 
     ylab = "Frequência", main= "Horários - regras 2012", col="blue")


##GERANDO UM FILTRO NAS HORAS PARA AS REGRAS DE 2013

filtro_2013 <- filter(df_geral, df_geral$ETNIA =="PARDA" &
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$TURNO == "NOTURNO" & df_geral$FAIXA == "ADULTO" &
                        df_geral$GENERO =="MASCULINO" & df_geral$ANO == 2013)


plot(filtro_2013$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2013", col="blue")

##GERANDO UM FILTRO NAS HORAS PARA AS REGRAS DE 2014

filtro_2014 <- filter(df_geral, df_geral$ZONA == "Norte" &
                        df_geral$TURNO == "NOTURNO" &
                        df_geral$GENERO =="MASCULINO" & df_geral$ANO == 2014)

plot(filtro_2014$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2014", col="blue")

##GERANDO FILTRO PARA 2015
filtro_2015 <- filter(df_geral, df_geral$ANO == 2015 & 
                        df_geral$TURNO == "NOTURNO" &
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO")


plot(filtro_2015$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2015", col="blue")


filtro_2016 <- filter(df_geral, df_geral$ANO == 2016 & 
                        df_geral$FAIXA == "ADULTO" & df_geral$ZONA == "Norte" 
                        & df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO"
                        & df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO"
                        & df_geral$TURNO == "NOTURNO")

##GERANDO FILTRO PARA 2016
plot(filtro_2016$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2016", col="blue")


##GERANDO FILTRO PARA 2017
filtro_2017 <- filter(df_geral, df_geral$ANO == 2017 & 
                        df_geral$TURNO == "NOTURNO" & df_geral$ZONA == "Oeste" 
                      & df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO"
                      & df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &df_geral$FAIXA == "ADULTO")


da<- sort(filtro_2017$HORARIO.APROXIMADO, decreasing = TRUE)
##GERANDO FILTRO PARA 2017

plot(filtro_2017$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2017", col="blue")


#------------------------------------------------------------------------------------------------------#
##CRIANDO VARIAVEL PARA A FREQUENCIA DE DIAS DA SEMANA E BAIRROS
#2012
  ##CRIANDO VARIAVEL PARA 2012
dia_da_semana_2012.freq <- table(filtro_2012$DIA.DA.SEM)
barplot(dia_da_semana_2012.freq[order(dia_da_semana_2012.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2012")
  
  ##APRESENTA OS BAIRROS MAIS FREQ?NTES PARA A REGRA
bairro_2012.freq <- table(filtro_2012$BAIRRO.DA.OCORRENCIA)
barplot(bairro_2012.freq[order(bairro_2012.freq, decreasing = T)], xlab = "Bairro", ylab = "Quantidade", col = "blue", main = "Ano 2012")
bairro_2012.freq


##2013
dia_da_semana_2013.freq <- table(filtro_2013$DIA.DA.SEM)
barplot(dia_da_semana_2013.freq[order(dia_da_semana_2013.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2013")


bairro_2013.freq <- table(filtro_2013$BAIRRO.DA.OCORRENCIA)
barplot(bairro_2013.freq[order(bairro_2013.freq, decreasing = T)], xlab = "Bairro", ylab = "Quantidade", col = "blue", main = "Ano 2013")
#plot(bairro_2013.freq)


#2014

dia_da_semana_2014.freq <- table(filtro_2014$DIA.DA.SEM)
barplot(dia_da_semana_2014.freq[order(dia_da_semana_2014.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2014")


bairro_2014.freq <- table(filtro_2014$BAIRRO.DA.OCORRENCIA)
bairro_2014.freq


#2015

dia_da_semana_2015.freq <- table(filtro_2015$DIA.DA.SEM)
barplot(dia_da_semana_2015.freq[order(dia_da_semana_2015.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2015")


bairro_2015.freq <- table(filtro_2015$BAIRRO.DA.OCORRENCIA)
bairro_2015.freq

#2016

dia_da_semana_2016.freq <- table(filtro_2016$DIA.DA.SEM)
barplot(dia_da_semana_2016.freq[order(dia_da_semana_2016.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2016")


bairro_2016.freq <- table(filtro_2016$BAIRRO.DA.OCORRENCIA)
bairro_2016.freq

#2017

dia_da_semana_2017.freq <- table(filtro_2017$DIA.DA.SEM)
barplot(dia_da_semana_2017.freq[order(dia_da_semana_2017.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2017")


bairro_2017.freq <- table(filtro_2017$BAIRRO.DA.OCORRENCIA)
bairro_2017.freq

##CRIANDO FILTROS DE TIPOS DE MORTE

morte_2012 <- filter(df_geral, df_geral$ANO == 2012 & 
                       df_geral$TIPO.DE.MORTE == "HOMICIDIO" & df_geral$ZONA == "Oeste" 
                     & df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO"
                     & df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &df_geral$FAIXA == "ADULTO")

barplot(morte_2012$TIPO.DE.MORTE, main="Tipo", xlab = "Genero", ylab = "Morte", col = "blue", main = "Ano 2012")

#------------------------------------------------------------------------------------------------------#
##VERIFICANDO IC DAS IDADES

head(df_geral);

idade.df_geral = df_geral$IDADE
idade.df_geral

base::mean(idade.df_geral, na.rm=TRUE)
#média das idades: 27.77247

var(idade.df_geral, na.rm=TRUE)
#116.685

idade.response = na.omit(df_geral)
n = length(idade.response)
sigma = 9.84
sem = sigma/sqrt(n); sem

ggplot(df_geral, aes(y=IDADE, x=GENERO))+
  geom_point(aes(color=GENERO))

ggplot(df_geral, aes(y=IDADE, x=ETNIA))+
  geom_point(aes(color=ETNIA))

ggplot(df_geral, aes(y=INSTRUMENTO.UTILIZADO, x=TIPO.DE.MORTE))+
  geom_point(aes(color=TIPO.DE.MORTE))

ggplot(df_geral, aes(y=INSTRUMENTO.UTILIZADO, x=PROFISSAO))+
  geom_point(aes(color=PROFISSAO))

ggplot(df_geral, aes(GENERO, IDADE)) +
  geom_boxplot(aes(fill=GENERO)) +
  theme(legend.position = "none")

ggplot(df_geral, aes(ETNIA, IDADE)) +
  geom_boxplot(aes(fill=ETNIA)) +
  theme(legend.position = "none")

ggplot(df_geral, aes(INSTRUMENTO.UTILIZADO, TIPO.DE.MORTE)) +
  geom_boxplot(aes(fill=INSTRUMENTO.UTILIZADO)) +
  theme(legend.position = "none")

lineplot.CI(GENERO, IDADE, data = df_geral, xlab = "GENERO", ylab = "IDADE")

ggplot(df_geral, aes(DIA.DA.SEMANA, TIPO.DE.MORTE)) +
  geom_boxplot(aes(fill=TIPO.DE.MORTE)) +
  theme(legend.position = "none")

 
#_________________________________________________________________________________________________
#quantile(df$IDADE)



