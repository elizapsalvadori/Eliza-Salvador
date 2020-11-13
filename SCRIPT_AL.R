### ARTIGO ERNANI - DOUTORADO
#### INDEPENDENCIA E ACCOUNTABILITY JUDICIAL NA AMERICA LATINA


### ABRIR BANCO DE DADOS

library(readxl)
dados_AL <- read_excel("Doutorado/Tr�s Poderes na Am�rica Latina/ARTIGO FINAL/dados_AL.xlsx")
View(dados_AL)

## TEMAS BRANCOS
install.packages("sjPlot")
install.packages("ggplot")
theme_set(theme_sjplot())

## INSTALANDO PACOTES
install.packages("readxl")
install.packages("ggplot2")

### TRANSFORMANDO VARIAVEIS EM NUMERICAS 

idhnovo <- as.numeric(dados_AL$idh)

gininovo <- as.numeric(dados_AL$gini)

pibnovo <- as.numeric(dados_AL$pib) 

### CRIAR VARIAVEL ELEVADA AO QUADRADO - INDEPENDENCIA JUDICIAL

dados_AL$inde_jud <- dados_AL$inde_jud^2


### CRIAR VARIAVEL NORMAL - INDEPENDENCIA JUDICIAL

dados_AL$jud <-sqrt(dados_AL$inde_jud)


## CHAMANDO A BASE
attach(dados_AL)

###### DEFASANDO VARIAVEL DEPENDENTE 

## ATIVANDO PACOTES NECESSARIOS

library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)

dados_AL$dep_lag <-Lag(dados_AL$per_corrup�ao, -1)
summary(dados_AL$dep_lag)

#### ESTATISTICA DESCRITIVA DA VD - PERCEPÇAO DE CORRUPÇAO 
summary(dados_AL$dep_lag)
summary(dados_AL$per_corrup�ao)

### GRAFICO DE BARRA DA VD - PERCEPÇAO DE CORRUPÇAO

library(ggplot2)
library(sjPlot)
library(tidyverse)
attach(dados_AL)

ggplot(data = dados_AL, aes(x = dep_lag)) +
  geom_histogram(bins=50, fill='black', color ='black')+
  ggtitle("Percepção de Corrupção")

ggplot(dados_AL, aes(x=dep_lag)) +
  geom_histogram(binwidth = .05) +
  theme_sjplot() +
  xlab("Percepção de Corrupção") +
  ylab("Frequência")


### ESTATISTICA DESCRITIVA - INDEPENDENCIA JUDICIAL - ELEVADA AO QUADRADO

dados_AL$inde_jud <- dados_AL$inde_jud^2
summary(dados_AL$inde_jud)
library(pander)
summary(dados_AL$inde_jud)
pander(summary(dados_AL$inde_jud))

### ESTATISTICA DESCRITIVA - INDEPENDENCIA JUDICIAL - VALOR NORMAL
pander(summary(dados_AL$jud))
pander(summary(dados_AL$inde_jud))



#### CORRELAÇAO VD + VI (INDE JUD)
attach(dados_AL)
library(pander)
library(stargazer)
cor(per_corrupçao, inde_jud)
pander(cor.test(per_corrupçao, inde_jud), caption= 
         "Correlação Independência Judicial")

### GRAFICO VD + VI QUADRATICO

ggplot(dados_AL, aes(x= inde_jud, y=per_corrupçao)) + geom_point() +
  theme_sjplot() +
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= "Independência Judicial", y= "Percepção da Corrupção")


### GRAFICO VI + VD QUADRATICO FACTOR (ANO)
ggplot(dados_AL[dados_AL$Ano != 2012,], aes(x = inde_jud, y = per_corrupçao)) + 
  geom_point(aes(colour = factor(Ano))) +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), colour = 'red') +
  geom_smooth(method = 'lm') +
  labs(x= 'Independência Judicial', y= 'Percepção da Corrupção')



### ESTATISTICA DESCRITIVA VD + VI - ACCOUNTABILITY

pander(summary(dados_AL$v2juaccnt_osp))


### CORRELAÇAO VD + VI - ACCOUNTABILITY

library(pander)
library(stargazer)
cor(per_corrupçao, v2juaccnt_osp)
pander(cor.test(per_corrupçao, v2juaccnt_osp), 
       caption = "Correlação Accountability Judicial")






### GRAFICO DE DISPERSï¾ã° VI + VD - ACCOUNTABILITY JUDICIAL
library(ggplot2)
library(sjPlot)
attach(dados_AL)


ggplot(dados_AL, aes(x = v2juaccnt_osp, y = per_corrupçao,
                               colour = factor(Ano))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percepção de Corrupção")

ggplot(dados_AL, aes(x = v2juaccnt_osp, y = per_corrupçao, colour = factor(Ano))) + 
  geom_point() +
  theme_sjplot() +
  labs(x= "Accountability Judicial", y= "Percepção da Corrupção")+
  facet_wrap(~Ano)



### CORRELAÇAO ENTRE VI'S = ACC + IND
library(pander)
pander(cor.test(v2juaccnt_osp, inde_jud), 
       caption = "Correlação entre Variáveis Indpendentes")



#### REGRESSAO 1 - ESTIMANDO REGRESSAO DE PAINEL (SEM TERMO QUADRATICO)
### Instalando pacote necessario

install.packages('plm')

## ATIVANDO PACOTES NECESSARIOS
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)
library(sjPlot)

#### RODANDO A REGRESSAO 1 - APENAS INDEPENDENCIA JUDICIAL, SEM V2JUA
attach(dados_AL)
library(lme4)
library(plm)
attach(dados_AL)
regressao1 <- plm(data = dados_AL, per_corrup�ao ~ inde_jud + jud +
                    democracia + log(pibnovo) + idhnovo + gininovo +dep_lag, 
                  cluster = 'Ano',
                  model = 'within', index = 'Ano')
summary(regressao1)
regressao1  


### GERANDO GRAFICO COEFPLOT REGRESSAO 1 
library(coefplot)
par(mfrow=c(2,2))
plot(regressao1)
coefplot(regressao1) +
  theme_sjplot()


#### RODANDO A REGRESSAO 2 - SEM TERMO QUADRATICO E ACCOUNTABILITY JUD
### ATIVANDO PACOTES 
library(sjPlot)
library(jtools)
library(tidyverse)
library(plm)
library(readxl)
library(gdata)
library(lme4)
library(Hmisc)
library(plm)

regressao2 <- plm(data = dados_AL, per_corrupçao ~ v2juaccnt_osp +
                    democracia + log(pibnovo) + idhnovo + gininovo +dep_lag, 
                  cluster = 'Ano',
                  model = 'within', index = 'Ano')
summary(regressao2)
regressao2


### GERANDO GRAFICO COEFPLOT REGRESSAO 2 
library(coefplot)
par(mfrow=c(2,2))
plot(regressao2)
coefplot(regressao2) +
  theme_sjplot()


#### RODANDO REGRESSAO 3 - EFEITOS FIXOS E DUAS VARIAVEIS INDEPENDENTES 
attach(dados_AL)
regressao3 <- plm(data = dados_AL, per_corrupçao ~ inde_jud +  jud +v2juaccnt_osp +
                    democracia + log(pibnovo) + idhnovo + gininovo +dep_lag, 
                  cluster = 'Ano',
                  model = 'within', index = 'Ano')
summary(regressao3)
regressao3

### GERANDO GRAFICO COEFPLOT REGRESSAO 3 
library(coefplot)
par(mfrow=c(2,2))
plot(regressao3)
coefplot(regressao3) +
  theme_sjplot()

#### RODANDO REGRESSAO 4 - EFEITO ALEATORIO COM TERMO QUADRATICO

regressao4 <- plm(data = dados_AL, per_corrupçao ~ inde_jud + jud + v2juaccnt_osp +
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = 'Ano', 
                   model = 'random', index = 'Ano')
summary(regressao4)
regressao4
coefplot(regressao4) +
  theme_sjplot()

### RODANDO REGRESSAO 5 - EFEITO ALEATORIO SEM TERMO QUADRATICO


regressao5 <- plm(data = dados_AL, per_corrupçao ~ jud + v2juaccnt_osp +
                  democracia + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                  cluster = 'Ano', 
                  model = 'random', index = 'Ano')
summary(regressao5)
regressao5
coefplot(regressao5) +
  theme_sjplot()


#### TESTE DE HAUSMAN - QUAL EFEITO APLICAR

library(pander)
phtest(regressao3, regressao4)
pander(phtest(regressao3, regressao4))



#### RODANDO REGRESSAO 6 - MODELO POOLED 

regressao6 <- plm(data = dados_AL, per_corrupçao ~ inde_jud + jud + v2juaccnt_osp + 
                     democracia
                   + log(pibnovo) + idhnovo + gininovo + dep_lag, 
                   cluster = 'Ano', 
                   model = 'pooling', index = 'Ano')
summary(regressao6)
regressao6
library(sjPlot)
coefplot(regressao6)+
  theme_sjplot()


#### TESTE LAGRANGE BREUSCH-PAGAN
plmtest(regressao6, type = "bp")
pander(plmtest(regressao6, type = "bp"))


### TESTE DE BREUSCH-GODFREY - AUTOCORRELAÇAO REGRESSAO EFEITO ALEATORIO

install.packages('carData')
library(car)
lmtest::bgtest(regressao4)
pander(lmtest::bgtest(regressao4))

### TESTE DE BREUSCH-GODFREY - AUTOCORRELAÇAO MODELO POOLDED
lmtest::bgtest(regressao6)
pander(lmtest::bgtest(regressao6))



