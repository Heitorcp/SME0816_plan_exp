# Quatro projetos de circuito de computador
# Análise com os dados completos
# variável resposta: número de ruídos

y<-c(19, 20, 19, 30, 8,
     80, 61, 73, 56, 80,
     47, 26, 25, 35, 50,
     95, 46, 83, 78, 97)

trat <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))


trat <- as.factor(trat)

dad <- data.frame(trat,y)



str(dad)

boxplot(split(dad$y,dad$trat))

tapply(dad$y,dad$trat,var)

tapply(dad$y,dad$trat,length)

mod <- lm(y ~ trat,data=dad)

res <- rstudent(mod)

par(mfrow=c(1,2),pty="s")
plot(fitted(mod),residuals(mod),pch=19,col="blue")
abline(h=0,col="red")

qqnorm(res)
qqline(res)
layout(1)


par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod)
# normal plot with simulated envelope
hnp(mod, half = F)

# Testes
shapiro.test(rstudent(mod))
bartlett.test(y ~ trat, data=dad)

# As pressuposições do modelo não foram satifeitas
# a hipótese de normalidade dos dados foi rejeitada

# Verificou-se a existência de um outlier.
tapply(y,trat,summary)
(media <- tapply(y,trat,mean))
(dp    <- tapply(y,trat,sd))

data.frame(trat,y,res)
# Observe no tratamento 4 o resíduo da obs. 46
# 17    4 46 -3.75308520
#-------------------------------------------

y.m <- c(
     19, 20, 19, 30, 8,
     80, 61, 73, 56, 80,
     47, 26, 25, 35, 50,
     95, 83, 78, 97)    # A observação 46 foi retirada

trat.m <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,4))
# Observe que o tratamento 4 passou a ter 4 repetições

trat.m <- as.factor(trat.m)

dad.m <- data.frame(trat.m,y.m)
length(dad$trat.m)
length(dad$y.m)
str(dad.m)

boxplot(split(dad.m$y.m,dad.m$trat.m))

tapply(dad.m$y.m,dad.m$trat.m,var)

mod.m <- lm(y.m ~ trat.m,data=dad.m)

res.m <- rstudent(mod.m)

par(mfrow=c(1,2),pty="s")
plot(fitted(mod.m),residuals(mod.m),pch=19,col="blue")
abline(h=0,col="red")

qqnorm(res.m)
qqline(res.m)
layout(1)


par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod.m)
# normal plot with simulated envelope
hnp(mod.m, half = F)

# Testes
shapiro.test(rstudent(mod.m))
bartlett.test(y.m ~ trat.m, data=dad.m)


# As suposições básicas do modelo foram atendidas
# Vamos interpretar o quadro da ANOVA

anova(mod.m)

# Verifica-se que existe efeito de tratamento, ou ainda, 
# que existe diferenças estatisticamente significantes 
# ao nível alpha=5% em pelo menos um contraste de médias.
# valor P = 1.106e-07 < 0,05.

anova(lm(y.m ~ trat.m))

# Continuando a análise aplica-se um teste de 
# comparações múltiplas

require(laercio)
aov.Tukey <- aov(y.m ~ trat.m,data=dad.m)
LTukey(aov.Tukey ,"trat",conf.level=0.95)

# Concluindo-se que: 
# não existem diferenças entre os projetos 
# de circuitos de computadores (4,2) e (3,1).

# Sendo que o projeto que apresenta o menor 
# número de ruídos é o projeto 1

#**************************************************

# Outra caminho poderia ser tomado, ao invés de retirar 
# o valor 46, observe que esse ponto só poderá ser retirado
# se o pesquisador permitir e justificar que foi um erro.

# Caso não seja possível retirar o ponto? O que fazer?
# Procurar uma transformação de dados
# Leia Variance-Stabilizing Transformations (Montgomery)

# Para tentar solucionar o problema, admite-se que exista
# um relacionamento entre sigma e mu, isto é, que o desvio padrão 
# seja proporcional a uma função não linear de lambda, ou ainda,
# sigma = theta mu^lambda
# linearizando tem-se:
# log(sigma) = log(theta) + log(mu^lambda)
# log(sigma) = log(theta) + lambda log(mu)

# Dessa maneira o y transformado será dado por: y^lambda
# Justificativa: Leia artigo disponibilizado

# Como obter lambda???
# lambda é o coeficiente angular da regressão 
#
# log(dp) = a + b log(mu)
#
# Veja Montgomery pág 84 ( 8a. edição): 
#transformações mais utilizadas

# No exemplo apresentado:

media
dp

plot(log(media),log(dp),col="blue",pch=19)
(reg <- lm(log(dp) ~ log(media)))
abline(reg)

# Conclusão: lambda=0,5 (aproximação 0,4992)
# Portanto, o  y transformado, dentotado por, yt, será:
# yt = y^(0.5) 

#-----------------------
# Outra maneira para encontrar o lambda
#
require(MASS)

bc <- boxcox(y ~ trat,
       lambda = seq(-2.50, 5.50, length = 1000))
(lambda <- bc$x[which.max(bc$y)])

yt <- y^lambda

dad$y.t <- yt

dad
boxplot(split(dad$y.t,dad$trat))
       
#---------------------------------------

# Refazendo a análise


mod <- lm(yt ~ trat)
mod <- lm(yt ~ trat,data=dad)

res <- rstudent(mod)

par(mfrow=c(1,2),pty="s")
plot(fitted(mod),residuals(mod),pch=19,col="blue")
abline(h=0,col="red")

qqnorm(res)
qqline(res)
layout(1)


par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod)
# normal plot with simulated envelope
hnp(mod, half = F)

# Testes

bartlett.test(yt ~ trat, data=dad)
shapiro.test(rstudent(mod))

# As suposições básicas do modelo foram atendidas
# Vamos interpretar o quadro da ANOVA

anova(mod)

# Verifica-se que existe efeito de tratamento, ou ainda, 
# que existe diferenças estatisticamente significantes 
# ao nível alpha=5% em pelo menos um contraste de médias.
# valor P = 1.106e-07 < 0,05.


# Continuando a análise aplica-se um teste de 
# comparações múltiplas

require(laercio)
aov.Tukey <- aov(y ~ trat,data=dad)
LTukey(aov.Tukey ,"trat",conf.level=0.95)

# Concluindo-se que: 
# não existem diferenças entre os projetos 
# de circuitos de computadores (4,2) e (3,1).

# Sendo que o projeto que apresenta o menor 
# número de ruídos é o projeto 1


#*************************************************
# Montgomery destaca que a falta de normalidade não afeta
# muito a análise de variância. Destaca, tb, que a falta de 
# homogeneidade sim.

# De fato, nesse exemplo em particular, a continuidade da 
# análise sem a suposição de normalidade dos erros não conduz 
# conclusões diferentes das obtidas. Observe,


anova(mod)

# Verifica-se que existe efeito de tratamento, ou ainda, 
# que existe diferenças estatisticamente significantes 
# ao nível alpha=5% em pelo menos um contraste de médias.
# valor P = 1.106e-07 < 0,05.


# Continuando a análise aplica-se um teste de 
# comparações múltiplas

require(laercio)
aov.Tukey <- aov(y ~ trat,data=dad)
LTukey(aov.Tukey ,"trat",conf.level=0.95)

# Concluindo-se que: 
# não existem diferenças entre os projetos 
# de circuitos de computadores (4,2) e (3,1).

# Sendo que o projeto que apresenta o menor 
# número de ruídos é o projeto 1

