# Discriminant analysis
require(MASS)
require(dplyr)
require(ggplot2)
require(pls)
required("factoextra")

# Data
dados<-read.csv("G:/Meu Drive/Projeto/Resultados/Model_4.csv",header=TRUE)
dados$Outcome<-as.factor(dados$Outcome)
#dados[,-c(ncol(dados))] <- scale(dados[,-c(ncol(dados))])
str(dados)

# Data partition
set.seed(555)
proportion <- 0.995
k <- nrow(dados)
l <- ncol(dados) -1
outs <- NULL

dados.pr <- prcomp(dados[c(1:l)], center = TRUE, scale = TRUE)
summary(dados.pr)
cumpro <- cumsum(dados.pr$sdev^2 / sum(dados.pr$sdev^2))
mean(cumpro)


fviz_pca_ind(dados.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = dados$Outcome, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Outcome")