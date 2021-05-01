# Discriminant analysis
require(MASS)
require(dplyr)
require(ggplot2)
require(ellipse)
	
# Data
dados<-read.csv("G:/Meu Drive/Projeto/Resultados/Model_13.csv",header=TRUE)
dados$Outcome<-as.factor(dados$Outcome)
#dados[,-c(ncol(dados))] <- scale(dados[,-c(ncol(dados))])
str(dados)

# Data partition
set.seed(555)
proportion <- 0.995
k <- nrow(dados)
outs <- NULL

for (j in 1:k)
{
index <- sample(1:nrow(dados), round(proportion*nrow(dados)))
train <- dados[index, ]
test <- dados[-index, ]


lda.train <- lda(formula=Outcome~.,data=train,na.action="na.omit")
lda.train.p <- predict(lda.train,train)

# Confusion matrix and accuracy - testing data
p2 <- predict(lda.train, test)$class
tab1 <- table(Predicted = p2, Actual = test$Outcome)
outs[j] <- sum(diag(tab1))/sum(tab1)
}
mean(outs)
lda.train <- lda(formula=Outcome~.,data=dados,na.action="na.omit")
lda.train
lda.data <- cbind(dados, predict(lda.train)$x)

dat_ell <- data.frame()
for(g in levels(lda.data$Outcome)){
dat_ell <- rbind(dat_ell, cbind(as.data.frame(with(lda.data[lda.data$Outcome==g,], ellipse(cor(LD1, LD2), 
                                         scale=c(sd(LD1),sd(LD2)), 
                                         centre=c(mean(LD1),mean(LD2))))),Outcome=g))
}

ggplot(lda.data, aes(x=LD1, y=LD2, col=Outcome) ) + 
	geom_point( size = 3, aes(color = Outcome))+theme_bw()+
	geom_path(data=dat_ell,aes(x=x,y=y,color=Outcome),size=1,linetype=1)



