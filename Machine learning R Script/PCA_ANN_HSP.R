# Discriminant analysis
require(neuralnet)
require(nnet)
require(ggplot2)
require(MASS)
require(dplyr)
require(pls)
require("factoextra")

# Data
dados<-read.csv("G:/Meu Drive/Projeto/Resultados/Model_13.csv",header=TRUE)
dados$Outcome<-as.factor(dados$Outcome)
#dados[,-c(ncol(dados))] <- scale(dados[,-c(ncol(dados))])
str(dados)

# Data partition
set.seed(555)
proportion <- 0.995
u <- ncol(dados) - 1
y <- ncol(dados)
t <- ncol(dados) + 2

pca <- prcomp(dados[c(1:u)], center = TRUE, scale = TRUE)
summary(pca)
pr_var = ( pca$sdev )^2 
prop_varex = pr_var / sum( pr_var )


for (i in 1:u){
	if (prop_varex[i] >= 0.01){
		pcomp <- i
	}
}

# Creating a new dataset
dadosN = data.frame(pca$x)
dadosN = data.frame(dadosN[,1:pcomp], Outcome = dados$Outcome)
u <- ncol(dadosN) - 1
y <- ncol(dadosN)
t <- ncol(dadosN) + 2
# Encode as a one hot vector multilabel data
dadosNN<- cbind(dadosN[,1:u], class.ind(as.factor(dadosN$Outcome)))
# Set labels name
names(dadosNN) <- c(names(dadosN)[1:u],"G","I","S")
# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
dadosNN[,1:u] <- data.frame(lapply(dadosNN[,1:u], scl))
str(dadosNN)

# Train test split proportions
proportion <- 0.995
k <- nrow(dadosNN)
outs <- NULL
ins <- NULL
for (i in 1:u)
{
for (j in 1:k)
{
index <- sample(1:nrow(dadosNN), round(proportion*nrow(dadosNN)))
train <- dadosNN[index, ]
test <- dadosNN[-index, ]
# Set up formula
n <- names(dadosNN)
f <- as.formula(paste("G + I + S ~", paste(n[!n %in% c("G","I","S")], collapse = " + ")))

nn <- neuralnet(f,
                data = train,
                hidden = i,
                stepmax = 1e+10, threshold=0.05,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal", rep = 1)


# Compute predictions

pr.nn <- predict(nn, test[, 1:u])

# Extract results
#pr.nn_ <- pr.nn

# Accuracy (Test set)
original_values <- max.col(test[, y:t])
pr.nn_2 <- max.col(pr.nn)
outs[c(i,j)] <- mean(pr.nn_2 == original_values)
}
ins[i] <- mean(outs[c(i,1:k)])
}

ins
i = which.max(ins)
ins[i]
nn <- neuralnet(f,
                data = dadosNN,
                hidden = i,
                stepmax = 1e+10, threshold=0.05,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal", rep = 10)
r = which.min(nn$result.matrix[1,])
plot(nn , rep = r,radius = 0.1, arrow.length = 0.2,
	show.weights = TRUE, dimension = 6,  fontsize = 12)