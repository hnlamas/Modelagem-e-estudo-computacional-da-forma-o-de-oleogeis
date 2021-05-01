# load libs
require(neuralnet)
require(nnet)
require(ggplot2)
set.seed(1)

# Data
dados<-read.csv("G:/Meu Drive/Projeto/Resultados/Model_12.csv", header=TRUE)
u <- ncol(dados) - 1
y <- ncol(dados)
t <- ncol(dados) + 2


# Encode as a one hot vector multilabel data
dadosN<- cbind(dados[,1:u], class.ind(as.factor(dados$Outcome)))
# Set labels name
names(dadosN) <- c(names(dados)[1:u],"G","I","S")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
dadosN[,1:u] <- data.frame(lapply(dadosN[,1:u], scl))
str(dadosN)

# Train test split proportions
proportion <- 0.995
k <- nrow(dadosN)
outs <- NULL
ins <- NULL
for (i in 1:u)
{
for (j in 1:k)
{
index <- sample(1:nrow(dadosN), round(proportion*nrow(dadosN)))
train <- dadosN[index, ]
test <- dadosN[-index, ]
# Set up formula
n <- names(dadosN)
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
                data = dadosN,
                hidden = i,
                stepmax = 1e+10, threshold=0.05,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal", rep = 10)
r = which.min(nn$result.matrix[1,])
plot(nn , rep = r,radius = 0.1, arrow.length = 0.2,
	show.weights = TRUE, dimension = 6,  fontsize = 12)