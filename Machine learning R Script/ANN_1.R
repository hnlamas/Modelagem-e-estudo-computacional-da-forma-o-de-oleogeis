# load libs
require(neuralnet)
require(nnet)
require(ggplot2)
set.seed(1)

# Data
dados<-read.csv(file="RNA_HSP.csv",header=TRUE)

# Encode as a one hot vector multilabel data
dadosN<- cbind(dados[, 1:6], class.ind(as.factor(dados$Outcome)))
# Set labels name
names(dadosN) <- c(names(dados)[1:6],"G","S","I")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
dadosN[, 1:6] <- data.frame(lapply(dadosN[, 1:6], scl))
head(dadosN)

# Train test split proportions
proportion <- 0.995
k <- nrow(dadosN)
outs <- NULL
ins <- NULL
for (i in 1:10)
{
for (j in 1:k)
{
index <- sample(1:nrow(dadosN), round(proportion*nrow(dadosN)))
train <- dadosN[index, ]
test <- dadosN[-index, ]
# Set up formula
n <- names(dadosN)
f <- as.formula(paste("G + S + I ~", paste(n[!n %in% c("G","S","I")], collapse = " + ")))
f

nn <- neuralnet(f,
                data = train,
                hidden = i,
                stepmax = 1e+5, threshold=0.05,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal", rep = 1)


# Compute predictions

pr.nn <- predict(nn, test[, 1:6])

# Extract results
#pr.nn_ <- pr.nn

# Accuracy (Test set)
original_values <- max.col(test[, 7:9])
pr.nn_2 <- max.col(pr.nn)
outs[c(i,j)] <- mean(pr.nn_2 == original_values)
}
ins[i] <- mean(outs[c(i,1:k)])
}
r <- which.max(ins)
nn <- neuralnet(f,
                data = dadosN,
                hidden = c(3),
                stepmax = 1e+6,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal", rep = 3)

plot(nn, rep = which.min(nn$result.matrix[1,]))
