calculate.output.vector <- function(data.matrix,weights.matrix,bias){
  net <- (data.matrix %*% weights.matrix) - bias
  return(1 / (1 + exp(-net)))
}

calculate.output.layer.deltas <- function(target.vector,output.layer.output.vector){
  return(output.layer.output.vector*(1 - output.layer.output.vector)*(target.vector - output.layer.output.vector))
}


calculate.hidden.layer.deltas <- function(hidden.layer.output.vector,output.layer.weights.matrix,output.layer.deltas){
  return(hidden.layer.output.vector*(1 - hidden.layer.output.vector)*(output.layer.deltas %*% t(output.layer.weights.matrix)))
}

recalling <- function(hidden.layer.weights.matrix,hidden.layer.bias,output.layer.weights.matrix,output.layer.bias,testing.data){
  testing.result <- c()
  print(data)
  for (i in 1:nrow(testing.data)) {
    data.matrix <- testing.data[i,]
    hidden.layer.output.vector <- calculate.output.vector(data.matrix,hidden.layer.weights.matrix,hidden.layer.bias)
    output.layer.output.vector <- calculate.output.vector(hidden.layer.output.vector,output.layer.weights.matrix,output.layer.bias)
    print(round(output.layer.output.vector))
    testing.result <- c(testing.result,output.layer.output.vector)
  }
  
  testing.data <- cbind(testing.data,matrix(testing.result,ncol = 3,byrow = TRUE))
  return(testing.data)
}

training <- function(data,training.rate,hidden.nodes){
  output.nodes <- 1
  hidden.layer.weights.matrix <- matrix(runif((ncol(data) - output.nodes)*hidden.nodes),ncol = hidden.nodes); #matrix(c(0.5,0.3,0.25,0.6,0.1,0.4),ncol = hidden.nodes) 
  hidden.layer.bias <- matrix(runif(hidden.nodes),nrow = 1)
  output.layer.weights.matrix <- matrix(runif(hidden.nodes*output.nodes),ncol = output.nodes); #matrix(runif(0.1,0.25,0.5,0.2,0.15,0.05),ncol = output.nodes) m
  output.layer.bias <- matrix(runif(output.nodes),nrow = 1)
  count <- 0
  all.train.error <- c()
  repeat {
    count <- count + 1
    misclassification.amount <- 0
    train.error.sum <- 0
    train.error <- 0
    for (i in 1:nrow(data)) {
      data.matrix <- matrix(data[i,1:(ncol(data) - output.nodes)], nrow = 1)
      label <- unname(data[i,(ncol(data) - (output.nodes - 1)):ncol(data)])
      hidden.layer.output.vector <- calculate.output.vector(data.matrix,hidden.layer.weights.matrix,hidden.layer.bias)
      output.layer.output.vector <- calculate.output.vector(hidden.layer.output.vector,output.layer.weights.matrix,output.layer.bias)
      train.error <-  ((label - output.layer.output.vector)^2)/2
      train.error.sum <- train.error.sum + ((label - output.layer.output.vector)^2)/2
      
      output.layer.deltas <- calculate.output.layer.deltas(label,output.layer.output.vector)
      hidden.layer.deltas <- calculate.hidden.layer.deltas(hidden.layer.output.vector,output.layer.weights.matrix,output.layer.deltas)
      output.layer.weights.deltas <- traing.rate * (t(hidden.layer.output.vector) %*% output.layer.deltas)
      output.layer.bias.deltas <- -traing.rate*output.layer.deltas
      hidden.layer.weights.deltas <- traing.rate * (t(data.matrix) %*% hidden.layer.deltas)
      hidden.layer.bias.deltas <- -traing.rate*hidden.layer.deltas
      #update output layer
      output.layer.weights.matrix <- output.layer.weights.matrix + output.layer.weights.deltas
      output.layer.bias <- output.layer.bias + output.layer.bias.deltas
      #update hidden layer
      hidden.layer.weights.matrix <- hidden.layer.weights.matrix + hidden.layer.weights.deltas
      hidden.layer.bias <- hidden.layer.bias + hidden.layer.bias.deltas
    }
    all.train.error[count] <- train.error.sum/nrow(data)
    if (train.error < 0.05 || count > 10000) {
      break
    }
  }
  return(list(hidden.layer.weights.matrix,hidden.layer.bias,output.layer.weights.matrix,output.layer.bias,all.train.error))
}

################# XOR #################
x <- c(0,0,1,1)
y <- c(0,1,0,1)
label <- c(0,1,1,0)
data <- cbind(x,y,label)
traing.rate <- 0.05
hidden.nodes <- 4
training.result <- training(data,traing.rate,hidden.nodes)
#error function
subtitle <- sprintf("training rate: %s  hidden nodes: %s",traing.rate,hidden.nodes)
plot(training.result[[5]],type = "l",main="stop criterion:  training error < 0.05 or interations > 10000",sub = subtitle,xlab = "interations",ylab = "error")
# <- recalling(traing.result[[1]],traing.result[[2]],traing.result[[3]],traing.result[[4]],cbind(x,y))
#######################################



