sigmoid <- function(net){
  1 / (1 + exp(-net))
}

calculate.output.vector <- function(data.matrix, weights.matrix, bias){
  net <- (data.matrix %*% weights.matrix) - bias
  sigmoid(net)
}

calculate.output.layer.deltas <- function(target.vector, output.layer.output.vector){
  output.layer.output.vector * (1 - output.layer.output.vector) * (target.vector - output.layer.output.vector)
}

calculate.hidden.layer.deltas <- function(layer.output.vector, next.layer.weights.matrix, next.layer.deltas){
  layer.output.vector * (1 - layer.output.vector) * (next.layer.deltas %*% t(next.layer.weights.matrix))
}

forward.propagation <- function(input.matrix, weights.list, bias.list){
  activations <- list(input.matrix)
  for (i in seq_along(weights.list)) {
    activations[[i + 1]] <- calculate.output.vector(activations[[i]], weights.list[[i]], bias.list[[i]])
  }
  activations
}

recalling <- function(weights.list, bias.list, testing.data){
  testing.result <- c()
  output.nodes <- ncol(weights.list[[length(weights.list)]])

  for (i in 1:nrow(testing.data)) {
    data.matrix <- matrix(testing.data[i, ], nrow = 1)
    activations <- forward.propagation(data.matrix, weights.list, bias.list)
    output.layer.output.vector <- activations[[length(activations)]]
    print(round(output.layer.output.vector))
    testing.result <- c(testing.result, output.layer.output.vector)
  }

  cbind(testing.data, matrix(testing.result, ncol = output.nodes, byrow = TRUE))
}

training <- function(data, training.rate, hidden.nodes){
  output.nodes <- 1
  input.nodes <- ncol(data) - output.nodes

  if (length(hidden.nodes) == 0) {
    stop("hidden.nodes 至少要有一層")
  }

  network.nodes <- c(input.nodes, hidden.nodes, output.nodes)
  layer.amount <- length(network.nodes) - 1

  weights.list <- lapply(1:layer.amount, function(i) {
    matrix(runif(network.nodes[i] * network.nodes[i + 1]), nrow = network.nodes[i], ncol = network.nodes[i + 1])
  })
  bias.list <- lapply(1:layer.amount, function(i) {
    matrix(runif(network.nodes[i + 1]), nrow = 1)
  })

  count <- 0
  all.train.error <- c()

  repeat {
    count <- count + 1
    train.error.sum <- 0
    train.error <- 0

    for (i in 1:nrow(data)) {
      data.matrix <- matrix(data[i, 1:input.nodes], nrow = 1)
      label <- matrix(unname(data[i, (input.nodes + 1):ncol(data)]), nrow = 1)

      activations <- forward.propagation(data.matrix, weights.list, bias.list)
      output.layer.output.vector <- activations[[length(activations)]]

      train.error <- ((label - output.layer.output.vector)^2) / 2
      train.error.sum <- train.error.sum + train.error

      deltas <- vector("list", layer.amount)
      deltas[[layer.amount]] <- calculate.output.layer.deltas(label, output.layer.output.vector)

      if (layer.amount > 1) {
        for (layer in (layer.amount - 1):1) {
          deltas[[layer]] <- calculate.hidden.layer.deltas(
            activations[[layer + 1]],
            weights.list[[layer + 1]],
            deltas[[layer + 1]]
          )
        }
      }

      for (layer in 1:layer.amount) {
        weights.deltas <- training.rate * (t(activations[[layer]]) %*% deltas[[layer]])
        bias.deltas <- -training.rate * deltas[[layer]]

        weights.list[[layer]] <- weights.list[[layer]] + weights.deltas
        bias.list[[layer]] <- bias.list[[layer]] + bias.deltas
      }
    }

    all.train.error[count] <- train.error.sum / nrow(data)
    if (train.error < 0.05 || count > 10000) {
      break
    }
  }

  list(weights.list = weights.list, bias.list = bias.list, all.train.error = all.train.error)
}

################# XOR #################
x <- c(0, 0, 1, 1)
y <- c(0, 1, 0, 1)
label <- c(0, 1, 1, 0)
data <- cbind(x, y, label)
training.rate <- 0.05
hidden.nodes <- c(4, 3)
training.result <- training(data, training.rate, hidden.nodes)

# error function
subtitle <- sprintf("training rate: %s  hidden nodes: %s", training.rate, paste(hidden.nodes, collapse = ","))
plot(training.result$all.train.error,
     type = "l",
     main = "stop criterion: training error < 0.05 or iterations > 10000",
     sub = subtitle,
     xlab = "iterations",
     ylab = "error")

# recalling(training.result$weights.list, training.result$bias.list, cbind(x, y))
#######################################
