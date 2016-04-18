
#' Simple Gradient Boosting
#'
#'
#' Fit a simple, educational implementation of tree-based gradient boosting model.
#'
#'@import rpart
#'@export
#'
#'@param formula an object of class "\link{formula}" with a response but no interaction terms.
#'The response variable should be a binomial factor that has values of \code{1} for a positive response
#' or \code{-1} for a negative or lack of response.
#'@param data the dataframe containing the independent variables and the response
#'@param iterations The number of training rounds for boosting.
#'@param verbose should the current training round be printed to the console?
#'@return An S3 object of class \code{boosted}. This includes
#'@examples
#'k <- kyphosis
#'k$Kyphosis <- factor(ifelse(k$Kyphosis == "present", 1L, -1L))
#'fit <- fitBoosted(Kyphosis ~ Age + Number + Start, data = k, iterations = 10)
#'
fitBoosted <- function(formula, data, iterations = 100, verbose = TRUE){
  formula <- as.formula(formula)
  outcome <- data[,as.character(formula[[2]])]
  numobs <- nrow(data)
  err <- numeric(iterations)
  alpha <- numeric(iterations)
  w <<- rep((1 / numobs), times = numobs)
  modelfit <- vector(mode = "list", length = iterations)
  predicted <- vector(mode = "list", length = iterations)
  weightsOut <- vector(mode = "list", length = iterations)
  prob <- vector(mode = "list", length = iterations)
  data <- data.frame(w, data)

  for(i in 1:iterations){
    if(verbose){
      cat("Solving iteration", i, "\n")
    }
    modelfit[[i]] <- rpart(formula, data = data[, -1], weights = w, control = rpart.control(maxdepth = 2))
    #predicted[[i]] <- ifelse(fitted(modelfit[[i]]) > 0.5, 1L, -1L)
    predicted[[i]] <- predict(modelfit[[i]], type = "class")
    prob[[i]] <- predict(modelfit[[i]], type = "prob")[, "1"]

    ind <- as.integer(predicted[[i]] != outcome)
    err[i] <- sum(w * ind)
    alpha[i] <- log((1 - err[i]) / err[i])
    w <<- w * exp(alpha[i] * ind)
    w <<- w / sum(w)
    data$w <- w
    weightsOut[[i]] <- w
  }
  # Find if the prediction matches the actual for every iteration
  alphamat <- matrix(rep(alpha, each = numobs), nrow = numobs, iterations)
  fitmat <- matrix(as.numeric(unlist(predicted)), nrow = numobs, ncol = iterations)
  #fitmat <- matrix(unlist(prob), nrow = numobs, ncol = iterations)
  g <- alphamat * fitmat
  #finalpredictions <- rowMeans(g)
  finalpredictions <- rowSums(g)
  #classpredictions <- ifelse(finalpredictions > 0.5, 1L, -1L)
  classpredictions <- ifelse(finalpredictions > 0, 1L, -1L)
  return(structure(list(fitted = classpredictions,
                        fittedProb = finalpredictions,
                        formula = formula,
                        data = data, fittedModels = modelfit,
                        modelPredictions = predicted, finalWights = w,
                        alpha = alpha, weights = weightsOut,
                        outcome = outcome, error = err),
                   class = "boosted"))
}



#' Is the Object a Boosted Model
#'
#' Test the inheritance of an object
#'@export
#'@param x any \code{R} object
#'@return \code{TRUE} if the object is a boosted model
#'
#'
#'
is.boosted <- function(x){
  return(inherits(x, "boosted"))
}

#'Model Predictions
#'
#'Apply a fitted \code{boosted} model to newdata to form predictions. If no \code{newdata} is included, returned the fitted values of the model.
#'
#'@export
#'@param object a \code{boosted} model returned from \link{fitBoosted}
#'@param newdata the new independent variables to use for prediction. This should be a data frame.
#'@param ... additional arguments affecting the predictions produced (ignored).
#'@return \code{predict.boosted} produces a numeric vector with the predicted classes from the \code{boosted} model.
#'
predict.boosted <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    return(fitted(object))
  }
  newdata <- cbind(w = 1 / nrow(newdata), newdata)
  numobs <- nrow(newdata)
  numweights <- length(object$alpha)
  predicted <- vector("list", length = numobs)
  #resultsmat <- numeric(numobs)
  pred <- as.data.frame(sapply(object$fittedModels,
                               FUN = function(x) predict(x, newdata = newdata[, -1])[, "1"]))
  alphamat <- matrix(rep(object$alpha, each = nrow(pred)), ncol = numweights)
  finalpred <- rowMeans(alphamat * pred)
  classpred <- ifelse(finalpred > 0.5, 1L, -1L)
#
#   amat <- matrix(rep(alpha, each = numobs), nrow = numobs)
#   fitmat <- matrix(as.numeric(unlist(predicted)), nrow = numobs)
#   g <- alphamat * fitmat
#   finalpredictions <- rowSums(g)
#   classpredictions <- ifelse(finalpredictions > 0.0, 1, -1)
  return(classpred)
}
