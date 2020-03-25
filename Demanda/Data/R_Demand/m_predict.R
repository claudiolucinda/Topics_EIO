m.predict<-function (object, newdata = NULL, returnData = FALSE, ...) 
{
  if (is.null(newdata)) 
    newdata <- mean(model.frame(object))
  if (!inherits(newdata, "mlogit.data")) {
    rownames(newdata) <- NULL
    lev <- colnames(object$probabilities)
    J <- length(lev)
    choice.name <- attr(model.frame(object), "choice")
    if (nrow(newdata)%%J) 
      stop("the number of rows of the data.frame should be a multiple of the number of alternatives")
    nchid <- nrow(newdata)%/%J
    attr(newdata, "index") <- data.frame(chid = factor(rep(1:nchid, 
                                                           each = J)), alt = factor(rep(lev, nchid)))
    attr(newdata, "class") <- c("mlogit.data", "data.frame")
    if (is.null(newdata[["choice.name"]])) {
      newdata[[choice.name]] <- FALSE
      newdata[[choice.name]][1] <- TRUE
    }
  }
  m <- match(c("choice", "shape", "varying", "sep", "alt.var", 
               "chid.var", "alt.levels", "opposite", "drop.index", 
               "id", "ranked"), names(object$call), 0L)
  if (sum(m) > 0) 
    object$call <- object$call[-m]
  newobject <- update(object, start = coef(object, fixed = TRUE), 
                      data = newdata, iterlim = 0, print.level = 0)
  result<-newobject$probabilities
  if (is.null(nrow(result))) {
    result <- as.matrix(result)
    result<-t(result)
  }
  if (nrow(result) == 1) {
    result <- as.numeric(result)
    names(result) <- colnames(object$probabilities)
    
  }
  if (returnData) 
    attr(result, "data") <- newdata
  result
}