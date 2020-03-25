m.effects <- function (object, covariate = NULL, type = c("aa", "ar", "rr", 
                                                          "ra"), data = NULL, ...) 
{
  type <- match.arg(type)
  if (is.null(data)) {
    P <- m.predict(object, returnData = TRUE)
    data <- attr(P, "data")
    attr(P, "data") <- NULL
  }
  else P <- m.predict(object, data)
  newdata <- data
  J <- length(P)
  alt.levels <- names(P)
  pVar <- substr(type, 1, 1)
  xVar <- substr(type, 2, 2)
  cov.list <- strsplit(as.character(attr(formula(object), "rhs")), " + ", fixed = TRUE)
  rhs <- sapply(cov.list, function(x) length(na.omit(match(x, 
                                                           covariate))) > 0)
  rhs <- (1:length(cov.list))[rhs]
  eps <- 1e-05
  if (rhs %in% c(1, 3)) {
    if (rhs == 3) {
      theCoef <- paste(alt.levels, covariate, sep = ":")
      theCoef <- coef(object)[theCoef]
    }
    else theCoef <- coef(object)[covariate]
    me <- c()
    for (l in 1:J) {
      newdata[l, covariate] <- data[l, covariate] + eps
      newP <- m.predict(object, newdata)
      me <- cbind(me, (newP - P)/eps)
      newdata <- data
    }
    if (pVar == "r")
      me <- t(t(me)/P)
    if (xVar == "r") 
      me <- me * matrix(rep(data[[covariate]], J), J)
    dimnames(me) <- list(alt.levels, alt.levels)
  }
  if (rhs == 2) {
    newdata[, covariate] <- data[, covariate] + eps
    newP <- m.predict(object, newdata)
    me <- (newP - P)/eps
    if (pVar == "r") 
      me <- me/P
    if (xVar == "r") 
      me <- me * data[[covariate]]
    names(me) <- alt.levels
  }
  me
}
