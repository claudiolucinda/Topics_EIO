## Accompanying materials to the Online Appendix to
## "Private returns to R\&D in the presence of spillovers, revisited"
## Forthcoming, Journal of Applied Econometrics
## Giovanni Millo, September 14th 2018

## COMFAC test routine, ported from Mans Soderbom's Stata code
## Giovanni Millo, 13/11/2013, see /ReplicateEberhardt/../MD_ar1
##
## Check against Eberhardt at al., pooled model w/o any effect:
##
## Stata> reg lny lnl l.lnl lnk l.lnk lnrd l.lnrd l.lny
## Stata> md_ar1, nx(3) beta(e(b)) cov(e(V))
##
## R> mod <- plm(dfm, data=ehs, model="pooling")
## R> comfactest(mod)
##
## FE model, standard covariance
##
## Stata> xtreg lny lnl l.lnl lnk l.lnk lnrd l.lnrd l.lny, fe
## Stata> md_ar1, nx(3) beta(e(b)) cov(e(V))
##
## R> mod <- plm(dfm, data=ehs, model="within")
## R> comfactest(mod)

comfactest <- function(x, vcov.=vcov, k=length(coef(x))) {

 ## NB assumes following order:
 ## y = Ly + x1 + Lx1 + x2 + Lx2 + ... + xK + LxK + <optional>

 ## extract coefs and vcov from model object
 ## (k limits the coefs for models like pmg, or if including dummies etc.)
 b <- coef(x)[1:k]
 vb <- vcov.(x)[1:k, 1:k]

 ## eliminate intercept if any
 if(names(b)[1] == "(Intercept)") {
    b <- b[-1]
    vb <- vb[-1,-1]
 }

 ## reorder coefficients and vcovs as in Soderbom
 b <- c(b[-1], b[1])
 vb <- vb[c(2:length(b), 1), c(2:length(b), 1)]

 ## extract gamma, which is "special", and name it
 ga <- b[length(b)]

 ## ncoef from static model
 k <- (length(b)-1)/2

 ## initialize h as two stacked ones
 h <- matrix(rep(1,2), ncol=1)
 ## if k>1 replicate structure
 if(k>1) {
  for(i in 2:k) {
    ## contour h with zeros
    h <- cbind(h, rep(0, dim(h)[[1]]))
    h <- rbind(h, matrix(0, nrow=2, ncol=dim(h)[[2]]))
    ## put two stacked ones in lower right corner
    rh <- dim(h)[[1]]
    ch <- dim(h)[[2]]
    h[(rh-1):rh, ch] <- 1
  }
 }
 ## contour h with last row and column of zeros
 h <- cbind(h, rep(0, dim(h)[[1]]))
 h <- rbind(h, rep(0, dim(h)[[2]]))
 ## put one in lower right corner
 h[dim(h)[[1]], dim(h)[[2]]] <- 1

 ## make (restricted?) coefficients' vector g
 g <- b
 ## divide even elements by -gamma
 #g[(1:length(g)) %% 2 == 0] <- g[(1:length(g)) %% 2 == 0]/(-g[length(g)])
 g[(1:length(g)) %% 2 == 0] <- g[(1:length(g)) %% 2 == 0]/(-ga)

 ## make gg
 ## initialize gg
 gg <- numeric(0)
 ## add two appropriate rows, k times
 ## (notice how in R this step can be coded as generic w/o having
 ## to separate cases k=1, j=1 etc. because one can rbind an empty
 ## matrix)
 for(j in 1:k) {
    newg <- matrix(nrow=2, ncol=2*k+1)
    newg[1,] <- c(rep(0, 2*(j-1)), 1, rep(0, 2*(k-j+1)))
    newg[2,] <- c(rep(0, 2*(j-1)+1), -1/ga, rep(0, 2*(k-j)),
                  b[2*j]/ga^2)
    gg <- rbind(gg, newg)
 }
 ## add last row, relative to gamma: all zeros (2k) plus 1 at the end
 gg <- rbind(gg, c(rep(0, dim(gg)[[2]]-1), 1))

 ## calc. estimated (restricted) parms and covariance
 omega <- gg %*% tcrossprod(vb, gg)
 aa <- solve(crossprod(h, solve(omega, h)))
 ## already solved aa, but this is numerically stabler
 theta <- solve(crossprod(h, solve(omega, h)),
                crossprod(h, solve(omega, g)))

 ## make se, t-ratios, p.values
 se <- sqrt(diag(aa))
 ttest <- theta/se
 pval <- 2*pnorm(abs(ttest), lower.tail=FALSE)

 coeftab <- cbind(theta, se, ttest, pval)
 dimnames(coeftab) <- list(c(names(b)[1:((length(b)-1)/2)*2-1],
                             names(b)[length(b)]),
                           c("Estimate", "Std. Error", "t value",
                             "Pr(>|t|)"))

 ## COMFAC test
 ght <- g-h%*%theta
 comfacstat <- crossprod(ght, solve(omega, ght))
 comfacprob <- pchisq(comfacstat, df=k, lower.tail=FALSE)
 comfactest <- list(statistic=comfacstat, p.value=comfacprob)

 return(list(model=coeftab, test=comfactest))
}

