# ================================================================
# Code for Models in Ellickson and Misra      
# Estimating Discrete Games
# Marketing Science (Invited Paper)
# July, 2011
# (c) Sanjog Misra & Paul Ellickson
# ================================================================
#
# The file implement the following estimators
#
# Complete Information Games
#	Bresnahan and Reiss (1990, 1991)
#	Berry (1992) - with three alternate identification strategies
#
# Incomplete Information Games
#	Nested Fixed Point (Rust 1994; Seim 2006)
#	2-Step Estimators (BHNK 2010; Ellickson and Misra 2008)
#	Nested Pseudo Likelihood (Aguirregabiria 2002)
#
# Data
#	The data used in the paper is from Jia (2008)
#	and is available from the Econometrica website.
# ================================================================


setwd("G:\\Meu Drive\\Aulas\\GV\\Curso de OI - Pós\\Mini Curso USP\\Topics_EIO\\Entrada\\Data\\")
# Read Data in
jiadat <-read.csv("jiadata2R.csv",header=T)

# ================================================================
# Complete Information - Model 1
# Bresnahan and Reiss 
# Simple Number of firms Estimator
# ================================================================

# Observed number of Firms

nf = jiadat$nfirms;

# Relevant X matrix
# Notice that since the firms are assumed identical no 
# firm specific covariates are used 

ints = rep(1,nrow(jiadat)); # For intercept
xmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban);

# Define Negative Log Likelihood

BR.nllik <- function(zeta)
{
# Setup parameters

	k = ncol(xmat);
	theta = zeta[1:k];
	delta = exp(zeta[k+1]); # We exponentiate delta to ensure that competiton reduces profits
		
# Define Profits
	
	Wprofit = xmat%*%theta;
	Kprofit = Wprofit; # Because of exchangability assumption
	
# Probability that duopoly profits are > 0

	probW.duo = pnorm(Wprofit-delta);
	probK.duo = probW.duo; # Errors are iid and identities dont matter
	
# Probability of seeing duopoly
	
	prob.duo = probW.duo*probK.duo;
	
# Probability that Monopoly profits are < 0
	
	probW.zero = pnorm(-Wprofit);
	probK.zero = probW.zero; # Errors are iid and identities dont matter
	
# Probability of seeing no entrants

	prob.zero = probW.zero*probK.zero;
	
# Probability of seeing 1 stores
	
	prob.mon = 1 - prob.zero - prob.duo;
	
# Check for numerical issues
	prob.zero[prob.zero<=0] = 1E-10
	prob.mon[prob.mon<=0] = 1E-10
	prob.duo[prob.duo<=0] = 1E-10
	
# Construct and return negative log likelihood
	llik = (nf==0)*log(prob.zero) + (nf==1)*log(prob.mon) + (nf==2)*log(prob.duo) ;
	-sum(llik); 
}

# Now minimize nllik
start = rep(0,5);
BR.res = optim(start,BR.nllik,hessian=T,method="BFGS");

# Standard Error can be obtained the usual way
BR.se = sqrt(diag(solve(BR.res$hess)))

# delta needs to be exponentiated
delta.est = exp(BR.res$par[5])
# Standard Error using delta rule
delta.se = sqrt(exp(2*BR.res$par[5]))*BR.se[5]


# =============================================================
# Complete Information - Models 2-4
# Berry Estimators
# We implement three estimators
# (i)   Based on assumption that most profitable firm moves first
# (ii)  WalMart moves first 
# (iii) Kmart moves first
# =============================================================

# Covariates for Walmart and Kmart
nmkts = nrow(jiadat); # Number of markets
ints = rep(1,nmkts);  # intercepts

Wxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$dBenton,jiadat$southern);
Kxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$MidWest);

# Number of player specific parameters
kW = ncol(Wxmat);
kK = ncol(Kxmat);

# This method uses simulation
# Results will match table in paper is 
# you use R 2.13.0 and set the following seed:

set.seed(1234);

# Keep random draws fixed
# We have kept the draws low to speed up computation
# and encourage the user to play around with this to 
# examine rhe effect on estimates etc.

nreps = 1000; # Number of Monte Carlo Replications
uW = matrix(rnorm(nreps*nmkts),nmkts,nreps)
uK = matrix(rnorm(nreps*nmkts),nmkts,nreps)

# Create Dependent Vectors
WalMart = jiadat$WalMart
Kmart = jiadat$Kmart

duo = WalMart*Kmart	
Wm = WalMart*(1-Kmart)
Km =(1-WalMart)*Kmart
nofirm = 1 - duo - Wm - Km	

# Objective function definition

Berry.Obj <- function(theta)
{
# Certain values are taken from the Global environment
# move.rule = { WalMart, Kmart, profit }
# All dependent and Independent Var Vectors	
# duo,Wm,Km, nofirm .. Wxmat 	
# set parameters

	theta.W = c(theta[1],theta[3:5],theta[6:7]);
	theta.K = c(theta[2],theta[3:5],theta[8]);
	delta   = exp(theta[9]); # We exponentiate to ensure competitive effects are negative
	
# Deterministic component of profits
    
	pi.W = Wxmat%*%theta.W;
    pi.K = Kxmat%*%theta.K;
	
	
# We use analytical probabilities 
# This will be difficult for more than two players or if richer error structures are involved
# In which case Monte-Carlo approaches proposed in Brerry may be used	
# Entry under assumption that WalMart moves first
			if(move.rule == "WalMart") {	
				p0.an = pnorm(-pi.W)*pnorm(-pi.K)
				p2.an = pnorm(pi.W-delta)*pnorm(pi.K-delta)
				pw.an = pnorm(pi.W)*pnorm(-pi.K + delta)
				pk.an = 1- p0.an - p2.an - pw.an;
			}
# Entry under assumption that Kmart moves first
			if(move.rule == "Kmart") {  
				p0.an = pnorm(-pi.W)*pnorm(-pi.K)
				p2.an = pnorm(pi.W-delta)*pnorm(pi.K-delta)
				pk.an = pnorm(pi.K)*pnorm(-pi.W + delta)
				pw.an = 1- p0.an - p2.an - pk.an;
			}
# Entry under assumption that the more profitable firm moves first
			if(move.rule == "profit") { 
				p0.an = pnorm(-pi.W)*pnorm(-pi.K)
				p2.an = pnorm(pi.W-delta)*pnorm(pi.K-delta)
				pw.an = pnorm(pi.W)*pnorm(-pi.K + delta) - (pnorm(-pi.W+delta) - pnorm(-pi.W))*(pnorm(-pi.K+delta) - pnorm(-pi.K))*(1 - pnorm((pi.K-pi.W)/2))
				pk.an = 1- p0.an - p2.an - pw.an
			}	
		
# Construct -LogLikelihood
# We use maximum Likelihood to ensure that the comparison to incomplete information  is fair
# Analytical Probabilities
			p.sim.duo = p2.an
			p.sim.W = pw.an
			p.sim.K = pk.an
			p.sim.0 = p0.an

# Check for numerical issues
	p.sim.duo[p.sim.duo<=0] = 1E-10
	p.sim.W[p.sim.W<=0] = 1E-10
	p.sim.K[p.sim.K<=0] = 1E-10
	p.sim.0[p.sim.0<=0] = 1E-10
	
# Log Likelihood
		llik = sum(sum(nofirm*log(p.sim.0) + duo*log(p.sim.duo) + Wm*log(p.sim.W) +Km*log(p.sim.K)))		
			
# Return Value
	-llik
	
}

# Estimation for each equilibrium selection approach
move.rule = "profit"
theta.start = c(-4.8960054, -15.3832747,   1.6653983,   0.9289151,   2.4720639,-1.4612322,   2.0311288 ,  2.1255131,   0.6889679);
Berry.pi.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# WalMart moves first
move.rule = "WalMart"
Berry.W.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# Kmart moves first
move.rule = "Kmart"
Berry.K.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# Standard Errors
Berry.pi.se = sqrt(diag(solve(Berry.pi.res$hess)))
Berry.W.se = sqrt(diag(solve(Berry.W.res$hess)))
Berry.K.se = sqrt(diag(solve(Berry.K.res$hess)))

# =============================================
# Incomplete Information Games - Model 1
# Nested Fixed point Algorithm
# =============================================

# Construct X matrices as before
nmkts = nrow(jiadat); # Number of markets
ints = rep(1,nmkts);  # intercepts

Wxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$dBenton,jiadat$southern);
Kxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$MidWest);

# Number of player specific parameters

kW = ncol(Wxmat);
kK = ncol(Kxmat);

# Dependent Vars
WalMart = jiadat$WalMart
Kmart = jiadat$Kmart

# Define NFXP negative log likelihood
nfxp.nll <- function(theta,pold.K = Kmart, pold.W = WalMart) {
	
# set parameters
	theta.W = c(theta[1],theta[3:5],theta[6:7]);
	theta.K = c(theta[2],theta[3:5],theta[8]);
	delta   = exp(theta[9]); # We exponentiate to ensure competitive effects are negative
	
# Deterministic component of profits
  	pi.W = Wxmat%*%theta.W;
    pi.K = Kxmat%*%theta.K;
	
# Do Nested Fixed Point Computation
	nfxp.reps=0; err = 10;
	
# User may want to play around with the tolerance to examine effect
# In principle as low a tolerance as possible should be used
# here we use  1E-12 	
# Note that the fixed point computation is done for all markets at once!
	
	while(err>1E-12 & nfxp.reps<10000) {
		nfxp.reps = nfxp.reps +1;
		pnew.W = pnorm(pi.W - delta*pold.K);
		pnew.K = pnorm(pi.K - delta*pnew.W);
		err = max(abs(pnew.K-pold.K)+abs(pnew.W-pold.W));
		pold.W = pnew.W;
		pold.K = pnew.K;
	}
	
# Check to see if reps are exhausted
# This might happen for certain guesses of parameters
	
	if(nfxp.reps>999) { stop("The number of NFXP reps needs to be increased. \n") }
	
# Construct Negative LogLikelihood
	ll = sum(WalMart*log(pold.W)+(1-WalMart)*log(1-pold.W) + Kmart*log(pold.K)+(1-Kmart)*log(1-pold.K))
	-ll
}

theta.start = c(-4.8960054, -15.3832747,   1.6653983,   0.9289151,   2.4720639,-1.4612322,   2.0311288 ,  2.1255131,   0.6889679);
nfxp.res = optim(theta.start,nfxp.nll,control=list(trace=10,maxit=5000),method="BFGS",hessian=T)
nfxp.se = sqrt(diag(solve(nfxp.res$hess)))

#=======================================
# Incomplete Information Games - Model 2
# Two Step Approach
#=======================================

# Construct X matrices as before

nmkts = nrow(jiadat); # Number of markets
ints = rep(1,nmkts);  # intercepts

Wxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$dBenton,jiadat$southern);
Kxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$MidWest);

# Number of player specific parameters
kW = ncol(Wxmat);
kK = ncol(Kxmat);

# Dependent Vars
WalMart = jiadat$WalMart
Kmart = jiadat$Kmart

# First Step - Nonparametric Binary Model
# Warning: Will take a looooong time
 library(np)
np.W = npreg(jiadat$WalMart~jiadat$population+jiadat$SPC+jiadat$urban+jiadat$dBenton+jiadat$southern)
np.K = npreg(jiadat$Kmart~jiadat$population+jiadat$SPC+jiadat$urban+jiadat$MidWest)
#CCPs from NP estiamtes
pred.W = as.matrix(fitted(np.W));
pred.K = as.matrix(fitted(np.K));

# First Step - SemiParametric Binary Models
# Faster than the Nonparametric approach 
# (also the one used in the paper)

# We will use the MGCV library 
library(mgcv);

# Implement GAM
gam.W = gam(WalMart~s(population,SPC)+s(population,dBenton)+s(SPC,dBenton)
+s(population,by=urban)+s(SPC,by=urban)+s(dBenton,by=urban)
+s(population,by=southern)+s(SPC,by=southern)+s(dBenton,by=southern)
+urban+southern,family="binomial",data=jiadat)

gam.K = gam(Kmart~s(population,SPC)+s(population,by=urban)+s(SPC,by=urban)
+s(population,by=MidWest)+s(SPC,by=MidWest)+urban+MidWest,family="binomial",data=jiadat)

# Now construct CCPs

pred.W = as.matrix(predict(gam.W,type="response"))
pred.K = as.matrix(predict(gam.K,type="response"))

# Parametric approach (Worth trying to see sresults)
# glm.W = glm(WalMart~population+SPC+urban+dBenton+southern,family="binomial")
# glm.K = glm(Kmart~population+SPC+urban+MidWest,family="binomial")
# pred.W = as.matrix(predict(glm.W,type="response"))
# pred.K = as.matrix(predict(glm.K,type="response"))

# Construct Pseudo nll - for second step

twostep.nll <- function(theta) {
	
# set parameters
	
	theta.W = c(theta[1],theta[3:5],theta[6:7]);
	theta.K = c(theta[2],theta[3:5],theta[8]);
	delta   = exp(theta[9]); # We exponentiate to ensure competitive effects are negative
	
# Deterministic component of profits
    
	pi.W = Wxmat%*%theta.W;
    pi.K = Kxmat%*%theta.K;
	
# Compute probabilities (Second Stage)
	
	prob.W = pnorm(pi.W - delta*pred.K);
	prob.K = pnorm(pi.K - delta*pred.W);
	
# Construct Negative LogLikelihood
	
	ll = sum(WalMart*log(prob.W)+(1-WalMart)*log(1-prob.W) + Kmart*log(prob.K)+(1-Kmart)*log(1-prob.K));
	-ll
}

theta = c(-4.8960054, -15.3832747,   1.6653983,   0.9289151,   2.4720639,-1.4612322,   2.0311288 ,  2.1255131,   0.6889679);
twostep.res = optim(theta,twostep.nll,control=list(trace=10,maxit=1000),method="BFGS")
delta.twostep  = exp(twostep.res$par[9])

# Setup for Bootstrap
nmkts = nrow(jiadat)
bootN  = 100;
theta = twostep.res$par
Boot.res = matrix(0,bootN,length(theta))

for( b in 1:bootN ) 
{
# Redefine Covariates for Walmart and Kmart
	jiadatBoot = jiadat[sample(1:nmkts,nmkts,replace=TRUE),]
	nmktsBoot = nrow(jiadatBoot); # Number of markets
	intsBoot = rep(1,nmktsBoot);  # intercepts
	
	Wxmat = cbind(intsBoot,jiadatBoot$population,jiadatBoot$SPC,jiadatBoot$urban,jiadatBoot$dBenton,jiadatBoot$southern);
	Kxmat = cbind(intsBoot,jiadatBoot$population,jiadatBoot$SPC,jiadatBoot$urban,jiadatBoot$MidWest);
	
# Create Dependent Vectors
	WalMart = jiadatBoot$WalMart
	Kmart = jiadatBoot$Kmart
	
# Estimate and Store
	Boot.res[b,] = optim(theta,twostep.nll,control=list(trace=10,maxit=1000),method="BFGS")$par
	cat("Bootstrap #",b," completed. \n",sep="")
}

# Standard Errors
twostep.se = sqrt(diag(var(Boot.res)))
delta.twostep.se = sqrt(var(exp(Boot.res[,9])))


# =====================================
# Incomplete Information Games- Model 3
# Nested Pseudo Likelihood
# =====================================

# Construct X matrices as before

nmkts = nrow(jiadat); # Number of markets
ints = rep(1,nmkts);  # intercepts

Wxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$dBenton,jiadat$southern);
Kxmat = cbind(ints,jiadat$population,jiadat$SPC,jiadat$urban,jiadat$MidWest);

# Create Dependent Vectors
WalMart = jiadat$WalMart
Kmart = jiadat$Kmart

# Define NPL Negative log Likelihood

npl.nll <- function(theta,npl.pold.K,npl.pold.W) {
	
# Number of player specific parameters

	kW = ncol(Wxmat);
	kK = ncol(Kxmat);

# set parameters

	theta.W = c(theta[1],theta[3:5],theta[6:7]);
	theta.K = c(theta[2],theta[3:5],theta[8]);
	delta   = exp(theta[9]); # We exponentiate to ensure competitive effects are negative
	
# Deterministic component of profits
    
	pi.W = Wxmat%*%theta.W;
    pi.K = Kxmat%*%theta.K;
		
# Compute probabilities
	
	prob.W = pnorm(pi.W - delta*npl.pold.K);
	prob.K = pnorm(pi.K - delta*npl.pold.W);
	
# Construct Negative LogLikelihood
	
	ll = sum(WalMart*log(prob.W)+(1-WalMart)*log(1-prob.W) + Kmart*log(prob.K)+(1-Kmart)*log(1-prob.K));
	-ll
}

# For NPL we simply iterate
# starting CCPs
# starting from data (could start anywhere)
npl.pold.K = Kmart
npl.pold.W = WalMart

# Main NPL loop
doNPLloop = function(theta,npl.pold.K,npl.pold.W,err.tol = 1E-8, quiet = FALSE) {

# Monkey with err.tol this to see effects
	
	err =10; 
	npl.reps = 0;
	tot.reps = 2000;
	
while(err>err.tol & npl.reps<tot.reps) {
	
	npl.reps = npl.reps + 1;
	old.theta = theta;

# Minimize Pseudo negative log Likelihood
	
	kth.res = optim(theta,npl.nll,control=list(maxit=10000),method="BFGS",npl.pold.K=npl.pold.K,npl.pold.W=npl.pold.W)

# Update prameters
    
	theta = kth.res$par;
	npl.theta.W = c(theta[1],theta[3:5],theta[6:7]);
	npl.theta.K = c(theta[2],theta[3:5],theta[8]);
	npl.delta   = exp(theta[9]); # We exponentiate to ensure competitive effects are negative

# Compute Deterministic component of profits

    npl.pi.W = Wxmat%*%npl.theta.W;
    npl.pi.K = Kxmat%*%npl.theta.K;
		
# Now reconstruct probabilities	
	
	npl.pnew.W = pnorm(npl.pi.W - npl.delta*npl.pold.K);
	npl.pnew.K = pnorm(npl.pi.K - npl.delta*npl.pold.W); 

# Acceleration Trick of Kasahara and Shimotsu
# Do one more step (slower but gives quicker convergence)	
	npl.pnew.W = pnorm(npl.pi.W - npl.delta*npl.pnew.K);
	npl.pnew.K = pnorm(npl.pi.K - npl.delta*npl.pnew.W); 
	
# Compute error ||Pnew - Pold||
	
	err = t(npl.pnew.K-npl.pold.K)%*%(npl.pnew.K-npl.pold.K)/2+t(npl.pnew.W-npl.pold.W)%*%(npl.pnew.W-npl.pold.W)/2;

# Update probabilities
	
	npl.pold.W = npl.pnew.W;
	npl.pold.K = npl.pnew.K;
	
# Spit out info
	if(!quiet) {	
	cat("NPL Estimator: ",npl.reps," iterations completed \n")
	cat(" Current Error: ",err,"\n \n");
	}
	if(npl.reps==tot.reps & err>err.tol) { 
		cat("The NPL algorithm did not converge.\n","Try increasing the number of iterations \n or lowering the tolerance. \n"); 
	    npl.res = kth.res
	}
	
	if(err<err.tol) {
		npl.res = kth.res;
		cat("Algorithm converged. \n"); 
	}
}
# results in npl.res 
	npl.res
}


# Use same starting values as other algorithms
theta = c(-4.8960054, -15.3832747,   1.6653983,   0.9289151,   2.4720639,-1.4612322,   2.0311288 ,  2.1255131,   0.6889679);
NPL.res = doNPLloop(theta,npl.pold.K,npl.pold.W)
delta.NPL  =exp(NPL.res$par[9])
# There is a typo in the paper regarding the estiamte of delta printed as 1.6 something rather than 1.16 something


# Standard Errors
# The steps follow the 2-step bootstrap
# We leave this as an exercise to the reader! :)


