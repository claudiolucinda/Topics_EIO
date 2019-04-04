* Tricks with Hicks: The EASI demand system
* Arthur Lewbel and Krishna Pendakur
* 2008, American Economic Review

* This code is written by Krishna Pendakur
* Suggested citation:  "Pendakur, Krishna. 2015.  "EASI GMM moment evaluator code for Stata".  available at www.sfu.ca/~pendakur
* keywords:  Stata, GMM, system, multiple-equation, demand, EASI, moment evaluator
* Herein, find Stata code to estimate a demand system with J equations, J prices, 
*	ndem demographic characteristics and npowers powers of implicit utility
* Because Stata's GMM routine can only handle about 100 parameters, this program
*   uses Stata's 'moment evaluator package', which is uglier, but gets the job done.
*   If you are doing a small EASI demand system, I recommend "EASI GMM.do", which is less ugly.
*   For small demand systems, "EASI GMM.do" and "EASI GMM moment evaluator.do" yield identical results.
* Note that if this code is run on hixdata.dta (available on the AER data archive), you get results that are
*   similar to those in Lewbel and Pendakur (2008), but NOT identical.  This is because that paper uses more
*   complicated (and consequently less transparent) instruments.  Here, the instrument list is kept simple.
* use_D=0 sets the matrix D (zy interactions) to zero.
* use_B=0 sets the matrix B (py interactions) to zero.
* use_Az=0 sets the matrices A_l (pz interactions) to zero.
set more off
macro drop _all
program drop _all
cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\EASI\"
use "hixdata.dta", clear

* set number of equations and prices and demographic characteristics
local J "9"
local Jm1=`J'-1
local ndem 5
local npowers "5"
scalar use_D=1    
scalar use_B=1    /* not quite the same coefs */
scalar use_Az=1
g one=1

*data labeling conventions:
* budget shares: s1 to sneq
* prices: p1 to nprice
* implicit utility: y, or related names
* demographic characteristics: z1 to zTdem
g w1=sfoodh
g w2=sfoodr
g w3=srent
g w4=soper
g w5=sfurn
g w6=scloth
g w7=stranop
g w8=srecr
g w9=spers

g p1=pfoodh
g p2=pfoodr
g p3=prent
g p4=poper
g p5=pfurn
g p6=pcloth
g p7=ptranop
g p8=precr
g p9=ppers

* normalised prices are what enter the demand system
* generate normalised prices, backup prices (they get deleted), and Ap
forvalues j=1(1)`Jm1' {
	g np`j'=p`j'-p`J'	
}

*list demographic characteristics: fill them in, and add them to zlist below
g z1=age
g z2=hsex
g z3=carown
g z4=tran
g z5=time
global zlist "z1 z2 z3 z4 z5"
if use_Az==0	global Azlist ""
if use_Az==1	global Azlist "$zlist"

*make y_stone=x-p'w, and gross instrument, y_tilda=x-p'w^bar
g x=log_y
forval r=1/`npowers' {
	g x`r'=x^`r'
}
g y_stone=x
g y_tilda=x
forvalues j=1/`J' {
	egen mean_w`j'=mean(w`j')
	replace y_tilda=y_tilda-mean_w`j'*p`j'
	replace y_stone=y_stone-w`j'*p`j'
}

global xzlist ""
foreach var in $zlist {
	di "`var'"
	g x`var'=x*`var'
	global xzlist "$xzlist x`var'"
}
macro list xzlist

global npzlist ""
foreach var in $zlist {
	forval j=1/`Jm1' {
		g np`j'`var'=np`j'*`var'
		global npzlist "$npzlist np`j'`var'"
	}
}
macro list npzlist

global npx ""
forval j=1/`Jm1' {
	g np`j'x=np`j'*x
	global npx "$npx np`j'x"
}
macro list npx


* create instruments and instrument list
local instlist ""
forval r=1/`npowers' {
	local instlist "`instlist' x`r'"
}
local instlist "`instlist' $zlist"
if use_D==1		local instlist "`instlist' $xzlist"
forval j=1/`Jm1' {
	local instlist "`instlist' np`j'"
}
if use_Az==1	local instlist "`instlist' $npzlist"
if use_B==1		local instlist "`instlist' $npx"
di "`instlist'"

*gmm `eqlist', inst(`instlist') winitial(unadjusted, independent) quickderivatives



// Program Evaluator for GMM Hixtrix
* make parameter list
global parlist ""
forval j=1/`Jm1' {
	forval r=0/`npowers' {
		global parlist "$parlist b`j'`r'"
	}
}
forval j=1/`Jm1' {
	forval t=1/`ndem' {
		global parlist "$parlist C`j'`t'"
	}
}
forval j=1/`Jm1' {
	forval t=1/`ndem' {
		if use_D==1		global parlist "$parlist D`j'`t'"
	}
}
forval j=1/`Jm1' {
	forval k=`j'/`Jm1' {
		global parlist "$parlist A`j'`k'_0"
		if use_Az==1	{
			forval t=1/`ndem' {
				global parlist "$parlist A`j'`k'_`t'"
			}
		}
	}
}
forval j=1/`Jm1' {
	forval k=`j'/`Jm1' {
		if use_B==1		global parlist "$parlist B`j'`k'"
	}
}
macro list parlist
local npars = wordcount("$parlist")
noisily display "number of parameters: `npars'"



global Jm1=`Jm1'
global npowers=`npowers'
global ndem=`ndem'

program gmm_hixtrix
version 14.0
syntax varlist if, at(name)
quietly {
	tempname $parlist
	// allocate the parameter vector to names, and make Cj, Dj and Ajk (which sum over z's)
		local i=1
		forval j=1/$Jm1 {
			forval r=0/$npowers {
				scalar `b`j'`r''=`at'[1,`i']
				local i=`i'+1
			}
		}
		forval j=1/$Jm1 {
			tempvar C`j'
			g double `C`j''=0
			forval t=1/$ndem {
				scalar `C`j'`t''=`at'[1,`i']
				replace `C`j''=`C`j'' + `C`j'`t''*z`t'
				local i=`i'+1
			}
		}
		if use_D==1 {
			forval j=1/$Jm1 {
				tempvar D`j'
				g double `D`j''=0
				forval t=1/$ndem {
					scalar `D`j'`t''=`at'[1,`i']
					replace `D`j''=`D`j'' + `D`j'`t''*z`t'
					local i=`i'+1
				}
			}
			}
		forval j=1/$Jm1 {
			forval k=`j'/$Jm1 {
				tempvar A`j'`k'
				scalar `A`j'`k'_0'=`at'[1,`i']
				g double `A`j'`k''=`A`j'`k'_0' `if'
				local i=`i'+1
				if use_Az==1	{
					forval t=1/$ndem {
						scalar `A`j'`k'_`t''=`at'[1,`i']
						replace `A`j'`k''=`A`j'`k'' + `A`j'`k'_`t''*z`t' `if'
						local i=`i'+1
					}
				}
			}
		}
		if use_B==1 {
			forval j=1/$Jm1 {
				forval k=`j'/$Jm1 {
					scalar `B`j'`k''=`at'[1,`i']
					local i=`i'+1
				}
			}
		}

	// make y
		tempvar y
		g double `y' =y_stone `if'
		forval j=1/$Jm1 {
			replace `y'=`y' + 0.5*(np`j')^2*`A`j'`j'' `if'
			local jp1=`j'+1
			forval k=`jp1'/$Jm1 {
				replace `y'=`y' + np`j'*np`k'*`A`j'`k'' `if'
			}
		}

	// make 1-0.5pBp
		if use_B==1 {
			tempvar denom
			g double `denom'=1 `if'
			forval j=1/$Jm1 {
				replace `denom'=`denom' - 0.5*(np`j')^2*`B`j'`j''  `if'
				local jp1=`j'+1
				forval k=`jp1'/$Jm1 {
					replace `denom'= `denom' - np`j'*np`k'*`B`j'`k''  `if'
				}
			}
			replace `y'=`y'/`denom'
		}
		
	// make Aj and Bjy (row sums Ap and Bpy)
	forval j=1/$Jm1 {
		tempvar A`j' B`j'y
		g double `A`j''=0 `if'
		g double `B`j'y'=0 `if'
		forval k=1/$Jm1 {
			if `k'<`j' {
				replace `A`j''=`A`j'' + `A`k'`j''*np`k' `if'
				if use_B==1			replace `B`j'y'=`B`j'y' + `B`k'`j''*np`k' *`y' `if'
			}
			if `k'>=`j' {
				replace `A`j''=`A`j'' + `A`j'`k''*np`k' `if'
				if use_B==1			replace `B`j'y'=`B`j'y' + `B`j'`k''*np`k' *`y' `if'
			}
		}	
	}
	
	// make residuals that are orthogonal to instruments
	forval j=1/$Jm1 {
		tempvar what`j'
		generate double `what`j'' = `b`j'0' + `C`j'' `if'
		forval r=1/$npowers {
				replace `what`j'' = `what`j'' + `b`j'`r''*`y'^`r' `if'
		}
		if use_D==1		replace  `what`j'' = `what`j'' + `D`j''*`y' `if'
		replace `what`j'' = `what`j'' + `A`j'' `if'
		if use_B==1		replace  `what`j'' = `what`j'' + `B`j'y' `if'
		local e`j': word `j' of `varlist'
		replace `e`j'' = w`j' - `what`j'' `if'
	}
	
}
end

local npars = wordcount("$parlist")
noisily display "number of parameters: `npars'"
local npars = wordcount("`instlist'")
noisily display "number of instruments: `npars'"
noisily display "number of equations: `Jm1'"
di "$parlist"
di "`instlist'"
display "$S_TIME  $S_DATE"

gmm gmm_hixtrix, nequations($Jm1) parameters($parlist) ///
instruments(`instlist')  winitial(unadjusted, independent) vce(robust) quickderivatives
*winitial(unadjusted, independent)

display "$S_TIME  $S_DATE"	
