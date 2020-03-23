* Tricks with Hicks: The EASI demand system
* Arthur Lewbel and Krishna Pendakur
* 2008, American Economic Review

* This code is written by Krishna Pendakur
* Suggested citation:  "Pendakur, Krishna. 2015.  "EASI GMM code for Stata".  available at www.sfu.ca/~pendakur
* keywords:  Stata, GMM, system, multiple-equation, demand, EASI
* Herein, find Stata code to estimate a demand system with J equations, J prices, 
*	ndem demographic characteristics and npowers powers of implicit utility
* Because Stata's GMM routine can only handle about 100 parameters, this will bomb if you have
*   J or ndem very large.  
*   In that case, use "EASI GMM moment evaluator.do", which is uglier, but gets the job done
*   For small demand systems, "EASI GMM.do" and "EASI GMM moment evaluator.do" yield identical results.
* use_D=0 sets the matrix D (zy interactions) to zero.
* use_B=0 sets the matrix B (py interactions) to zero.
* use_Az=0 sets the matrices A_l (pz interactions) to zero.

set more off
macro drop _all
program drop _all
version 11
cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\EASI\"
use ".\hixdata.dta", clear

* set number of equations and prices and demographic characteristics
local J "8"
local Jm1=`J'-1
local ndem 5
local npowers "5"
scalar use_D=0    
scalar use_B=0    
scalar use_Az=0
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

* make global macros for x_stone and y
local x_stone "( x"
forval j=1/`J' {
	local x_stone "`x_stone' -(p`j'*w`j')"
}
local x_stone "`x_stone' )"
di "`x_stone'

local y "( y_stone "
forval j=1/`Jm1' {
	local y "`y' + 0.5*(np`j')^2*{A`j'`j': one $Azlist}"
	local jp1=`j'+1
	forval k=`jp1'/`Jm1' {
		local y "`y' + np`j'*np`k'*{A`j'`k': one $Azlist}"
	}
}
local y "`y' )"

if use_B==1 {
	local denom "(1 "
	forval j=1/`Jm1' {
		local denom "`denom' - 0.5*(np`j')^2*{B`j'`j'}"
		local jp1=`j'+1
		forval k=`jp1'/`Jm1' {
			local denom "`denom' - np`j'*np`k'*{B`j'`k'}"
		}
	}
	local denom "`denom' )"
	local y "(`y' / `denom')"
}

* display y; note that Stata GMM only wants the variable lists in GMM parameter vectors on the first occurrence
local y_first "`y'"
di "`y_first'"
if use_Az==0	local y: subinstr local y " one " "", all
if use_Az==1	local y: subinstr local y " one $zlist" "", all
di "`y'"

local eqlist ""
* make equations
forval j=1/`Jm1' {
	if `j'==1	local eq`j' "(w`j' - {b`j'0} - {b`j'1}*`y_first'"
	if `j'>1	local eq`j' "(w`j' - {b`j'0} - {b`j'1}*`y'"
	forval r=2/`npowers' {
		local eq`j' "`eq`j'' - {b`j'`r'}*`y'^`r'"
	}
	local eq`j' "`eq`j'' - {C`j':$zlist}"
	if use_D==1		local eq`j' "`eq`j'' - {D`j':$zlist}*`y'"
	forval k=1/`Jm1' {
		if `k'<`j' {
			local eq`j' "`eq`j'' - {A`k'`j':}*np`k'"
			if use_B==1			local eq`j' "`eq`j'' - {B`k'`j':}*np`k'*`y'"
		}
		if `k'>=`j' {
			local eq`j' "`eq`j'' - {A`j'`k':}*np`k'"
			if use_B==1			local eq`j' "`eq`j'' - {B`j'`k':}*np`k'*`y'"
		}
	}	
	local eq`j' "`eq`j'' )"
	local eqlist "`eqlist' `eq`j''"
}

forval j=1/`Jm1' {
	di "equation `j':"
	di "`eq`j'')'"
	di ""
}


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

gmm `eqlist', inst(`instlist') winitial(unadjusted, independent) quickderivatives

