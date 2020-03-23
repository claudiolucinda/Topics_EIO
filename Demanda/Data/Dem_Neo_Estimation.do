********************************************************************
* Version 01 of the AIDS system
* Claudio R. Lucinda
* ****************************************************************** 

clear all
set memory 128m
set matsize 800
set more off, permanently
version 14

cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\"


adopath + "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\"

use "EASI\hixdata.dta", clear

local J=9
local Jm1=`J'-1
local i=1
foreach varn in sfoodh sfoodr srent soper sfurn scloth stranop srecr spers {
	qui mean `varn'
	matrix m`i'=e(b)
	scalar sc_`varn'=m`i'[1,1]
}

gen l_Big_P=0

local sharenames "sfoodh sfoodr srent soper sfurn scloth stranop srecr spers"
local pricenames "pfoodh pfoodr prent poper pfurn pcloth ptranop precr ppers"

forvalues i=1/`J' {
	local tshare: word `i' of `sharenames'
	local tprice: word `i' of `pricenames'
	qui replace l_Big_P=l_Big_P+sc_`tshare'*`tprice'

}

gen l_y_P=log_y-l_Big_P
/* ------------------------------------------------------------*/
/* ---- Comecando a Estimacao do Modelo AIDS ------------------*/
/* ------------------------------------------------------------*/
* 1) Criando as equações 

*set trace on
forvalues i=1/`Jm1' {
	local nom: word `i' of `sharenames'
	global t`nom' (`nom' `pricenames' l_y_P)
}

display "$tsfoodh"

reg3 $tsfoodh $tsfoodr $tsrent $tsoper $tsfurn $tscloth $tstranop $tsrecr, sure

reg3 $tsfoodh $tsfoodr $tsrent $tsoper $tsfurn $tscloth $tstranop $tsrecr, sure ireg3


* Restrições Homogeneidade
local aa=1
foreach var in sfoodh sfoodr srent soper sfurn scloth stranop srecr {
	constraint define `aa' [`var']_b[pfoodh]+[`var']_b[pfoodr]+[`var']_b[prent]+[`var']_b[poper]+[`var']_b[pfurn]+[`var']_b[pcloth]+[`var']_b[ptranop]+[`var']_b[precr]+[`var']_b[ppers]=0
	local ++aa

}

local Jm2=`Jm1'-1
forvalues i=1/`Jm2' {
	local k=`i'+1
	forvalues j=`k'/`Jm1' {
	local tempprice1: word `i' of `pricenames'
	local tempprice2: word `j' of `pricenames'
	local tempshare1: word `i' of `sharenames'
	local tempshare2: word `j' of `sharenames'
	constraint define `aa' [`tempshare1']_b[`tempprice2']=[`tempshare2']_b[`tempprice1']
	local ++aa
	}
}

reg3 $tsfoodh $tsfoodr $tsrent $tsoper $tsfurn $tscloth $tstranop $tsrecr, ireg3 constr(1-`aa') su nolog




/* ----------------------------------------------------------- */
/* -------Calculando as Elasticidades------------------------- */
/* ----------------------------------------------------------- */
* Usando o NLCOM para ter os coeficientes e os erros-padrão da última equação
* Fazendo em um loop para ficar mais abstrato
local term0 ""

*local b=1
forvalues i=1/`Jm1' {
	local tempshare: word `i' of `sharenames'
	local term1 " (al_`tempshare': [`tempshare']_b[_cons])"
	local term0 `term0' `term1'
	local term1 " (ga_`tempshare': [`tempshare']_b[l_y_P])"
	local term0 `term0' `term1'
	forvalues j=1/`J' {
	local tempprice: word `j' of `pricenames'
	local term1 " (be_`tempshare'_`tempprice': [`tempshare']_b[`tempprice'])"
	local term0 `term0' `term1'
	}


}
* constantes da última equação
local finalshare: word `J' of `sharenames'
local term2 ""
forvalues i=1/`Jm1' {
	local tempshare: word `i' of `sharenames'
	local term1 "-[`tempshare']_b[_cons]"
	local term2 `term2'`term1'
}
local term2 "(al_`finalshare': `term2'+1)"
di "`term2'"
*local term0 `term0' "(al_`finalshare': `term2')"

local term3 ""
forvalues i=1/`Jm1' {
	local tempshare: word `i' of `sharenames'
	local term1 "-[`tempshare']_b[l_y_P]"
	local term3 `term3'`term1'
}
local term3 "(ga_`finalshare': `term3')"
di "`term3'"

local term3 ""
local sharenames "sfoodh sfoodr srent soper sfurn scloth stranop srecr spers"
local finalshare: word 9 of `sharenames'
forvalues i=1/8 {
	local tempshare: word `i' of `sharenames'
	local term1 "-[`tempshare']_b[ppers]"
	local term3 `term3'`term1'
}
local term3 "(be_`finalshare'_ppers: `term3')"
di "`term3'"


*di "`term0'"
nlcom `term0' (al_spers: 1-[sfoodh]_b[_cons]-[sfoodr]_b[_cons]-[srent]_b[_cons]-[soper]_b[_cons]-[sfurn]_b[_cons]-[scloth]_b[_cons]-[stranop]_b[_cons]-[srecr]_b[_cons]) ///
(ga_spers: -[sfoodh]_b[l_y_P]-[sfoodr]_b[l_y_P]-[srent]_b[l_y_P]-[soper]_b[l_y_P]-[sfurn]_b[l_y_P]-[scloth]_b[l_y_P]-[stranop]_b[l_y_P]-[srecr]_b[l_y_P]) ///
(be_spers_pfoodh: -[sfoodh]_b[pfoodh]-[sfoodr]_b[pfoodh]-[srent]_b[pfoodh]-[soper]_b[pfoodh]-[sfurn]_b[pfoodh]-[scloth]_b[pfoodh]-[stranop]_b[pfoodh]-[srecr]_b[pfoodh]) ///
(be_spers_pfoodr: -[sfoodh]_b[pfoodr]-[sfoodr]_b[pfoodr]-[srent]_b[pfoodr]-[soper]_b[pfoodr]-[sfurn]_b[pfoodr]-[scloth]_b[pfoodr]-[stranop]_b[pfoodr]-[srecr]_b[pfoodr]) ///
(be_spers_prent: -[sfoodh]_b[prent]-[sfoodr]_b[prent]-[srent]_b[prent]-[soper]_b[prent]-[sfurn]_b[prent]-[scloth]_b[prent]-[stranop]_b[prent]-[srecr]_b[prent]) ///
(be_spers_poper: -[sfoodh]_b[poper]-[sfoodr]_b[poper]-[srent]_b[poper]-[soper]_b[poper]-[sfurn]_b[poper]-[scloth]_b[poper]-[stranop]_b[poper]-[srecr]_b[poper]) ///
(be_spers_pfurn: -[sfoodh]_b[pfurn]-[sfoodr]_b[pfurn]-[srent]_b[pfurn]-[soper]_b[pfurn]-[sfurn]_b[pfurn]-[scloth]_b[pfurn]-[stranop]_b[pfurn]-[srecr]_b[pfurn]) ///
(be_spers_pcloth: -[sfoodh]_b[pcloth]-[sfoodr]_b[pcloth]-[srent]_b[pcloth]-[soper]_b[pcloth]-[sfurn]_b[pcloth]-[scloth]_b[pcloth]-[stranop]_b[pcloth]-[srecr]_b[pcloth]) ///
(be_spers_ptranop: -[sfoodh]_b[ptranop]-[sfoodr]_b[ptranop]-[srent]_b[ptranop]-[soper]_b[ptranop]-[sfurn]_b[ptranop]-[scloth]_b[ptranop]-[stranop]_b[ptranop]-[srecr]_b[ptranop]) ///
(be_spers_precr: -[sfoodh]_b[precr]-[sfoodr]_b[precr]-[srent]_b[precr]-[soper]_b[precr]-[sfurn]_b[precr]-[scloth]_b[precr]-[stranop]_b[precr]-[srecr]_b[precr]) ///
(be_spers_ppers: -[sfoodh]_b[ppers]-[sfoodr]_b[ppers]-[srent]_b[ppers]-[soper]_b[ppers]-[sfurn]_b[ppers]-[scloth]_b[ppers]-[stranop]_b[ppers]-[srecr]_b[ppers]), post

global nprice: word count `pricenames'
global ncols=$nprice+1
matrix elastsLAAIDS=J($nprice,$ncols,.)

local medpoint=0
local i=1
foreach nom of local sharenames {
	if `medpoint'==0{
	qui predictnl er_`i'=_b[ga_`nom']/`nom'+1
	}
	else if `medpoint'==1 {
		qui egen med_`nom'=mean(`nom')
		qui predictnl er_`i'=_b[ga_`nom']/med_`nom'+1
		drop med_`nom'
	}
	qui su er_`i'
	mat elastsLAAIDS[`i',10]=r(mean)
	drop er_`i'
	local ++i
}

*set trace on
forvalues i=1/`J' {
	local tempshare1: word `i' of `sharenames'
*	local tempprice1: word `i' of `pricenames'
	forvalues j=1/`J' {
		if `i'==`j'{
			local delt=1
		}
		else {
			local delt=0
		} 
		local tempshare2: word `j' of `sharenames'
		local tempprice2: word `j' of `pricenames'
		if `medpoint'==0 {
		qui predictnl ep_`i'_`j'=((_b[be_`tempshare1'_`tempprice2']-_b[ga_`tempshare1']*`tempshare2')/`tempshare1')-`delt'
		}
		else if `medpoint'==1 {
			qui egen med_`tempshare1'=mean(`tempshare1')
			if `i' == `j' {
				qui predictnl ep_`i'_`j'=((_b[be_`tempshare1'_`tempprice2']-_b[ga_`tempshare1']*med_`tempshare1')/med_`tempshare1')-`delt'
				drop med_`tempshare1'
			}
			else {
				qui egen med_`tempshare2'=mean(`tempshare2')
				qui predictnl ep_`i'_`j'=((_b[be_`tempshare1'_`tempprice2']-_b[ga_`tempshare1']*med_`tempshare2')/med_`tempshare1')-`delt'
				drop med_`tempshare1' med_`tempshare2'
			}
			
		}
		qui su ep_`i'_`j'
		mat elastsLAAIDS[`i',`j']=r(mean)
		drop ep_`i'_`j'
	}
}

mat rownames elastsLAAIDS = `sharenames'
mat colnames elastsLAAIDS = `pricenames' income
estout matrix(elastsLAAIDS, fmt(%9.3f))
 

nlsur nlaidsCRL @ sfoodh sfoodr srent soper sfurn scloth stranop srecr ///
pfoodh pfoodr prent poper pfurn pcloth ptranop precr ppers log_y, ifgnls ///
param(a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 g1_1 g1_2 g1_3 g1_4 g1_5 g1_6 g1_7 g1_8 ///
g2_2 g2_3 g2_4 g2_5 g2_6 g2_7 g2_8 ///
g3_3 g3_4 g3_5 g3_6 g3_7 g3_8 ///
g4_4 g4_5 g4_6 g4_7 g4_8 ///
g5_5 g5_6 g5_7 g5_8 ///
g6_6 g6_7 g6_8 ///
g7_7 g7_8 ///
g8_8) nolog nequations(8) ///
hasconstants(a1 a2 a3 a4 a5 a6 a7 a8)

run Elast_NLaids.do
estout matrix(elastsAIDS, fmt(%9.3f))


nlsur quaidsCRL @ sfoodh sfoodr srent soper sfurn scloth stranop srecr ///
pfoodh pfoodr prent poper pfurn pcloth ptranop precr ppers log_y, ifgnls ///
param(a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 g1_1 g1_2 g1_3 g1_4 g1_5 g1_6 g1_7 g1_8 ///
g2_2 g2_3 g2_4 g2_5 g2_6 g2_7 g2_8 ///
g3_3 g3_4 g3_5 g3_6 g3_7 g3_8 ///
g4_4 g4_5 g4_6 g4_7 g4_8 ///
g5_5 g5_6 g5_7 g5_8 ///
g6_6 g6_7 g6_8 ///
g7_7 g7_8 ///
g8_8 ll1 ll2 ll3 ll4 ll5 ll6 ll7 ll8) nolog nequations(8) ///
hasconstants(a1 a2 a3 a4 a5 a6 a7 a8)

do Elast_QUAIDS.do
estout matrix(elasts, fmt(%9.3f))
