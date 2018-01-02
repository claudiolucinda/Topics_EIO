********************************************************************
* Version 01 of the AIDS system
* Claudio R. Lucinda
* ****************************************************************** 

clear all
set memory 128m
set matsize 800
set more off, permanently
version 11
cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\2014-2\Demanda\"

adopath + "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\2014-2\Demanda\"

use "Neo_data.dta", clear

/* Guardando as Medias para o calculo das Elasticidades */
qui mean tsval_soja
matrix m1=e(b)
qui mean tsval_milho
matrix m2=e(b)
qui mean tsval_girassol
matrix m3=e(b)
qui mean tsval_composto
matrix m4=e(b)
qui mean tsval_oliva
matrix m6=e(b)
qui mean l_y_P
matrix m5=e(b)
qui mean tsval_canola
matrix m7=e(b)

scalar sc_soja=m1[1,1]
scalar sc_milho=m2[1,1]
scalar sc_girassol=m3[1,1]
scalar sc_composto=m4[1,1]
scalar sc_oliva=m6[1,1]
scalar sc_canola=m7[1,1]
scalar sc5=m5[1,1]


/* ------------------------------------------------------------*/
/* ---- Comecando a Estimacao do Modelo AIDS ------------------*/
/* ------------------------------------------------------------*/


/* Determinando as Equacoes - Oleo de Canola de fora !!!!*/

global tsval_soja (tsval_soja l_p_soja l_p_milho l_p_girassol l_p_canola l_p_composto l_p_oliva l_y_P)
global tsval_milho (tsval_milho l_p_soja l_p_milho l_p_girassol l_p_canola l_p_composto l_p_oliva l_y_P)
global tsval_girassol (tsval_girassol l_p_soja l_p_milho l_p_girassol l_p_canola l_p_composto l_p_oliva l_y_P)
global tsval_composto (tsval_composto l_p_soja l_p_milho l_p_girassol l_p_canola l_p_composto l_p_oliva l_y_P)
global tsval_oliva (tsval_oliva l_p_soja l_p_milho l_p_girassol l_p_canola l_p_composto l_p_oliva l_y_P)
global l_Big_Q (l_Big_Q l_Big_P l_y)
reg3 $tsval_soja $tsval_milho $tsval_girassol $tsval_composto $tsval_oliva, sure
/* Estabelecendo as Restricoes de Homogeneidade */

scalar aa=1
foreach x in soja milho girassol composto oliva {
	local bbb=aa
	constraint define `bbb' [tsval_`x']l_p_soja+[tsval_`x']l_p_girassol+[tsval_`x']l_p_canola+[tsval_`x']l_p_composto+[tsval_`x']l_p_oliva=0
	scalar aa=aa+1
}


/* Estabelecendo as Restricoes de Simetria */

constraint define 6 [tsval_soja]l_p_milho=[tsval_milho]l_p_soja
constraint define 7 [tsval_soja]l_p_girassol=[tsval_girassol]l_p_soja
constraint define 8 [tsval_soja]l_p_composto=[tsval_composto]l_p_soja
constraint define 9 [tsval_soja]l_p_oliva=[tsval_oliva]l_p_soja
constraint define 10 [tsval_milho]l_p_girassol=[tsval_girassol]l_p_milho
constraint define 11 [tsval_milho]l_p_composto=[tsval_composto]l_p_milho
constraint define 12 [tsval_milho]l_p_oliva=[tsval_oliva]l_p_milho
constraint define 13 [tsval_girassol]l_p_composto=[tsval_composto]l_p_girassol
constraint define 14 [tsval_girassol]l_p_oliva=[tsval_oliva]l_p_girassol
constraint define 15 [tsval_composto]l_p_oliva=[tsval_oliva]l_p_composto


reg3 $tsval_soja $tsval_milho $tsval_girassol $tsval_composto $tsval_oliva, ireg3 constr(1-15) su nolog
reg3 $tsval_soja $tsval_milho $tsval_girassol $tsval_composto $tsval_oliva $l_Big_Q, ireg3 constr(1-15) su nolog



/* ----------------------------------------------------------- */
/* -------Calculando as Elasticidades------------------------- */
/* ----------------------------------------------------------- */




/* Loop dos Calculos */
global nomes "soja milho girassol composto oliva"
global nomes2 "soja milho girassol composto oliva"
scalar bb=1

mat elast1=J(6,6,0)
foreach x of global nomes {
	scalar aa=1
	foreach y of global nomes2 {
		matrix elast1[bb,aa]=(1/sc_`x')*([tsval_`x']_b[l_p_`y']-[tsval_`x']_b[l_y_P]*sc_`y')+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_`y'
		scalar aa=aa+1
	}
	scalar bb=bb+1
}

matrix elast2=elast1-I(6)


mat pval1=J(6,6,0)
scalar bb=1
foreach x of global nomes {
	scalar aa=1
	foreach y of global nomes2 {
		if aa==bb {
			qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_`y']-[tsval_`x']_b[l_y_P]*sc_`y')+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_`y'=-sc_`y'+1
			matrix pval1[bb,aa]=r(p)
		}
		else {
			qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_`y']-[tsval_`x']_b[l_y_P]*sc_`y')+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_`y'=-sc_`y'
			matrix pval1[bb,aa]=r(p)
		}
		scalar aa=aa+1
	}
	scalar bb=bb+1
}


mat pval2=J(6,6,0)
scalar bb=1
foreach x of global nomes {
	scalar aa=1
	foreach y of global nomes2 {
		if aa==bb {
			qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_`y']-[tsval_`x']_b[l_y_P]*sc_`y')+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_`y'=sc_`y'+1
			matrix pval2[bb,aa]=r(p)
		}
		else {
			qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_`y']-[tsval_`x']_b[l_y_P]*sc_`y')+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_`y'=sc_`y'
			matrix pval2[bb,aa]=r(p)
		}
		scalar aa=aa+1
	}
	scalar bb=bb+1
}


/* Calculando as Elasticidades - sexta coluna */
scalar bb=1
foreach x of global nomes {
	matrix elast2[bb,6]=(1/sc_`x')*([tsval_`x']_b[l_p_canola]-[tsval_`x']_b[l_y_P]*sc_canola)+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola
	qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_canola]-[tsval_`x']_b[l_y_P]*sc_canola)+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola=-sc_canola
	matrix pval1[bb,6]=r(p)
	qui testnl (1/sc_`x')*([tsval_`x']_b[l_p_canola]-[tsval_`x']_b[l_y_P]*sc_canola)+(1+([tsval_`x']_b[l_y_P]/sc_`x'))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola=sc_canola
	matrix pval2[bb,6]=r(p)
	scalar bb=bb+1
}

/* Calculando as Elasticidades - sexta linha */

scalar beta_6=(-[tsval_soja]_b[l_y_P]-[tsval_milho]_b[l_y_P]-[tsval_girassol]_b[l_y_P]-[tsval_composto]_b[l_y_P]-[tsval_oliva]_b[l_y_P])
*foreach x of global nomes {
*	scalar gamma_`x'=(1-[tsval_soja]_b[l_p_`x']-[tsval_milho]_b[l_p_`x']-[tsval_girassol]_b[l_p_`x']-[tsval_composto]_b[l_p_`x']-[tsval_oliva]_b[l_p_`x']
*}

scalar bb=1
foreach x of global nomes {
	matrix elast2[6,bb]=(1/sc_canola)*((-[tsval_soja]_b[l_p_`x']-[tsval_milho]_b[l_p_`x']-[tsval_girassol]_b[l_p_`x']-[tsval_composto]_b[l_p_`x']-[tsval_oliva]_b[l_p_`x'])-beta_6*sc_`x')+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_`x'
	qui testnl (1/sc_canola)*((-[tsval_soja]_b[l_p_`x']-[tsval_milho]_b[l_p_`x']-[tsval_girassol]_b[l_p_`x']-[tsval_composto]_b[l_p_`x']-[tsval_oliva]_b[l_p_`x'])-beta_6*sc_`x')+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_`x'=-sc_`x'
	matrix pval1[6,bb]=r(p)
	qui testnl (1/sc_canola)*((-[tsval_soja]_b[l_p_`x']-[tsval_milho]_b[l_p_`x']-[tsval_girassol]_b[l_p_`x']-[tsval_composto]_b[l_p_`x']-[tsval_oliva]_b[l_p_`x'])-beta_6*sc_`x')+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_`x'=sc_`x'
	matrix pval2[6,bb]=r(p)
	scalar bb=bb+1
}

matrix elast2[6,6]=(1/sc_canola)*((-[tsval_soja]_b[l_p_canola]-[tsval_milho]_b[l_p_canola]-[tsval_girassol]_b[l_p_canola]-[tsval_composto]_b[l_p_canola]-[tsval_oliva]_b[l_p_canola])-beta_6*sc_canola)+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola-1
qui testnl (1/sc_canola)*((-[tsval_soja]_b[l_p_canola]-[tsval_milho]_b[l_p_canola]-[tsval_girassol]_b[l_p_canola]-[tsval_composto]_b[l_p_canola]-[tsval_oliva]_b[l_p_canola])-beta_6*sc_canola)+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola=-sc_canola+1
matrix pval1[6,6]=r(p)
qui testnl (1/sc_canola)*((-[tsval_soja]_b[l_p_canola]-[tsval_milho]_b[l_p_canola]-[tsval_girassol]_b[l_p_canola]-[tsval_composto]_b[l_p_canola]-[tsval_oliva]_b[l_p_canola])-beta_6*sc_canola)+(1+(beta_6/sc_canola))*(1+[l_Big_Q]_b[l_Big_P])*sc_canola=sc_canola+1
matrix pval2[6,6]=r(p)

matrix colnames elast2 = P_Soja P_Milho P_Girassol P_Composto P_Oliva P_Canola
matrix colnames pval1 = P_Soja P_Milho P_Girassol P_Composto P_Oliva P_Canola
matrix rownames elast2 = Q_Soja Q_Milho Q_Girassol Q_Composto Q_Oliva P_Canola
matrix rownames pval1 = Q_Soja Q_Milho Q_Girassol Q_Composto Q_Oliva Q_Canola
matrix colnames pval2 = P_Soja P_Milho P_Girassol P_Composto P_Oliva Q_Canola 
matrix rownames pval2 = Q_Soja Q_Milho Q_Girassol Q_Composto Q_Oliva Q_Canola

matrix list elast2, format(%6.4g) title(Elasticidades)
matrix list pval1, format(%6.4g) title(P-Valores - Shares Negativos)
matrix list pval2, format(%6.4g) title(P-Valores - Shares Positivos)

mat2txt, matrix(elast2) ///
saving("elast2.txt") ///
title("Elasticidades") format(%6.3f %6.3f %6.3f %6.3f %6.3f) replace

mat2txt, matrix(pval1) ///
saving("pval1.txt") ///
title("P-Valores - Shares Positivos") format(%6.3f %6.3f %6.3f %6.3f %6.3f) replace

mat2txt, matrix(pval2) ///
saving("pval2.txt") ///
title("P-Valores - Shares Negativos") format(%6.3f %6.3f %6.3f %6.3f %6.3f) replace



qui reg3 $tsval_soja $tsval_milho $tsval_girassol $tsval_composto $tsval_oliva $l_Big_Q, ireg3 constr(1-15) su nolog
estimates store f1, title ("Estimacao SURE")

mark samp_setter if e(sample)

/* Guardando os Resultados */
estout * , ///
cells(b(star fmt(%9.3f)) t(par)) unstack ///
stats(N r2 p, fmt(%9.0f %9.3f %9.3f) ///
labels("N-Obs" "R-sq" "p-val")) ///
legend label collabels(, none) ///
varlabels(_cons Constante) posthead("") ///
prefoot("") postfoot("") ///
varwidth(16) modelwidth(12) 

estout matrix(elast2, fmt(%9.3f))


******************************************
* Now adapting stuff
******************************************

matrix startvals=J(1,25,.1)

su tsval*

cap program drop nlsurlaaidsCRL2
/*
set trace on

gen ttsval_soja=tsval_soja
gen ttsval_milho=tsval_milho
gen ttsval_girassol=tsval_girassol
gen ttsval_composto=tsval_composto
gen ttsval_oliva=tsval_oliva


nlsurlaaidsCRL2 ttsval_soja ttsval_milho ttsval_girassol ttsval_composto ttsval_oliva ///
l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola l_y_P if samp_setter==1, at(startvals)


set trace off
*/
nlsur laaidsCRL2 @ tsval_soja tsval_milho tsval_girassol tsval_composto tsval_oliva ///
l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola l_y_P if samp_setter==1, ifgnls ///
param(a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 g1_1 g1_2 g1_3 g1_4 g1_5 ///
g2_2 g2_3 g2_4 g2_5 g3_3 g3_4 g3_5 g4_4 g4_5 ///
g5_5) nolog nequations(5) ///
hasconstants(a1 a2 a3 a4 a5)

do Elast_LAaids.do

cap program drop nlsurnlaidsCRL2
/*
matrix startvals=J(1,25,.1)

su tsval*


set trace on

gen ttsval_soja=tsval_soja
gen ttsval_milho=tsval_milho
gen ttsval_girassol=tsval_girassol
gen ttsval_composto=tsval_composto
gen ttsval_oliva=tsval_oliva


nlsurnlaidsCRL2 ttsval_soja ttsval_milho ttsval_girassol ttsval_composto ttsval_oliva ///
l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola l_y_P if samp_setter==1, at(startvals)


set trace off
*/
nlsur nlaidsCRL2 @ tsval_soja tsval_milho tsval_girassol tsval_composto tsval_oliva ///
l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola l_y if samp_setter==1, ifgnls ///
param(a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 g1_1 g1_2 g1_3 g1_4 g1_5 ///
g2_2 g2_3 g2_4 g2_5 g3_3 g3_4 g3_5 g4_4 g4_5 ///
g5_5) nolog nequations(5) ///
hasconstants(a1 a2 a3 a4 a5)

do Elast_NLaids.do


nlsur quaidsCRL2 @ tsval_soja tsval_milho tsval_girassol tsval_composto tsval_oliva ///
l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola l_y if samp_setter==1, ifgnls ///
param(a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 g1_1 g1_2 g1_3 g1_4 g1_5 ///
g2_2 g2_3 g2_4 g2_5 g3_3 g3_4 g3_5 g4_4 g4_5 ///
g5_5 ll1 ll2 ll3 ll4 ll5) nolog nequations(5) ///
hasconstants(a1 a2 a3 a4 a5)

do Elast_QUAIDS.do
