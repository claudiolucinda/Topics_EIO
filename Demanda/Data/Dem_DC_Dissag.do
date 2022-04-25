************************************************
* Código Dados Logit (ASCLOGIT) Desagregados
* Cláudio R. Lucinda
* FEA-RP/USP
************************************************


clear all
set memory 128m
set matsize 800
set more off

cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\"
adopath + "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\"
*cap log close
*log using  "Output_Micro_DC.log", replace

insheet using mgdata5.txt

/*
1. Panelist
2. Category purchase? 0 - No; 1 - Yes
3. Week
4-7. Brand bought if any
8-11. Price
12-15. Feature
16-19. Display
*/

rename v1 panelist
rename v2 compra
rename v3 semana
forvalues i=4/7 {
	local j=`i'-3
	rename v`i' marca_`j'
}

forvalues i=8/11 {
	local j=`i'-7
	rename v`i' price_`j'
}

forvalues i=12/15 {
	local j=`i'-11
	rename v`i' feature_`j'
}

forvalues i=16/19 {
	local j=`i'-15
	rename v`i' display_`j'
}
duplicates drop panelist semana, force


****************************************************************************************
* Mlogit
* Neste caso, assume-se que as variáveis independentes são iguais entre as alternativas
* O que vai variar entre as alternativas - para dar a identificação - são os coeficientes
****************************************************************************************

gen depvar=0
forvalues i=1/4 {
	replace depvar=depvar+`i'*marca_`i'
}
tab depvar

gen price_esc=0
forvalues i=1/4 {
	replace price_esc=price_esc+marca_`i'*price_`i'
}

gen feature_esc=0
forvalues i=1/4 {
	replace feature_esc=feature_esc+marca_`i'*feature_`i'
}

gen display_esc=0
forvalues i=1/4 {
	replace display_esc=display_esc+marca_`i'*display_`i'
}

* Usando o (1) como categoria base
* Usando o (0) engasga o otimizador numérico

*mlogit depvar price_esc feature_esc display_esc, base(0) gradient

mlogit depvar price_esc feature_esc display_esc if depvar!=0, base(1)

* OBS: A REMONTAGEM DOS DADOS A SEGUIR É NECESSÁRIA PARA ESTES MODELOS, O NLOGIT, BIPROBIT, MIXLOGIT, ASCPROBIT....
*************************************************************************************
* ASCLogit
* Aqui a primeira coisa que vamos fazer é reshape as paradas
*************************************************************************************
drop depvar *_esc compra
gen nro=_n
drop panelist semana
reshape long marca_ price_ display_ feature_, i(nro) j(brand)

* Com a constante - Não convergência
*asclogit marca_ price_ display_ feature_, case(nro) alternatives(brand) difficult base(1)

*asclogit marca_ price_ display_ feature_, case(nro) alternatives(brand) 


* Sem a constante
asclogit marca_ price_ display_ feature_, case(nro) alternatives(brand) nocons
El_asclogit, price(price_) choicevar(brand)
mat li r(elast)

asclogit marca_ price_ display_ feature_, case(nro) alternatives(brand) 


***********************************************************************************
* CLOGIT
***********************************************************************************
* Sem a constante
clogit marca_ price_ display_ feature_, group(nro)

* Com a constante - e para qualquer variável que seja constante entre os "Cases"
xi: clogit marca_ price_ display_ feature_ i.brand, group(nro)

**********************************************************************************
* Mixlogit
**********************************************************************************

xi: mixlogit marca_  display_ feature_ i.brand, group(nro) rand(price_)
El_mixlogit, price(price_) choicevar(brand)
mat li r(elast)

preserve
mixlbeta price_, saving(Coefs.dta) replace
use "Coefs.dta", clear
kdensity price_
restore

gen price2_=-price_

xi: mixlogit marca_  display_ feature_ i.brand, group(nro) rand(price2) ln(1) difficult
El_mixlogit, price(price2) choicevar(brand)
mat li r(elast)

preserve
mixlbeta price2, saving(Coefs.dta) replace
kdensity price2
restore

**********************************************************************************
* Nested Logit
**********************************************************************************

nlogitgen type=brand(tipo1:1|2,tipo2:3|4)
nlogittree brand type
/*
* Dois níveis. No primeiro display e no segundo feature and price

nlogit marca_ price_ feature_ || type: display_, base(tipo2) || brand:, noconst case(nro)

nlogit marca_ price_ feature_ display_ || type:, base(tipo2) || brand:, noconst case(nro)
*/

* Esse é o correspondente ao do clogit de cima
nlogit marca_ price_ feature_ display_ i.brand || type:, base(tipo2) || brand:, noconst case(nro)

* constrained
constraint 1 [tipo1_tau]_cons=[tipo2_tau]_cons

* Esse é o correspondente ao do clogit de cima
nlogit marca_ price_ feature_ display_ i.brand || type:, base(tipo2) || brand:, noconst case(nro) constraints(1)
