/*************************************************************************************************************************
MULTI-STAGE LINEAR APROXIMATION OF ALMOST IDEAL DEMAND SYSTEM - MUTI-STAGE LA-AIDS
Código GARI
Julho - 2014
FEA-RP/USP
Base: ANP / Datasus
*************************************************************************************************************************/


/* MODELO MULTI-STAGE LA-AIDS SIMPLES (S/ CORREÇÃO DE AUTOCORRELAÇÃO TEMPORAL [VECM])

ESTÁGIO INFERIOR:

S_int = Alpha_in + Beta_i * log(Expend_nt / P_nt) + Gamma_ii * Pr_int + Gamma_ij * Pr_jnt + Z_nt * Theta_in + Erro_int
		
ESTÁGIO SUPERIOR:

log(Qtot_nt) = Alpha_n + Beta * log(Renda_nt) + Gamma * P_nt + Z_n * Theta + Erro_nt

*/


set matsize 4000


**************************************************************************************************************************
* Criando as variáveis de gasto (Expenditure (Expend_nt)) total com cada produto *****************************************

* Criando o valor gasto com cada produto i:
gen valor_int_gas = prvendagas_defl * gas
gen valor_int_etl = prvendaetl_defl * etanol
* Somando os valores gastos com cada produto:
egen valor_nt = rowtotal(valor_int_*)

**************************************************************************************************************************
* Gerando Market Share de Valor (por tipo de produto, por mês e por região (S_int)) **************************************

gen S_int_gas = (valor_int_gas) / valor_nt
gen S_int_etl = (valor_int_etl) / valor_nt

* Correção caso haja valores impróprios de share:
drop if S_int_gas >1
rename estado estado_extenso
**************************************************************************************************************************
**************************************************************************************************************************
* Gerando o Preço de Stone  Linear ***************************************************************************************

* Gerando o share médio (w_in)
* Gerando o valor gasto com cada produto ao longo de todo o período disponível (Soma em t)
bysort municipio: egen valor_in_gas = sum(valor_int_gas)
bysort municipio: egen valor_in_etl = sum(valor_int_etl)
* Gerando o valor total gasto com todos os produtos ao longo de todo o período disponível (Soma em i e em t)
egen valor_n = rowtotal(valor_in_*)
* Gerando share de Gasto no Produto ao longo de todo o período disponível
bysort municipio: gen w_in_gas = valor_in_gas / valor_n 
bysort municipio: gen w_in_etl = valor_in_etl / valor_n
* Gerando a média do share por Região, considerando todo o período (Média em i)
egen w_i_gas = mean(w_in_gas)
egen w_i_etl = mean(w_in_etl)

* Gerando os componentes do Índice de Preços de Stone (Aproximação Linear):
gen Stone_int_gas = w_in_gas * log(prvendagas_defl)
replace Stone_int_gas = 0 if Stone_int_gas ==.
gen Stone_int_etl = w_in_etl * log(prvendaetl_defl)
replace Stone_int_etl = 0 if Stone_int_etl ==.
* Gerando o Índice Preço de Stone em nível e em log
gen logP_nt = Stone_int_gas + Stone_int_etl
gen P_nt = exp(logP_nt)

* Ajuste dos nomes das variáveis
gen Pr_int_gas = log(prvendagas_defl)
gen Pr_int_etl = log(prvendaetl_defl)
gen Renda_nt = log(renda_pcdefl)

**************************************************************************************************************************
* Gerando o Gasto "deflacionado" no mercado ******************************************************************************

gen logExpend_nt = log(valor_nt/ P_nt)

**************************************************************************************************************************
**************************************************************************************************************************


**************************************************************************************************************************
* Demais Ajustes do LA-AIDS ou Ajustes específicos desta base/trabalho ***************************************************

* Declarando os dados de painel / tempo
egen region=group(id)
tsset region t

egen estado = group(uf)

* Gerando a média do Share Valor para todo o período e todas as regiões (Média em n e t)
egen S_i_gas = mean(S_int_gas)
egen S_i_etl = mean(S_int_etl)

* Criando Variável de Tendência (trend)
bysort region: gen trend = t


local i = 2
* Criando Dummies para identificar o efeito da Lei em ES
foreach state in ES MS MG PR MT BA SE PE {
	
	* Gera dummy de Estado:
	gen `state' = 0
	replace `state' = 1 if uf =="`state'"
	
	* Gera dummy com interação de Estado com Preço
	gen `state'gas = Pr_int_gas * `state'
	gen `state'etl = Pr_int_etl * `state'
	
	constraint define `i' [S_int_gas]`state'gas   = - [S_int_gas]`state'etl
	local i = `i' + 1	
}
	* Gera dummy para Lei:
	gen Lei = 0
	replace Lei = 1 if uf =="ES" & t>=92 
	replace Lei = 1 if uf =="MS" & t>=118
	replace Lei = 1 if uf =="MG" & t>=119
	replace Lei = 1 if uf =="PR" & t>=132
	replace Lei = 1 if uf =="MT" & t>=140
	replace Lei = 1 if uf =="BA" & t>=142
	replace Lei = 1 if uf =="SE" & t>=143
	replace Lei = 1 if uf =="PE" & t>=155
	* Gera dummy com interação de Lei com Preço
	gen Leigas = Pr_int_gas * Lei
	gen Leietl = Pr_int_etl * Lei	
	* Restrições
	constraint define 10 [S_int_gas]Leigas  = - [S_int_gas]Leietl
	

/*
* Dummy para quando a Margem de Gasolina está maior que a de Etanol e razão de preços próxima de 0.7
gen margem_gas = prvendagas_defl - prcompragasfob_defl 
gen margem_etanol = prvendaetl_defl - prcompraetlfob_defl 
gen d_margem = 0
replace d_margem = 1  if margem_gas > margem_etanol & razao_p >= 0.68 & razao_p <= 0.72 & margem_gas !=.

gen d_margemESdepois = d_margem * d_ES_depois
*/
fillin id t
* Variável para check da dummy d_menor07
gen d_menor = 0
replace d_menor = prvendaetl_defl / prvendagas_defl 

gen d_menor07 = 0
replace d_menor07 = 1 if  prvendaetl_defl / prvendagas_defl < 0.7

gen cumulus = d_menor07
replace cumulus = 0 if cumulus == .
* interpolando dados de flex para períodos missing
bysort id: replace cumulus = cumulus + cumulus[_n-1] if cumulus[_n-1] !=.
bysort id: gen inst_flex = cumulus
bysort id: replace inst_flex = cumulus - cumulus[_n-12] if cumulus[_n-12] !=. | cumulus[_n-12] !=0
bysort id: replace inst_flex = inst_flex[_n-1] if d_menor07 ==.

drop if _fillin ==1
**************************************************************************************************************************
**************************************************************************************************************************

* Regressão LA-AIDS Multi-Estágio(versão linear do modelo AIDS) **********************************************************
**************************************************************************************************************************

* Especificando termos para o cálculo das Elasticidades
gen invS_i_gas =  1 / S_i_gas
gen W_gas_razao_S_gas = w_i_gas / S_i_gas
gen W_etl_razao_S_gas = w_i_etl / S_i_gas
gen invS_i_etl =  1 / S_i_etl
gen W_etl_razao_S_etl = w_i_etl / S_i_etl
gen W_gas_razao_S_etl = w_i_gas / S_i_etl

* Gerando série de Tendencia
gen trend_uf = trend*estado
gen trend_id = trend*region


* Definindo equações do sistema e afins
*global mod_etanol    "S_int_etl logExpend_nt Pr_int_gas Pr_int_etl Esgas Esetl Depoisgas Depoisetl Leigas Leietl frota_total frota_flex pop homens educ0 educ1a3 educ4a7 educmais8 postos d_mes_* trend_id i.region"
*global mod_gasMargem  "S_int_gas logExpend_nt Pr_int_gas Pr_int_etl Esgas Esetl Depoisgas Depoisetl Leigas Leietl d_margem d_margemESdepois frota_total frota_flex pop homens educ0 educ1a3 educ4a7 educmais8 postos d_mes_* trend_id i.region"


* Variável global para omitir efeitos fixos da tabela de resultados
/*global omitir _It_38 _Iregion_2
forvalues num = 39(1)156 {
	global omitir _It_`num' $omitir 
}

forvalues num = 3(1)640 {
	global omitir _Iregion_`num' $omitir 
}
*/

* Restrição de Homogeneidade
constraint define 1 [S_int_gas]Pr_int_gas = - [S_int_gas]Pr_int_etl



*******************************************************************************************************************************************
* Regressão LA-AIDS Multi-Estágio(versão linear do modelo AIDS) ***************************************************************************
*******************************************************************************************************************************************


if homogeneo ==1 {
	xi: reg3 (S_int_gas:$mod_gasolina) (Qtot_nt:$mod_estagio_sup),  endog($endogenas) exog($exogenas) constraints(1-10) 
}
if homogeneo ==0 {
	xi: reg3 (S_int_gas:$mod_gasolina) (Qtot_nt:$mod_estagio_sup),  endog($endogenas) exog($exogenas)
}
est store mod01, title("Modelo Estimado")
*
* Calculando as Elasticidades-Preço Próprias do modelo de Gasolina
nlcom (Elast_gas_gas_BR:    invS_i_gas * [S_int_gas]_b[Pr_int_gas] + w_i_gas + [Qtot_nt]_b[logP_nt] * w_i_gas + [Qtot_nt]_b[logP_nt] * [S_int_gas]_b[logExpend_nt] * W_gas_razao_S_gas - 1) ///
	  (Elast_gas_etl_BR:    invS_i_gas * [S_int_gas]_b[Pr_int_etl] + w_i_etl + [Qtot_nt]_b[logP_nt] * w_i_etl + [Qtot_nt]_b[logP_nt] * [S_int_gas]_b[logExpend_nt] * W_etl_razao_S_gas) ///
	  (Elast_etl_etl_BR:  - invS_i_etl * [S_int_gas]_b[Pr_int_etl] + w_i_etl + [Qtot_nt]_b[logP_nt] * w_i_etl - [Qtot_nt]_b[logP_nt] * [S_int_gas]_b[logExpend_nt] * W_etl_razao_S_etl - 1) ///
	  (Elast_etl_gas_BR:  - invS_i_etl * [S_int_gas]_b[Pr_int_gas] + w_i_gas + [Qtot_nt]_b[logP_nt] * w_i_gas - [Qtot_nt]_b[logP_nt] * [S_int_gas]_b[logExpend_nt] * W_gas_razao_S_etl), post

est store elast01, title("Elasticidades")

* Recuperando os valores das Elasticidades
matrix define Elast = r(b)
/*
svmat Elast, names(col)

	  
display "Elasticidade Própria do modelo S_int_gas de Brasil antes da Lei:"
testnl _b[Elast_gas_gas_BR] = w_i_gas - 1
gen Testnl_gas_gas_BR = r(p)
display "Elasticidade Cruzada do modelo S_int_gas de Brasil antes da Lei:"
testnl _b[Elast_gas_etl_BR] = w_i_etl 	
gen Testnl_gas_etl_BR = r(p)
display "Elasticidade Própria do modelo S_int_gas de Brasil antes da Lei:"
testnl _b[Elast_etl_etl_BR] = w_i_etl - 1
gen Testnl_etl_etl_BR = r(p)
display "Elasticidade Cruzada do modelo S_int_etl de Brasil antes da Lei:"
testnl _b[Elast_etl_gas_BR] = w_i_gas 
gen Testnl_etl_gas_BR = r(p)


*keep homogeneo instrumentos inferior Elast_* Testnl_*
order homogeneo instrumentos inferior Elast_* Testnl_*

duplicates drop
drop if Elast_gas_gas_BR ==.

*append using "C:\Dropbox\Lei ES\Programas\Municipal\Municipal3.dta" 
*saveold "C:\Dropbox\Lei ES\Programas\Municipal\Municipal3.dta", replace


*******************************************************************************************************************************************
*******************************************************************************************************************************************

*/
