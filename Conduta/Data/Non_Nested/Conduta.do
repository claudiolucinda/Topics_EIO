*****************************************************************
* Análise Empírica
* Claudio R. Lucinda
* FEA-RP/USP
******************************************************************

clear
set more off, permanently

cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Conduta\Data\Non_Nested\"

use "Dados.dta", clear

*****************************************
* Deflacionando as coisas
*****************************************

foreach var in pgas_sindi pgas_bb petan_sindi petan_bb m_pgas m_petan pgas_atac petan_atac Val_gas_atac Val_etan_atac {
	qui gen `var'_defl=`var'*(100/Deflator)
}

sort Z date
****************************************
* Vendo os gráficos
****************************************
/*
line pgas_sindi pgas_bb m_pgas date if Z==1
line pgas_sindi_defl pgas_bb_defl m_pgas_defl date if Z==1
line pgas_sindi_defl pgas_bb_defl m_pgas_defl pgas_atac_defl date if Z==1
*/
xtset Z date


****************************************
* Conduta imposta - Gasolina
****************************************

gen input_CMg_CP=m_pgas

*gen input_CMg_Cartel=m_pgas*(1+(1/$elast_gg)*1)+m_petan*(Qtde_etan/Qtde_gas)*((1/$elast_ag)*1)
gen input_CMg_Cartel=m_pgas*(1+(1/$elast_gg)*1+(1/$elast_ga)*(.1))+m_petan*(Qtde_etan/Qtde_gas)*((1/$elast_ag)*1+(1/$elast_aa)*(.1))

*gen input_CMg_Cournot=m_pgas*(1+(1/$elast_gg)*($sh_g))+m_petan*(Qtde_etan/Qtde_gas)*((1/$elast_ag)*($sh_e))
*gen input_CMg_Cournot_etan=m_petan*(1+(1/$elast_aa)*($sh_e)+(1/$elast_ag)*($sh_g))+m_pgas*(Qtde_gas/Qtde_etan)*((1/$elast_ga)*($sh_e)+(1/$elast_gg)*($sh_g))
gen input_CMg_Cournot=m_pgas*(1+(1/$elast_gg)*($sh_g)+(1/$elast_ga)*($sh_e))+m_petan*(Qtde_etan/Qtde_gas)*((1/$elast_ag)*($sh_e)+(1/$elast_aa)*($sh_e))


gen lerner_Cartel=(m_pgas-input_CMg_Cartel)/m_pgas
gen lerner_Cournot=(m_pgas-input_CMg_Cournot)/m_pgas
gen lerner_obs=(m_pgas-pgas_atac)/m_pgas

twoway line input_CMg_Cartel pgas_atac date if Z==1, ytitle("Em R$/litro") xtitle("Tempo") title("Custo Marginal - Conduta Cartel e Preço de Atacado") note("Zona 01") scheme(sj) ///
legend(order(1 "CMg Cartel" 2 "Preço Atacado em R$")) ylabel(,format(%4,2f))
graph save fig_cartel.gph, replace
graph export fig_cartel.pdf, replace
graph export fig_cartel.wmf, replace

twoway line input_CMg_Cournot pgas_atac date if Z==1, ytitle("Em R$/litro") xtitle("Tempo") title("Custo Marginal - Conduta Competitiva e Preço de Atacado") note("Zona 01") scheme(sj) ///
legend(order(1 "CMg Competitiva" 2 "Preço Atacado em R$")) ylabel(,format(%4,2f))
graph save fig_cournot.gph, replace
graph export fig_cournot.pdf, replace
graph export fig_cournot.wmf, replace

*graph combine fig_cartel.gph fig_cournot.gph, cols(2) scheme(sj)

gen ano=year(dofm(date))
gen month=month(dofm(date))
graph bar (mean) lerner_Cartel lerner_Cournot lerner_obs, over(month) over(ano) legend(order(1 "Margem Cartel" 2 "Margem Cournot" 3 "Margem Preço Atacado")) ylabel(,format(%4,2f)) scheme(sj) nofill ///
ytitle("Margem como % do Preço de Varejo") title("Margens médias - anos 2014 e 2015")
graph export margens.pdf, replace
graph export margens.wmf, replace

mat results_gas=J(2,2,.)
reg input_CMg_Cartel pgas_atac
nnest input_CMg_Cournot pgas_atac 

mat results_gas[1,1]=r(tcm1)
mat results_gas[1,2]=r(tsigm1)
mat results_gas[2,1]=r(qm1)
mat results_gas[2,2]=r(qsigm1)

mat colnames results_gas=TestStat PValue
mat rownames results_gas=JStat QStat

esttab matrix(results_gas, fmt(%6,4f)) using "Tab_res_gas.tex", tex nomtitle replace
esttab matrix(results_gas, fmt(%6,4f)) using "Tab_res_gas.txt", tab nomtitle replace

******************************************************
* Etanol
******************************************************

****************************************
* Conduta imposta - Gasolina
****************************************

gen input_CMg_CP_etan=m_petan

gen input_CMg_Cartel_etan=m_petan*(1+(1/$elast_aa)*.1+(1/$elast_ag)*(1))+m_pgas*(Qtde_gas/Qtde_etan)*((1/$elast_ga)*.1+(1/$elast_gg)*(1))
*set trace on
gen input_CMg_Cournot_etan=m_petan*(1+(1/$elast_aa)*($sh_e)+(1/$elast_ag)*($sh_g))+m_pgas*(Qtde_gas/Qtde_etan)*((1/$elast_ga)*($sh_e)+(1/$elast_gg)*($sh_g))
*gen input_CMg_Cournot_etan=m_petan*(1+(1/$elast_aa)*($sh_e))+m_pgas*(Qtde_gas/Qtde_etan)*((1/$elast_ga)*($sh_e))

gen lerner_Cartel_etan=(m_petan-input_CMg_Cartel_etan)/m_petan
gen lerner_Cournot_etan=(m_petan-input_CMg_Cournot_etan)/m_petan
gen lerner_obs_etan=(m_petan-petan_atac)/m_petan

twoway line input_CMg_Cartel_etan petan_atac date if Z==1, ytitle("Em R$/litro") xtitle("Tempo") title("Custo Marginal - Conduta Cartel e Preço de Atacado") note("Zona 01") scheme(sj) ///
legend(order(1 "CMg Cartel" 2 "Preço Atacado em R$")) ylabel(,format(%4,2f))
graph save fig_cartel_etan.gph, replace
graph export fig_cartel_etan.pdf, replace
graph export fig_cartel_etan.wmf, replace

twoway line input_CMg_Cournot_etan petan_atac date if Z==1, ytitle("Em R$/litro") xtitle("Tempo") title("Custo Marginal - Conduta Competitiva e Preço de Atacado") note("Zona 01") scheme(sj) ///
legend(order(1 "CMg Competitiva" 2 "Preço Atacado em R$")) ylabel(,format(%4,2f))
graph save fig_cournot_etan.gph, replace
graph export fig_cournot_etan.pdf, replace
graph export fig_cournot_etan.wmf, replace

*graph combine fig_cartel.gph fig_cournot.gph, cols(2) scheme(sj)

graph bar (mean) lerner_Cartel_etan lerner_Cournot_etan lerner_obs_etan, over(month) over(ano) legend(order(1 "Margem Cartel" 2 "Margem Competitiva" 3 "Margem Preço Atacado")) ylabel(,format(%4,2f)) scheme(sj) nofill ///
ytitle("Margem como % do Preço de Varejo") title("Margens médias - anos 2014 e 2015")
graph export margens_etan.pdf, replace
graph export margens_etan.wmf, replace

mat results_etan=J(2,2,.)
reg input_CMg_Cartel_etan petan_atac
nnest input_CMg_Cournot_etan petan_atac 

mat results_etan[1,1]=r(tcm1)
mat results_etan[1,2]=r(tsigm1)
mat results_etan[2,1]=r(qm1)
mat results_etan[2,2]=r(qsigm1)

mat colnames results_etan=TestStat PValue
mat rownames results_etan=JStat QStat

esttab matrix(results_etan, fmt(%6,4f)) using "Tab_res_etan.tex", tex nomtitle replace
esttab matrix(results_etan, fmt(%6,4f)) using "Tab_res_etan.txt", tab nomtitle replace
