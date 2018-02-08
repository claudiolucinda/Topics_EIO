********************************************************************************
* Código para Replicação da Análise apresentada no Parecer Barrionuevo & Lucinda
* 2017
********************************************************************************

* Diretório de Trabalho
cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Conduta\Data\Non_Nested\"



cap log close
log using "4.0.1 LA-AIDS Modelo LA-AIDS Municipal sem homogeneidade.log", replace
*******************************************************************************************************************************************
* Modelo 1: Exógenas: Preço Compra FOB GAS / ETANOL e Instrumento Flex, com restrição de homogeneidade ************************************


use "final_municipal.dta", clear
replace frota_total=frota_total/1000000
replace frota_flex=frota_flex/1000000
replace pop=pop/1000000


global mod_gasolina   "S_int_gas logExpend_nt Pr_int_gas Pr_int_etl ESgas ESetl MSgas MSetl MGgas MGetl PRgas PRetl MTgas MTetl BAgas BAetl SEgas SEetl PEgas PEetl Leigas Leietl frota_total frota_flex pop homens postos d_mes_1-d_mes_3 d_mes_5-d_mes_12 trend_id i.region"
global mod_estagio_sup "Qtot_nt Renda_nt logP_nt frota_total frota_flex pop educ1a3 educ4a7 educmais8 postos"
global endogenas "logP_nt Pr_int_gas Pr_int_etl frota_flex"
global exogenas "oil_defl sugar_defl prcompragascif_defl prcompraetlcif_defl inst_flex"


gen inferior = "Frota/Postos"
gen instrumentos = "Preços CIF/Flex"
gen homogeneo =1

do "4.0.1 LA-AIDS Modelo LA-AIDS Municipal.do"

est restore mod01
predict temp_sh, eq(S_int_gas) xb

qui gen alpha_gas=temp_sh-([S_int_gas]_b[logExpend_nt]*logExpend_nt+[S_int_gas]_b[Pr_int_gas]*Pr_int_gas+[S_int_gas]_b[Pr_int_etl])
qui gen alpha_etl=1-alpha_gas

qui gen ln_c=alpha_gas*Pr_int_gas+alpha_etl*Pr_int_etl+0.5*([S_int_gas]_b[Pr_int_gas]*(Pr_int_gas^2)+2*[S_int_gas]_b[Pr_int_etl]*Pr_int_gas*Pr_int_etl ///
-[S_int_gas]_b[Pr_int_etl]*(Pr_int_etl^2))+((exp(Pr_int_gas))^([S_int_gas]_b[logExpend])*(exp(Pr_int_gas))^(-[S_int_gas]_b[logExpend]))

gen Pr_int_gas_OLD=Pr_int_gas
replace Pr_int_gas=1.1*Pr_int_gas if ano>2010

qui gen ln_c2=alpha_gas*Pr_int_gas+alpha_etl*Pr_int_etl+0.5*([S_int_gas]_b[Pr_int_gas]*(Pr_int_gas^2)+2*[S_int_gas]_b[Pr_int_etl]*Pr_int_gas*Pr_int_etl ///
-[S_int_gas]_b[Pr_int_etl]*(Pr_int_etl^2))+((exp(Pr_int_gas))^([S_int_gas]_b[logExpend])*(exp(Pr_int_gas))^(-[S_int_gas]_b[logExpend]))

gen exp_0=exp(ln_c)
gen exp_a=exp(ln_c2)

su exp_0 exp_a if ano>2010

gen delta=abs(exp_a-exp_0)*pop*1e6

/*
estout mod01 , cells(b(fmt(%6.4f) star) t(par fmt(%6.4f)) ) /// 
stats(N chi2 ll r2_1 r2_2, labels(`"Observações"' `"LR chi2"' `"Log-Lik."' `"R2 Ajustado"' `"R2 Ajustado"')) indicate("Interação Tempo e Lei=d_mes_*" "Dummies Municipio=_Iregion*") label style(tex) ///
varlabels(_cons Constante) replace prehead("\tiny" "\begin{tabular}{l*{@M}{cc}}" "\hline \hline") posthead("\hline") ///
prefoot("\hline") postfoot("\hline \hline" "\multicolumn{@span}{p{12cm}}{\small\textit{Source:} Elaboração do Autor. @starlegend}" "\end{tabular}" ) stardetach ///
drop(ESgas ESetl MSgas MSetl MGgas MGetl PRgas PRetl MTgas MTetl BAgas BAetl SEgas SEetl PEgas PEetl Leigas Leietl educ1a3 educ4a7 educmais8 postos trend_id)
/*
qui estout mod01 using "../Tab_dem.tex", cells(b(fmt(%6.4f) star) t(par fmt(%6.4f)) ) /// 
stats(N chi2 ll r2_1 r2_2, labels(`"Observações"' `"LR chi2"' `"Log-Lik."' `"R2 Ajustado"' `"R2 Ajustado"')) indicate("Interação Tempo e Lei=d_mes_*" "Dummies Municipio=_Iregion*") label style(tex) ///
varlabels(_cons Constante) replace prehead("\tiny" "\begin{tabular}{l*{@M}{cc}}" "\hline \hline") posthead("\hline") ///
prefoot("\hline") postfoot("\hline \hline" "\multicolumn{@span}{p{12cm}}{\small\textit{Source:} Elaboração do Autor. @starlegend}" "\end{tabular}" ) stardetach ///
drop(ESgas ESetl MSgas MSetl MGgas MGetl PRgas PRetl MTgas MTetl BAgas BAetl SEgas SEetl PEgas PEetl Leigas Leietl educ1a3 educ4a7 educmais8 postos trend_id) 
*/
estout elast01 , cells(b(fmt(%6.4f) star) t(par fmt(%6.4f)) ) /// 
label style(tex) varlabels(_cons Constante) replace prehead("\tiny" "\begin{tabular}{l*{@M}{cc}}" "\hline \hline") posthead("\hline") ///
prefoot("\hline") postfoot("\hline \hline" "\multicolumn{@span}{p{12cm}}{\small\textit{Source:} Elaboração do Autor. @starlegend}" "\end{tabular}" ) stardetach


qui estout elast01 using "../Tab_elast.tex", cells(b(fmt(%6.4f) star) t(par fmt(%6.4f)) ) /// 
label style(tex) varlabels(_cons Constante) replace prehead("\tiny" "\begin{tabular}{l*{@M}{cc}}" "\hline \hline") posthead("\hline") ///
prefoot("\hline") postfoot("\hline \hline" "\multicolumn{@span}{p{12cm}}{\small\textit{Source:} Elaboração do Autor. @starlegend}" "\end{tabular}" ) stardetach 
*/
global elast_gg=Elast[1,1]
global elast_ga=Elast[1,2]
global elast_aa=Elast[1,3]
global elast_ag=Elast[1,4]
global sh_g=.186
global sh_e=.0186

log close

do "Conduta.do"
