***************************************************************
* Teste Parâmetro de Conduta - Dados Agregados
* Claudio R. Lucinda
* 2017
* NOTA: NÃO SÃO OS DADOS ORIGINAIS. APENAS PARA FINS DIDÁTICOS
***************************************************************

clear 
cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Conduta\Data\"


use "BaseFinal.dta", clear

* cortando o banco de dados

keep if date_full>=tm(2006m1) & date_full<=tm(2015m3)

* Transformando o RPK em índice
su rpk if date_full==tm(2006m1)
local temp=r(mean)

gen ind_rpk=rpk*100/`temp'

* Raitzen
* Cons_m3_TOT Fat_L_USD_TOT Fat_B_USD_TOT Cons_m3_VIS Fat_L_USD_VIS Fat_B_USD_VIS

foreach var in Cons_m3_TOT Cons_m3_VIS {
	replace `var'=`var'/1000
}

* BR
* Fat_BRL Cons_m3 Pref_lt

* Vamos usar o VIS para a Raizen
gen Fat_L_BRL_VIS=Fat_L_USD_VIS*Cbio if date_full<tm(2010m4)
replace Fat_L_BRL_VIS=Fat_L_USD_VIS if date_full>=tm(2010m4)
gen P_L_BRL_VIS=Fat_L_BRL_VIS/Cons_m3_VIS
gen P_L_BRL_BP=Fat_BRL/Cons_m3 

gen P_L_REF=P_ref*1000

* Calculando o Preço AirBP

gen P_L_AIRBP=1000*Cons_BRL_AIRBP/Cons_lts_AIRBP
replace P_L_AIRBP=. if date_full==tm(2012m9)

gen Q_tot=Cons_m3_VIS+Cons_m3
egen P_tot=rowmean(P_L_BRL_VIS P_L_BRL_BP)

su IPA_10 if date_full==tm(2015m3)
local temp=r(mean)

foreach var in P_tot P_L_BRL_BP P_L_BRL_VIS P_L_REF P_L_AIRBP {
	gen `var'_R = `var'*100/`temp'
}
tsset date_full

gen  l_pprod=l.P_L_REF_R

gen MG_1=(P_L_BRL_VIS_R-P_L_REF_R)/P_L_REF_R
gen MG_2=(P_L_BRL_BP_R-P_L_REF_R)/P_L_REF_R

gen MG_3=(P_L_BRL_VIS_R-l_pprod)/l_pprod
gen MG_4=(P_L_BRL_BP_R-l_pprod)/l_pprod

line MG_3 MG_4 date_full
su MG_*


* Conjectura constante
constraint define 1 [eq_of1]_b[Cons_m3_VIS]=[eq_of2]_b[Cons_m3]
eststo Mod01: reg3 (eq_dem: Q_tot P_tot_R ind_rpk Sal_ind Desemp) (eq_of1: P_L_BRL_VIS_R Cons_m3_VIS l_pprod) (eq_of2: P_L_BRL_BP_R Cons_m3 l_pprod), inst(l_pprod ind_rpk Sal_ind Desemp) constraints(1)

eststo CP01: nlcom (cp: -[eq_of1]_b[Cons_m3_VIS]*[eq_dem]_b[P_tot]), post
test cp=0
estadd scalar p_cp=r(p)
test cp=1/.5
estadd scalar p_monop=r(p)
test cp=1
estadd scalar p_cournot=r(p)


eststo Mod02: reg3 (eq_dem: Q_tot P_tot_R ind_rpk Sal_ind Desemp) (eq_of1: P_L_BRL_VIS_R Cons_m3_VIS l_pprod) (eq_of2: P_L_BRL_BP_R Cons_m3 l_pprod), inst(l_pprod ind_rpk Sal_ind Desemp) 
test[eq_of1]_b[Cons_m3_VIS]=[eq_of2]_b[Cons_m3]

eststo CP02: nlcom (pc1:-[eq_of1]_b[Cons_m3_VIS]*[eq_dem]_b[P_tot]) (pc2:-[eq_of2]_b[Cons_m3]*[eq_dem]_b[P_tot]), post 
test pc1=0
estadd scalar p_cp1=r(p)
test pc1=1/.5
estadd scalar p_monop1=r(p)
test pc1=1
estadd scalar p_cournot1=r(p)
test pc2=0
estadd scalar p_cp2=r(p)
test pc2=1/.5
estadd scalar p_monop2=r(p)
test pc2=1
estadd scalar p_cournot2=r(p)

test (pc1=0) (pc2=0)

/*
estout Mod01 Mod02 using Resultados_mods.txt, ///
cells(b(fmt(%6,3f) star) t(par fmt(%6,3f)) ) /// 
stats(N r2_1 r2_2 r2_3, labels("N" "R-quad Dem" "R-quad Of Raizen" "R-quad Of BR") fmt(%6,0f %9,3f)) ///
varlabels(_cons Constante P_tot_R "Preço QAV Médio." ind_rpk "RPK" Sal_ind "Renda" Desemp "Desemprego" Q_tot "Qtde Total" l_pprod "Preço Prod." ///
Cons_m3_VIS "Qtde Raizen" Cons_m3 "Qtde BR") ///
eqlabels("Eq. Demanda" "Eq. Oferta") postfoot("@starlegend - Est. t em parênteses") style(tab) ///
stardetach replace

estout CP01 CP02 using Resultados_CPars.txt, ///
cells(b(fmt(%6,3f) star) t(par fmt(%6,3f)) ) /// 
stats(p_cp p_monop p_cournot p_cp1 p_monop1 p_cournot1 p_cp2 p_monop2 p_cournot2, labels("C.P" "Cartel" "Cournot" "C.P. Raizen" "Cartel Raizen" "Cournot Raizen" "C.P. BR" "Cartel BR" "Cournot BR") fmt(%9,3f)) ///
eqlabels("Mod. 01" "Mod.02") postfoot("@starlegend - Est. t em parênteses") style(tab) ///
stardetach replace


gen sh_1=(Cons_m3_VIS/Q_tot)
gen sh_2=(Cons_m3/Q_tot)
*/
