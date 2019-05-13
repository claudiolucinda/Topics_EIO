***************************************************************
* Teste Parâmetro de Conduta - Dados Agregados
* Claudio R. Lucinda
* 2017
* NOTA: NÃO SÃO OS DADOS ORIGINAIS. APENAS PARA FINS DIDÁTICOS
***************************************************************

clear 
cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Conduta\Data\"

use dados.dta, clear

local nomes "RPK-Rev. Pass. Km" "Qtde" "Desemprego" "Preço Produtor" "Renda" "Preço Distribuidor"
local ynames "Pass." "Metros Cúbicos" "Índice" "R$/l" "Índice PIM/PF" "R$/l"
local i=1
gen tempdate=t+tm(2002m01)
format tempdate %tm

foreach var in rpk LNQ Desemp Pprod Renda pdist2 {
	qui gen E_`var'=exp(`var')
}	

gen l_pprod=l.Pprod
gen E_l_pprod=l.E_Pprod

reg3 (eq_dem: E_LNQ E_pdist2 E_rpk E_Renda E_Desemp) (eq_of:E_pdist2 E_l_pprod E_LNQ), inst(l_pprod rpk Renda Desemp)
testnl -[eq_of]_b[E_LNQ]*[eq_dem]_b[E_pdist2]=0
estadd scalar p_pc=r(p)
testnl -[eq_of]_b[E_LNQ]*[eq_dem]_b[E_pdist2]=1
estadd scalar p_monop=r(p)
testnl -[eq_of]_b[E_LNQ]*[eq_dem]_b[E_pdist2]=.381167
estadd scalar p_cournot=r(p)

nlcom -[eq_of]_b[E_LNQ]*[eq_dem]_b[E_pdist2]
matrix kkk=r(b)
matrix lll=r(V)
local kkk1=kkk[1,1]
local lll1=lll[1,1]^.5
estadd scalar theta=`kkk1'
estadd scalar sigtheta=`lll1' 
estimates store mod01, title(Resultados)

/*
estout mod01 using Resultados.txt, ///
cells(b(fmt(%6,3f) star) t(par fmt(%6,3f)) ) /// 
stats(N r2_1 r2_2 theta sigtheta p_pc p_monop p_cournot, labels("N" "R-quad Dem" "R-quad Of" "Theta" "D.P.(Theta)" "Pval (Comp.)" "Pval (Monop.)" "Pval (Cournot)") fmt(%6,0f %9,3f)) ///
varlabels(_cons Constante pdist2 "Preço Dist." rpk "RPK" Renda "Renda" Desemp "Desemprego" LNQ "Quantidade" l_pprod "Preço Prod.") ///
eqlabels("Eq. Demanda" "Eq. Oferta") postfoot("@starlegend - Est. t em parênteses") style(tab) ///
stardetach replace
*/

