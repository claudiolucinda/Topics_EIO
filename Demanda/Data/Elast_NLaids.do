*estimates save est

* Recover the parameters of fifth equation:
*-------------------------------------------
*estimates use est
nlcom (a1:_b[/a1]) (a2:_b[/a2]) (a3:_b[/a3]) (a4:_b[/a4]) ///
	(a5:_b[/a5]) (a6:_b[/a6]) (a7:_b[/a7]) (a8:_b[/a8]) ///
	(a9:1-_b[/a1]-_b[/a2]-_b[/a3]- _b[/a4]-_b[/a5]-_b[/a6]-_b[/a7]- _b[/a8]) ///
	(b1:_b[/b1]) (b2:_b[/b2]) (b3:_b[/b3]) (b4:_b[/b4]) ///
	(b5:_b[/b5]) (b6:_b[/b6]) (b7:_b[/b7]) (b8:_b[/b8]) ///
	(b9:-_b[/b1]-_b[/b2]-_b[/b3]- _b[/b4]-_b[/b5]-_b[/b6]-_b[/b7]- _b[/b8]) ///
	(g1_1: _b[/g1_1]) (g1_2: _b[/g1_2]) (g1_3: _b[/g1_3]) (g1_4: _b[/g1_4]) (g1_5: _b[/g1_5]) (g1_6: _b[/g1_6]) ///
	(g1_7: _b[/g1_7]) (g1_8: _b[/g1_8]) ///
	(g1_9:- _b[/g1_1]- _b[/g1_2]- _b[/g1_3]- _b[/g1_4]- _b[/g1_5]- _b[/g1_6]- _b[/g1_7]- _b[/g1_8]) ///
    (g2_2: _b[/g2_2]) (g2_3: _b[/g2_3]) (g2_4: _b[/g2_4]) (g2_5: _b[/g2_5]) (g2_6: _b[/g2_6]) ///
	(g2_7: _b[/g2_7]) (g2_8: _b[/g2_8]) ///
	(g2_9:- _b[/g1_2]- _b[/g2_2]- _b[/g2_3]- _b[/g2_4]- _b[/g2_5]- _b[/g2_6]- _b[/g2_7]- _b[/g2_8]) ///
    (g3_3: _b[/g3_3]) (g3_4: _b[/g3_4]) (g3_5: _b[/g3_5]) (g3_6: _b[/g3_6]) ///
	(g3_7: _b[/g3_7]) (g3_8: _b[/g1_8]) ///
	(g3_9:- _b[/g1_3]- _b[/g2_3]- _b[/g3_3]- _b[/g3_4]- _b[/g3_5]- _b[/g3_6]- _b[/g3_7]- _b[/g3_8]) ///
    (g4_4: _b[/g4_4]) (g4_5: _b[/g4_5]) (g4_6: _b[/g4_6]) ///
	(g4_7: _b[/g4_7]) (g4_8: _b[/g4_8]) ///
	(g4_9:- _b[/g1_4]- _b[/g2_4]- _b[/g3_4]- _b[/g4_4]- _b[/g4_5]- _b[/g4_6]- _b[/g4_7]- _b[/g4_8]) ///
    (g5_5: _b[/g5_5]) (g5_6: _b[/g5_6]) ///
	(g5_7: _b[/g5_7]) (g5_8: _b[/g5_8]) ///
	(g5_9:- _b[/g1_5]- _b[/g2_5]- _b[/g3_5]- _b[/g4_5]- _b[/g5_5]- _b[/g5_6]- _b[/g5_7]- _b[/g5_8]) ///
    (g6_6: _b[/g6_6]) ///
	(g6_7: _b[/g6_7]) (g6_8: _b[/g6_8]) ///
	(g6_9:- _b[/g1_6]- _b[/g2_6]- _b[/g3_6]- _b[/g4_6]- _b[/g5_6]- _b[/g6_6]- _b[/g6_7]- _b[/g6_8]) ///
    (g7_7: _b[/g7_7]) (g7_8: _b[/g7_8]) ///
	(g7_9:- _b[/g1_7]- _b[/g2_7]- _b[/g3_7]- _b[/g4_7]- _b[/g5_7]- _b[/g6_7]- _b[/g7_7]- _b[/g7_8]) ///
    (g8_8: _b[/g8_8]) ///
	(g8_9:- _b[/g1_8]- _b[/g2_8]- _b[/g3_8]- _b[/g4_8]- _b[/g5_8]- _b[/g6_8]- _b[/g7_8]- _b[/g8_8]) ///
	(g9_9: - (- _b[/g1_1]- _b[/g1_2]- _b[/g1_3]- _b[/g1_4]- _b[/g1_5]- _b[/g1_6]- _b[/g1_7]- _b[/g1_8]) ///
    -(- _b[/g1_2]- _b[/g2_2]- _b[/g2_3]- _b[/g2_4]- _b[/g2_5]- _b[/g2_6]- _b[/g2_7]- _b[/g2_8]) ///
    -(- _b[/g1_3]- _b[/g2_3]- _b[/g3_3]- _b[/g3_4]- _b[/g3_5]- _b[/g3_6]- _b[/g3_7]- _b[/g3_8]) ///
    -(- _b[/g1_4]- _b[/g2_4]- _b[/g3_4]- _b[/g4_4]- _b[/g4_5]- _b[/g4_6]- _b[/g4_7]- _b[/g4_8]) ///
    -(- _b[/g1_5]- _b[/g2_5]- _b[/g3_5]- _b[/g4_5]- _b[/g5_5]- _b[/g5_6]- _b[/g5_7]- _b[/g5_8]) ///
    -(- _b[/g1_6]- _b[/g2_6]- _b[/g3_6]- _b[/g4_6]- _b[/g5_6]- _b[/g6_6]- _b[/g6_7]- _b[/g6_8]) ///
    -(- _b[/g1_7]- _b[/g2_7]- _b[/g3_7]- _b[/g4_7]- _b[/g5_7]- _b[/g6_7]- _b[/g7_7]- _b[/g7_8]) ///
    -(- _b[/g1_8]- _b[/g2_8]- _b[/g3_8]- _b[/g4_8]- _b[/g5_8]- _b[/g6_8]- _b[/g7_8]- _b[/g8_8])), post
	
	
	*(g12_12:- _b[/g1_12]- _b[/g2_12]- _b[/g3_12]- _b[/g4_12]- _b[/g5_12]- _b[/g6_12]- _b[/g7_12]- _b[/g8_12]- _b[/g9_12]- _b[/g10_12]- _b[/g11_12]) 

	***post missing gamma
*-------------------------------------------forvalues i=1/5{          gen gama1`i'=_b[g1`i']
*}
*set trace on
forvalues i=1/9{
	forvalues j=`i'/9{
		qui gen gama`i'_`j'=_b[g`i'_`j']
		if `j'>`i' {
			qui gen gama`j'_`i'=_b[g`i'_`j']
		}
	}
}

/*
*set trace off
*** Generate b(p):
gen double bp=1
local runner=1
foreach var of varlist LP_ACUCAR LP_AVES LP_CARNE LP_CARNE_IND LP_CLO LP_FFM LP_FRUTAS LP_LATICINIOS LP_LEG_VEG LP_OL_GORD LP_TUB LP_PAN {
	replace bp=bp*exp(`var')^(_b[b`runner'])
	local ++runner
}
*gen double bp=p1^_b[b1]*p2^_b[b2]*p3^_b[b3]*p4^_b[b4]*p5^_b[b5]
*/
*** Generate P(p):

local sharenames "sfoodh sfoodr srent soper sfurn scloth stranop srecr spers"
local pricenames "pfoodh pfoodr prent poper pfurn pcloth ptranop precr ppers"


qui gen double lnap  = 5
local runner=1
foreach var of local pricenames {
	qui replace lnap=lnap+_b[a`runner']*`var'
	local ++runner
}

forvalues i = 1/9 {
	local lnpi: word `i' of `pricenames'
	forvalues j = 1/9 {
		local lnpj: word `j' of `pricenames'
		qui replace lnap = lnap + 0.5*gama`i'_`j'*`lnpi'*`lnpj'
	}
}

 ***Budget elasticities
global nprice: word count `pricenames'
global ncols=$nprice+1

 matrix elastsAIDS=J($nprice,$ncols,.)

forvalues i=1/9 {
	local wi: word `i' of `sharenames'
	qui predictnl  eAIDS_`i' = _b[b`i']/`wi' + 1, se(se_`i')
	qui su eAIDS_`i'
	mat elastsAIDS[`i',10]=r(mean)
} 

 ****Price elasticities (uncompensated)
forvalues i=1/9{
	forvalues j=1/9{
		gen gp`j'=0
		forvalues l=1/9{
			local lnpl: word `l' of `pricenames'
			qui replace gp`j'=gp`j'+gama`j'_`l' * `lnpl'
		}
		if `i'==`j'{
			local delt=1
		}
		else {
			local delt=0
		} 
		local wi: word `i' of `sharenames'
	
		qui predictnl eAIDS_`i'_`j'= (gama`i'_`j' -  _b[b`i']*(_b[a`j'] + gp`j'))/`wi' - `delt' ///
			, se(se`i'_`j')
		qui su eAIDS_`i'_`j'
		mat elastsAIDS[`i',`j']=r(mean)
			drop gp`j'
	}
}
drop gama* lnap se*
mat rownames elastsAIDS = `sharenames'
mat colnames elastsAIDS = `pricenames' income
