*estimates save est

* Recover the parameters of fifth equation:
*-------------------------------------------
*estimates use est
nlcom (a1:_b[/a1]) (a2:_b[/a2]) (a3:_b[/a3]) (a4:_b[/a4]) ///
	(a5:_b[/a5]) (a6:1-_b[/a1]-_b[/a2]-_b[/a3]- _b[/a4]-_b[/a5]) ///
	(b1:_b[/b1]) (b2:_b[/b2]) (b3:_b[/b3]) (b4:_b[/b4]) ///
	(b5:_b[/b5]) (b6:-_b[/b1]-_b[/b2]-_b[/b3]- _b[/b4]-_b[/b5]) ///
	(g1_1: _b[/g1_1]) (g1_2: _b[/g1_2]) (g1_3: _b[/g1_3]) (g1_4: _b[/g1_4]) (g1_5: _b[/g1_5]) ///
	(g1_6:- _b[/g1_1]- _b[/g1_2]- _b[/g1_3]- _b[/g1_4]- _b[/g1_5]) ///
    (g2_2: _b[/g2_2]) (g2_3: _b[/g2_3]) (g2_4: _b[/g2_4]) (g2_5: _b[/g2_5]) ///
	(g2_6:- _b[/g1_2]- _b[/g2_2]- _b[/g2_3]- _b[/g2_4]- _b[/g2_5]) ///
    (g3_3: _b[/g3_3]) (g3_4: _b[/g3_4]) (g3_5: _b[/g3_5]) ///
	(g3_6:- _b[/g1_3]- _b[/g2_3]- _b[/g3_3]- _b[/g3_4]- _b[/g3_5]) ///
    (g4_4: _b[/g4_4]) (g4_5: _b[/g4_5])  ///
	(g4_6:- _b[/g1_4]- _b[/g2_4]- _b[/g3_4]- _b[/g4_4]- _b[/g4_5]) ///
    (g5_5: _b[/g5_5]) ///
	(g5_6:- _b[/g1_5]- _b[/g2_5]- _b[/g3_5]- _b[/g4_5]- _b[/g5_5]) ///
    (g6_6: - (- _b[/g1_1]- _b[/g1_2]- _b[/g1_3]- _b[/g1_4]- _b[/g1_5]) ///
    -(- _b[/g1_2]- _b[/g2_2]- _b[/g2_3]- _b[/g2_4]- _b[/g2_5]) ///
    -(- _b[/g1_3]- _b[/g2_3]- _b[/g3_3]- _b[/g3_4]- _b[/g3_5]) ///
    -(- _b[/g1_4]- _b[/g2_4]- _b[/g3_4]- _b[/g4_4]- _b[/g4_5]) ///
    -(- _b[/g1_5]- _b[/g2_5]- _b[/g3_5]- _b[/g4_5]- _b[/g5_5])), post
	
	
	*(g12_12:- _b[/g1_12]- _b[/g2_12]- _b[/g3_12]- _b[/g4_12]- _b[/g5_12]- _b[/g6_12]- _b[/g7_12]- _b[/g8_12]- _b[/g9_12]- _b[/g10_12]- _b[/g11_12]) 

	***post missing gamma
*-------------------------------------------forvalues i=1/5{          gen gama1`i'=_b[g1`i']
*}
*set trace on
global sharevars tsval_soja tsval_milho tsval_girassol tsval_composto tsval_oliva tsval_canola
global pricevars l_p_soja l_p_milho l_p_girassol l_p_composto l_p_oliva l_p_canola
global priceind l_y_P


global nprice: word count $pricevars
global ncols=$nprice+1
matrix elastsLAAIDS=J($nprice,$ncols,.)

forvalues i=1/$nprice{
	forvalues j=`i'/$nprice{
		qui gen gama`i'_`j'=_b[g`i'_`j']
		if `j'>`i' {
			qui gen gama`j'_`i'=_b[g`i'_`j']
		}
	}
}

*** Generate P(p):
gen double lnap  = 5
local runner=1
foreach var of global pricevars {
	qui replace lnap=lnap+_b[a`runner']*`var'
}

forvalues i = 1/$nprice {
	local lnpi: word `i' of $pricevars
	forvalues j = 1/$nprice {
		local lnpj: word `j' of $pricevars
		qui replace lnap = lnap + 0.5*gama`i'_`j'*`lnpi'*`lnpj'
	}
}

 ***Budget elasticities
* set trace on
forvalues i=1/$nprice {
	local wi: word `i' of $sharevars
	qui predictnl  eLAAIDS_`i' = _b[b`i']/`wi' + 1, se(se_`i')
	qui su eLAAIDS_`i'
	mat elastsLAAIDS[`i',$ncols]=r(mean)
	drop eLAAIDS_`i' se_`i'
} 


local temp=$nprice-1
 ****Price elasticities (uncompensated)
forvalues i=1/$nprice {
	forvalues j=1/$nprice {
		qui gen gp`j'=0
		forvalues l=1/`temp'{
			local lnpl: word `l' of $pricevars
			qui replace gp`j'=gp`j'+gama`j'_`l' * `lnpl'
		}
		if `i'==`j'{
			local delt=1
		}
		else {
			local delt=0
		} 
		local wi: word `i' of $sharevars
	
		qui predictnl eLAAIDS_`i'_`j'= (gama`i'_`j' -  _b[b`i']*(_b[a`j'] + gp`j'))/`wi' - `delt' ///
			, se(se`i'_`j')
		qui su eLAAIDS_`i'_`j'
		mat elastsLAAIDS[`i',`j']=r(mean)
			drop gp`j'
	}
}
drop gama* lnap se*
mat rownames elastsLAAIDS = $sharevars
mat colnames elastsLAAIDS = $pricevars income
estout matrix(elastsLAAIDS, fmt(%9.3f))
