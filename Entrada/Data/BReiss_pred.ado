
cap program drop BReiss_pred

program BReiss_pred
	version 12
	*args var fig
	syntax [if] [in] , VARIABLE(varlist) MKTSIZE(varlist) FIXEDCST(varlist) MKTSIZEOFF(varlist) [PROBESTS]

	tempvar p2 p3 p4 p5 s v f maxprob
	*tempvar s v f 
	gen double `s'=0
	foreach var in `mktsize' {
		replace `s'=`s'+[lambda]_b[`var']*`var'
	}
	replace `s'=`s'+`mktsizeoff'
	gen double `v'=0
	foreach var in `variable' {
		replace `v'=`v'+[beta]_b[`var']*`var'
	}
	replace `v'=`v'+[beta]_b[_cons]
	
	gen double `f'=0
	foreach var in `fixedcst' {
		replace `f'=`f'+[gammaL]_b[`var']*`var'
	}
	replace `f'=`f'+[gammaL]_b[_cons]
	local alpha2=[alpha2]_b[_cons]
	local alpha3=[alpha3]_b[_cons]
	local alpha4=[alpha4]_b[_cons]
	local alpha5=[alpha5]_b[_cons]
	local gamma2=[gamma2]_b[_cons]
	local gamma3=[gamma3]_b[_cons]
	local gamma4=[gamma4]_b[_cons]
	local gamma5=[gamma5]_b[_cons]
	
	qui gen double `p2'=normal(`s'*(`v'-`alpha2')-`f'-`gamma2')
	qui gen double `p3'=normal(`s'*(`v'-`alpha2'-`alpha3')-`f'-`gamma2'-`gamma3')
	qui gen double `p4'=normal(`s'*(`v'-`alpha2'-`alpha3'-`alpha4')-`f'-`gamma2'-`gamma3'-`gamma4')
	qui gen double `p5'=normal(`s'*(`v'-`alpha2'-`alpha3'-`alpha4'-`alpha5')-`f'-`gamma2'-`gamma3' -`gamma4'-`gamma5')
	
	qui gen double prob0=(1-normal(`s'*(`v')-`f'))
	qui gen double prob1=(normal(`s'*(`v')-`f')-`p2')
	qui gen double prob2=(`p2'-`p3')
	qui gen double prob3=(`p3'-`p4')
	qui gen double prob4=(`p4'-`p5')
	qui gen double prob5=(`p5')
	qui egen double `maxprob'=rowmax(prob0 prob1 prob2 prob3 prob4 prob5)
	
	
	gen double number=0
	forvalues i=0/5 {
		qui replace number=number+`i'*(prob`i'==`maxprob')
		
	}
end
