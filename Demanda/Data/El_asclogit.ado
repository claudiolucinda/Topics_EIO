cap program drop El_asclogit
program define El_asclogit, rclass

syntax [if], PRICE(varname) CHOICEVAR(varname) [PRICEINT(varname)]

if ("`e(cmd)'" != "asclogit") error 301

local cmd `e(cmd)'

** Mark the prediction sample **
marksample touse, novarlist

tempvar prob0
qui predict `prob0' if `touse', pr

qui levelsof `choicevar', local(choices)
global n_esc: word count `choices'

matrix elasts=J($n_esc,$n_esc,0)

tempvar backprice
qui gen `backprice'=`price'

if "`priceint'" != "" {
	tempvar backint
	qui gen `backint'=`priceint'
}


forvalues i=1/$n_esc {
	qui replace `price'=1.01*`price' if `choicevar'==`i' & `touse'
	if "`priceint'" !="" {
		qui replace costxrenda=1.01*`priceint' if `choicevar'==`i' & `touse'
	}
	cap drop prob1
	qui predict prob1 if e(sample)

	qui gen diffprob_perc=2*(prob1-`prob0')/(`prob0'+prob1) if `touse'
	qui bysort `choicevar': egen mean_elast=mean(diffprob_perc) if `touse'
	forvalues j=1/$n_esc {
		qui su mean_elast if `choicevar'==`j'
		local elasti=100*r(mean)
		matrix elasts[`i',`j']=`elasti'
		
	}
	qui replace `price'=`backprice'
	if "`priceint'" != "" {
		qui replace `priceint'=`backint'
	}
	drop prob1 diffprob_perc mean_elast
}

matrix rownames elasts=`choices'
matrix colnames elasts=`choices'
return matrix elast=elasts


end
