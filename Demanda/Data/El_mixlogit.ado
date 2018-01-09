cap program drop El_mixlogit

program El_mixlogit, rclass
version 13

syntax [if], PRICE(varname) CHOICEVAR(varname) [PRICEINT(varname)]

	if ("`e(cmd)'" != "mixlogit") error 301

	local cmd `e(cmd)'

	** Mark the prediction sample **
	marksample touse, novarlist

	tempvar prob0
	qui mixlpred `prob0' if `touse'
	levelsof `choicevar', local(choices)
	global n_esc: word count `choices'

	matrix elasts=J($n_esc,$n_esc,0)

	tempvar backprice
	qui gen `backprice'=`price'

	if "`priceint'" != "" {
		tempvar backint
		qui gen `backint'=`priceint'
	}

	tempvar dprob
	tempvar m_elast
	tempvar prob1
	tempvar mean_share
	forvalues i=1/$n_esc {
		qui replace `price'=1.01*`price' if `choicevar'==`i' & `touse'
		if "`priceint'" != "" {
			qui replace `priceint'=1.01*`priceint' if `choicevar'==`i' & `touse'
		}
		cap drop `prob1'
		qui mixlpred `prob1' if `touse'

		qui gen double `dprob'=2*(`prob1'-`prob0')/(`prob0'+`prob1') if `touse'
		qui bysort `choicevar': egen double `m_elast'=mean(`dprob') if `touse'
		forvalues j=1/$n_esc {
			qui su `m_elast' if `touse'  & `choicevar'==`j' 
			local elasti=100*r(mean)
			matrix elasts[`i',`j']=`elasti'
			
		}
		qui replace `price'=`backprice'
		if "`priceint'" != "" {
			replace `priceint'=`backint'
		}
		drop `prob1'
		drop `dprob'
		drop `m_elast'
	}

	matrix rownames elasts=`choices'
	matrix colnames elasts=`choices'
	return matrix elast=elasts
end
