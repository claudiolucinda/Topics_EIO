cap program drop logsumNL
program define logsumNL, rclass
	version 13
	*args var fig
	syntax newvarlist(max=1) [if], CONDSHARE(varlist) MKTVAR(varlist) NESTVAR(varlist) PRICEVAR(varlist)
	
	if ("`e(cmd)'" != "ivregress") & ("`e(cmd)'" != "ivreg2") error 301
	
	** Mark the prediction sample **
	marksample touse, novarlist

		
	tempvar meanval
	qui predict double `meanval' if `touse', xb
	
	tempvar ksi
	qui predict double `ksi' if `touse', resid
	
	qui replace `meanval'=`meanval'-_b[`condshare']*`condshare' if `touse'
	
	qui replace `meanval'=`meanval'+`ksi' if `touse'
	
	qui levelsof `nestvar', local(nest)
	
	tempvar temp0
	tempvar temp1
	qui gen `temp0'=0
	foreach n of local nest {
	
	qui bysort `mktvar': egen `temp1'=sum(`meanval') if `touse' & `nestvar'==`n'
	qui replace `temp1'=0 if `touse' & `temp1'==.
	qui replace `temp1'=exp(`temp1')^(1-_b[`condshare'])
	qui replace `temp0'=`temp0'+`temp1'
	drop `temp1'
	
	}
	
	gen `varlist'=log(`temp0')/(abs(_b[`pricevar']))
	
	end


