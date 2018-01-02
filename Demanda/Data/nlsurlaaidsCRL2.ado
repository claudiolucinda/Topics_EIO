*! Version 1.0.0 29may2014
*! Claudio R. Lucinda

program nlsurlaaidsCRL2

*	version 10

	syntax varlist(min=12 max=12) if, at(name)
	
	tokenize `varlist'
	args w1 w2 w3 w4 w5 lnp1 lnp2 lnp3 lnp4 lnp5 lnp6 lnwp
	
	
	tempname a1 a2 a3 a4 a5 a6
	local runner=1
	forvalues i=1/5 {
		scalar `a`i''=`at'[1,`runner']
		local ++runner
	}
	scalar `a6' = 1 - `a1' - `a2' - `a3'- `a4' - `a5' 
	
	tempname b1 b2 b3 b4 b5 b6 
	forvalues i=1/5 {
		scalar `b`i''=`at'[1,`runner']
		local ++runner
	}
	scalar `b6' = -`b1' - `b2' - `b3' -`b4' - `b5' 

	
	forvalues i=1/5 {
		forvalues j=`i'/5 {
			tempname g`i'_`j'
			scalar `g`i'_`j''=`at'[1,`runner']
			if `j'>`i' {
				tempname g`j'_`i'
				scalar `g`j'_`i''=`g`i'_`j''
			}
			local ++runner
			
		}
		tempname g`i'_6
		scalar `g`i'_6'=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'
		tempname g6_`i'
		scalar `g6_`i''=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'
		
		*local ++runner
	}
	local i=6
	tempname g6_6
	scalar `g6_6'=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'

	// Okay, now that we have all the parameters, we can 
	// calculate the expenditure shares.	
	quietly {
		// First get the price index
		// I set a_0 = 5
		// Finally, the expenditure shares for 3 of the 4
		// goods (the fourth is dropped to avoid singularity)
		forvalues i=1/5 {
			replace `w`i'' = `a`i'' + `g`i'_1'*`lnp1' + `g`i'_2'*`lnp2' +	///
				      `g`i'_3'*`lnp3' + `g`i'_4'*`lnp4' +`g`i'_5'*`lnp5' + `g`i'_6'*`lnp6' +	///
				      `b`i''*`lnwp'
					  
		}			  
					  
	}


end
