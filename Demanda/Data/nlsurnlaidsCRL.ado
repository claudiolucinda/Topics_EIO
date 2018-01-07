*! Version 1.0.0 29may2014
*! Claudio R. Lucinda
program nlsurnlaidsCRL

	version 10

	syntax varlist(min=18 max=18) if, at(name)
	
	tokenize `varlist'
	args w1 w2 w3 w4 w5 w6 w7 w8 lnp1 lnp2 lnp3 lnp4 lnp5 lnp6 lnp7 lnp8 lnp9 lnm
	
	
	tempname a1 a2 a3 a4 a5 a6 a7 a8 a9 
	local runner=1
	forvalues i=1/8 {
		scalar `a`i''=`at'[1,`runner']
		local ++runner
	}
	scalar `a9' = 1 - `a1' - `a2' - `a3'- `a4' - `a5' - `a6' - `a7' - `a8' 
	
	tempname b1 b2 b3 b4 b5 b6 b7 b8 b9 
	forvalues i=1/8 {
		scalar `b`i''=`at'[1,`runner']
		local ++runner
	}
	scalar `b9' = -`b1' - `b2' - `b3' -`b4' - `b5' - `b6' -`b7' - `b8' 

	
	forvalues i=1/8 {
		forvalues j=`i'/8 {
			tempname g`i'_`j'
			scalar `g`i'_`j''=`at'[1,`runner']
			if `j'>`i' {
				tempname g`j'_`i'
				scalar `g`j'_`i''=`g`i'_`j''
			}
			local ++runner
			
		}
		tempname g`i'_9
		scalar `g`i'_9'=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'-`g`i'_6'-`g`i'_7'-`g`i'_8'
		tempname g9_`i'
		scalar `g9_`i''=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'-`g`i'_6'-`g`i'_7'-`g`i'_8'
		
		*local ++runner
	}
	local i=9
	tempname g9_9
	scalar `g9_9'=-`g`i'_1'-`g`i'_2'-`g`i'_3'-`g`i'_4'-`g`i'_5'-`g`i'_6'-`g`i'_7'-`g`i'_8'
/*
	tempname ll1 ll2 ll3 ll4 ll5 ll6 ll7 ll8 ll9 ll10 ll11 ll12
	forvalues i=1/11 {
		scalar `ll`i''=`at'[1,`runner']
		local ++runner
	}
	scalar `ll12' = -`ll1' - `ll2' - `ll3' -`ll4' - `ll5' - `ll6' -`ll7' - `ll8' - `ll9'-`ll10' - `ll11'
		

	
	tempname a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
	scalar `a1' = `at'[1,1]	
	scalar `a2' = `at'[1,2]	
	scalar `a3' = `at'[1,3]	
	scalar `a4' = `at'[1,4]	
	scalar `a5' = `at'[1,5]	
	scalar `a6' = `at'[1,6]	
	scalar `a7' = `at'[1,7]	
	scalar `a8' = `at'[1,8]	
	scalar `a9' = `at'[1,9]	
	scalar `a10' = `at'[1,10]	
	scalar `a11' = `at'[1,11]	
	scalar `a12' = 1 - `a1' - `a2' - `a3'- `a4' - `a5' - `a6' - `a7' - `a8' - `a9'- `a10' - `a11'
	
	tempname b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12
	scalar `b1' = `at'[1,13]
	scalar `b2' = `at'[1,14]
	scalar `b3' = `at'[1,15]
	scalar `b4' = `at'[1,16]
	scalar `b5' = `at'[1,17]
	scalar `b6' = `at'[1,18]
	scalar `b7' = `at'[1,19]
	scalar `b8' = `at'[1,20]
	scalar `b9' = `at'[1,21]
	scalar `b10' = `at'[1,22]
	scalar `b11' = `at'[1,23]
	scalar `b4' = -`b1' - `b2' - `b3'-`b4' - `b5' - `b6'-`b7' - `b8' - `b9'-`b10' - `b11'

	tempname g11 g12 g13 g14 g15 g16 g17 g18 g19 g110 g111 g112
	tempname g21 g22 g23 g24 g25 g26 g27 g28 g29 g210 g211 g212
	tempname g31 g32 g33 g34 g35 g36 g37 g38 g39 g310 g311 g312
	tempname g41 g42 g43 g44 g45 g46 g47 g48 g49 g410 g411 g412
	tempname g51 g52 g53 g54 g55 g56 g57 g58 g59 g510 g511 g512
	tempname g61 g62 g63 g64 g65 g66 g67 g68 g69 g610 g611 g612
	tempname g71 g72 g73 g74 g75 g76 g77 g78 g79 g710 g711 g712
	tempname g81 g82 g83 g84 g85 g86 g87 g88 g89 g810 g811 g812
	tempname g91 g92 g93 g94 g95 g96 g97 g98 g99 g910 g911 g912
	tempname g101 g102 g103 g104 g105 g106 g107 g108 g109 g1010 g1011 g1012
	tempname g111 g112 g113 g114 g115 g116 g117 g118 g119 g1110 g1111 g1112
	scalar `g11' = `at'[1,7]
	scalar `g12' = `at'[1,8]
	scalar `g13' = `at'[1,9]
	scalar `g11' = `at'[1,7]
	scalar `g12' = `at'[1,8]
	scalar `g13' = `at'[1,9]
	scalar `g14' = -`g11' - `g12' - `g13'

	scalar `g21' = `g12'
	scalar `g22' = `at'[1,10]
	scalar `g23' = `at'[1,11]
	scalar `g24' = -`g21' - `g22' - `g23'

	scalar `g31' = `g13'
	scalar `g32' = `g23'
	scalar `g33' = `at'[1,12]
	scalar `g34' = -`g31' - `g32' - `g33'

	scalar `g41' = `g14'
	scalar `g42' = `g24'
	scalar `g43' = `g34'
	scalar `g44' = -`g41' - `g42' - `g43'
	
	tempname l1 l2 l3 l4
	scalar `l1' = `at'[1,13]
	scalar `l2' = `at'[1,14]
	scalar `l3' = `at'[1,15]
	scalar `l4' = -`l1' - `l2' - `l3'

*/

	// Okay, now that we have all the parameters, we can 
	// calculate the expenditure shares.	
	quietly {
		// First get the price index
		// I set a_0 = 5
		tempvar lnpindex
		gen double `lnpindex' = 5 
		
		forvalues i=1/9 {
			replace `lnpindex'=`lnpindex'+ `a`i''*`lnp`i''
		}
		forvalues i = 1/9 {
			forvalues j = 1/9 {
				replace `lnpindex' = `lnpindex' + 	///
					0.5*`g`i'_`j''*`lnp`i''*`lnp`j''
			}
		}
		// The b(p) term in the QUAIDS model:
		tempvar bofp
		gen double `bofp' = 0
		forvalues i = 1/9 {
			replace `bofp' = `bofp' + `lnp`i''*`b`i''
		}
		replace `bofp' = exp(`bofp')
		// Finally, the expenditure shares for 3 of the 4
		// goods (the fourth is dropped to avoid singularity)
		forvalues i=1/8 {
			replace `w`i'' = `a`i'' + `g`i'_1'*`lnp1' + `g`i'_2'*`lnp2' +	///
				      `g`i'_3'*`lnp3' + `g`i'_4'*`lnp4' +`g`i'_5'*`lnp5' + `g`i'_6'*`lnp6' +	///
				      `g`i'_7'*`lnp7' + `g`i'_8'*`lnp8' +	///
				      `b`i''*(`lnm' - `lnpindex')
*				      `ll`i''/`bofp'*(`lnm' - `lnpindex')^2
*			su `w`i''
					  
		}			  
					  
}
					  /*					  
		replace `w2' = `a2' + `g21'*`lnp1' + `g22'*`lnp2' +	///
				      `g23'*`lnp3' + `g24'*`lnp4' +	///
				      `b2'*(`lnm' - `lnpindex') +	///
				      `l2'/`bofp'*(`lnm' - `lnpindex')^2
		replace `w3' = `a3' + `g31'*`lnp1' + `g32'*`lnp2' +	///
				      `g33'*`lnp3' + `g34'*`lnp4' +	///
				      `b3'*(`lnm' - `lnpindex') +	///
				      `l3'/`bofp'*(`lnm' - `lnpindex')^2
				      
	}
*/	
end
