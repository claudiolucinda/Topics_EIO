*! mixlogsum 1.0.0 27042016
*! author Claudio Ribeiro Lucinda
*! FEA-RP/USP
*!
cap program drop mixlogsum
program define mixlogsum
	version 9.2

	syntax newvarname [if] [in], [NREP(integer 50) BURN(integer 15)] INTVAR(varlist numeric max=1) INTCOEFF(name)


	if ("`e(cmd)'" != "mixlogit") error 301

	local cmd `e(cmd)'
	
	** Mark the prediction sample **
	marksample touse, novarlist
	markout `touse' `e(indepvars)' `e(group)'

	** Generate variables used to sort data **
	tempvar sorder altid
	gen `sorder' = _n
	sort `e(group)'
	by `e(group)': gen `altid' = _n 
	
	** Drop data not in prediction sample **
	preserve
	qui keep if `touse'
	
	** Generate individual id **
	qui duplicates report `e(group)'
	mata: mixl_np = st_numscalar("r(unique_value)")
	mata: mixl_T = J(st_nobs(),1,1)

	** Generate choice occacion id **
	tempvar csid
	sort `e(group)'
	by `e(group)': egen `csid' = sum(1)
	qui duplicates report `e(group)'
	local nobs = r(unique_value)

	** Sort data **
	sort `e(group)' `altid'

	** Set Mata matrices to be used in prediction routine **
	local rhs `e(indepvars)'
	mata: mixl_X = st_data(., tokens(st_local("rhs")))
	mata: mixl_CSID = st_data(., ("`csid'"))
	mata: mixl_ITER=st_data(., ("`intvar'"))
	local totobs = _N	

	** Restore data **
	restore
	local nrep `e(nrep)'
	tempname b
	matrix `b' = e(b)
	matrix B_s=e(b)
	mat intercoeff=B_s[1,"Mean:`intcoeff'"]
	scalar int_coeff=intercoeff[1,1]
	mat drop intercoeff B_s
	qui gen double `varlist' = .

	mata: mixl_lsum("`b'", "`varlist'", "`touse'")

	** Restore sort order **
	sort `sorder'	
end

version 9.2
mata: 
void mixl_lsum(string scalar B_s, string scalar P_s, string scalar TOUSE_s)
{
	external mixl_X
	external mixl_T
	external mixl_CSID
	external mixl_ITER
	external mixl_np
	
	np = mixl_np
	command = st_local("cmd")
	nrep = strtoreal(st_local("nrep"))
	totobs = strtoreal(st_local("totobs"))
	kfix = st_numscalar("e(kfix)")
	krnd = st_numscalar("e(krnd)")
	krln = st_numscalar("e(krln)")
	burn = strtoreal(st_local("burn"))
	corr = st_numscalar("e(corr)")
	user = st_numscalar("e(userdraws)")
	INT_COEFF=st_numscalar("int_coeff")

	B = st_matrix(B_s)'

	kall = kfix + krnd
	MEAN_BPRICE=B[kall,1]
	
	
	if (kfix > 0) {
		MFIX = B[|1,1\kfix,1|]
		MFIX = MFIX :* J(kfix,nrep,1)	
	}

	MRND = B[|(kfix+1),1\kall,1|]
	
	if (corr == 1) {
		ncho = st_numscalar("e(k_aux)")
		SRND = invvech(B[|(kall+1),1\(kall+ncho),1|]) :* lowertriangle(J(krnd,krnd,1))
	}
	else {
		SRND = diag(B[|(kall+1),1\(kfix+2*krnd),1|])
	}

	
	P = J(totobs,1,0)
	if (krln > 0)  {
			MEAN_BPRICE=exp(MEAN_BPRICE)
			INT_COEFF=exp(INT_COEFF)
	}
	i = 1
//	n = 1
	for (n=1; n<=np; n++) {
		
/*		if (user == 1) { */
/*			ERR = invnormal(mixl_USERDRAWS[.,(1+nrep*(n-1))..(nrep*n)]) */
/*		} */
/*		else { */
		ERR = invnormal(halton(nrep,krnd,(1+burn+nrep*(n-1)))')
/*		} */
	
		if (kfix > 0) BETA = MFIX \ (MRND :+ (SRND*ERR))
		else BETA = MRND :+ (SRND*ERR)
		if (krln > 0) {
			if ((kall-krln) > 0) { 
				BETA = BETA[|1,1\(kall-krln),nrep|]\exp(BETA[|(kall-krln+1),1\kall,nrep|])
			}
			else {
				BETA = exp(BETA)
			}
		}
		
		t = 1
		nc = mixl_T[i,1]
		
		for (t=1; t<=nc; t++) {
			XMAT = mixl_X[|i,1\(i+mixl_CSID[i,1]-1),cols(mixl_X)|]
			ITER_MAT=mixl_ITER[|i,1\(i+mixl_CSID[i,1]-1),cols(mixl_ITER)|]
			
			PRICE_SIGMA=BETA[rows(BETA),.]
			
			MARGEFF_INC=J(rows(ITER_MAT),1,1)*(MEAN_BPRICE:+PRICE_SIGMA):+(ITER_MAT*INT_COEFF)
			DENOM=MARGEFF_INC
			
			R=J(rows(XMAT),1,1)*colsum(exp(XMAT*BETA))
			R2=abs(ln(R):/DENOM)
						
			P[|i,1\(i+mixl_CSID[i,1]-1),1|] = mean(R2',1)'
			i = i + mixl_CSID[i,1]
		}
	}
	st_store(.,P_s,TOUSE_s,P)	
}
end	

exit

