****************************************************
* Calculador de Matriz de Elasticidades
* Cláudio R. Lucinda
****************************************************

su price if cdid==$nro_mkt

local nobs=r(N)

matrix elasts=J(`nobs',`nobs',.)

preserve
keep if cdid==$nro_mkt
local bigobs=_N
forvalues i=1/`bigobs' {
	local k=`i'+1
	forvalues j=`k'/`bigobs' {
		mat elasts[`i',`j']=-_b[price]*share[`j']*price[`j']
		mat elasts[`j',`i']=-_b[price]*share[`i']*price[`i']
	}
}
forvalues i=1/`bigobs' {
	mat elasts[`i',`i']=_b[price]*(1-share[`i'])*price[`i']
}
restore
