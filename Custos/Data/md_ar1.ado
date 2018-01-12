
/* 
 
Mans Soderbom, January 2001

*/


program define md_ar1
	version 6.0
	set mat 800



syntax, [nx(real 1) beta(string) cov(string)]

mat def bpi=`beta''
mat def vbpi=`cov'

mat bpi=bpi[1..(2*`nx'+1),1]
mat vbpi=vbpi[1..(2*`nx'+1),1..(2*`nx'+1)]

local j=1
while `j'<=`nx' {

if `j'==1 {
mat def h=J(2,1,1)
mat def g=bpi[1,1]\(-bpi[2,1]/bpi[2*`nx'+1,1])
}

else if `j'>=1 {
mat def h=(h, J(2*(`j'-1),1,0))\( J(2,`j'-1,0), J(2,1,1))
mat def g=g\bpi[2*(`j'-1)+1,1]\-1*bpi[2*(`j'-1)+2,1]/bpi[2*`nx'+1,1]
}

local j=`j'+1
}

mat def h=(h, J(2*`nx',1,0))\( J(1,`nx',0), J(1,1,1))
mat def g=g\ bpi[2*`nx'+1,1]

if `nx'==1 {
mat def gg=( J(1,1,1),J(1,2,0) )\(  J(1,1,0),(-1/bpi[3,1]), /* 
*/ (bpi[2,1]/(bpi[3,1]^2))  )
}


else if `nx'>=1 {		/* big loop starts */

local j=1			
while `j'<=`nx'-1 {

if `j'==1 {
mat def gg=(J(1,1,1),J(1,2*`nx',0))\(J(1,1,0),(-1/bpi[2*`nx'+1,1]), /*
*/ J(1,2*(`nx'-`j'),0),(bpi[2,1]/(bpi[2*`nx'+1,1]^2)))
}

else if `j'>=1 {
mat def gg=gg\(  J(1,2*(`j'-1),0),J(1,1,1),J(1,2*(`nx'-`j'+1),0)  )\  /*
*/ (  J(1,2*(`j'-1)+1,0),(-1/bpi[2*`nx'+1,1]),J(1,2*(`nx'-`j'),0),  /*
*/ (bpi[2*`j',1]/(bpi[2*`nx'+1,1]^2)) )

}

local j=`j'+1

}

mat def gg=gg\( J(1,2*(`nx'-1),0),J(1,1,1),J(1,2,0) ) \ /*
*/ ( J(1,2*(`nx'-1)+1,0), (-1/bpi[2*`nx'+1,1]),  	  /*
*/ (bpi[2*`nx',1]/(bpi[2*`nx'+1,1]^2)) )

}				/* big loop ends */

mat def gg=gg\( J(1,2*`nx',0),J(1,1,1) )

mat omega=gg*vbpi*gg'
mat iomega=inv(omega)
mat aa=inv(h'*iomega*h)
mat theta=aa*(h'*iomega*g)
mat covjj=(vecdiag(aa))
mat covjj=covjj'

local j=1
while `j'<=`nx'+1 {

if `j'==1 {
mat stderr=sqrt(covjj[1,1])
mat trat=abs(theta[1,1]/stderr[1,1])
mat pval=2*(1-normprob(abs(trat[1,1])))
mat hname=bpi[1,1..1]
}
else if `j'>=1 {
mat stderr=stderr\sqrt(covjj[`j',1])
mat trat=trat\abs(theta[`j',1]/stderr[`j',1])
mat pval=pval\(2*(1-normprob(abs(trat[`j',1]))))
mat hname=hname\bpi[2*(`j'-1)+1..2*(`j'-1)+1,1]
}

local j=`j'+1
}

mat chisq=(g-h*theta)'*iomega*(g-h*theta)
local pcomfac=chiprob(`nx',chisq[1,1])

mat all=theta,stderr,trat,pval


mat colnames all = Coef Std t-value Prob
local names: rowname hname
mat rownames all =`names'

mat list all
di in gr " "
di in gr "Prob[COMFAC]: " in ye %8.5f `pcomfac'
	end


