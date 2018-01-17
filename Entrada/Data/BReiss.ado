
cap program drop BReiss

program BReiss
	version 12
	*args var fig
	syntax varlist(max=1) [if] [in] , VARIABLE(varlist) MKTSIZE(varlist) FIXEDCST(varlist) MKTSIZEOFF(varlist) [PROBESTS]
	constraint 1 [alpha4]_cons=0

	ml model lf firmentry (lambda:`varlist'=`mktsize',nocons offset(`mktsizeoff')) ///
	(beta:`varlist'=`variable') (gammaL:`varlist'=`fixedcst') (alpha2:`varlist'=) ///
	(alpha3:`varlist'=) (alpha4:`varlist'=) (alpha5:`varlist'=) (gamma2:`varlist'=) (gamma3:`varlist'=) ///
	(gamma4:`varlist'=) (gamma5:`varlist'=), constraint(1)

	ml search lambda 0 50 beta 0 1 alpha2 0 1 alpha3 0 1 alpha4 0 1 alpha5 0 1 gamma2 0 1 gamma3 0 1 gamma4 0 1 gamma5 0 1 gammaL -1 1

	ml maximize, difficult gradient

end



