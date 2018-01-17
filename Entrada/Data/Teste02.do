cd "C:\Users\ClaudioLucinda\Dropbox\Consultoria - OLD\NET-2016\"


import excel "TireDlrs.xls", sheet("Tire Data") firstrow clear

cap program drop BReiss
*set trace on
BReiss tire, variable(old pinc lnhdd ffarm) mktsize(opop ngrw pgrw octy) fixedcst(acre) mktsizeoff(tpop)

cap program drop BReiss_pred
set trace on
BReiss_pred , variable(old pinc lnhdd ffarm) mktsize(opop ngrw pgrw octy) fixedcst(acre) mktsizeoff(tpop)
