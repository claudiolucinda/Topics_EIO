cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Entrada\Data\"


import excel "TireDlrs.xls", sheet("Tire Data") firstrow clear

cap program drop BReiss
*set trace on
BReiss tire, variable(old pinc lnhdd ffarm) mktsize(opop ngrw pgrw octy) fixedcst(acre) mktsizeoff(tpop)

cap program drop BReiss_pred
set trace on
BReiss_pred , variable(old pinc lnhdd ffarm) mktsize(opop ngrw pgrw octy) fixedcst(acre) mktsizeoff(tpop)
