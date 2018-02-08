**********************************************
* Teste Parte 2 - Rodando o gmm
**********************************************

clear 

*cd "C:\Users\ClaudioLucinda\Documents\Aulas\GV\Curso de OI - PÃ³s\Custos\ACF-Lucinda\"

cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - PÃ³s\Mini Curso USP\Topics_EIO\Custos\Data\"

adopath + "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - PÃ³s\Mini Curso USP\Topics_EIO\Custos\Data\"

use sample.dta, clear
cap program drop ACF_proc
gen newid=plantid
xtset newid year

*set trace on
reg lnva lnk lnw lnb, robust cluster(newid)
est store mod01

xtreg lnva lnk lnw lnb, fe robust cluster(newid)
est store mod02

* Colocando EF de ano
xi: reg lnva lnk lnw lnb i.year, robust cluster(newid)
xi: xtreg lnva lnk lnw lnb i.year, fe robust cluster(newid)

* Painel Balanceado
tempvar q
bysort newid: gen `q' = _n
su `q'
local maxn=r(max)
gen byte balanced = 0
replace balanced = 1 if `q'==`maxn'
bysort newid: egen balanced2=sum(balanced)
drop balanced
rename balanced2 balanced

* Pra ver o efeito da seleção
bysort balanced: su lnk lnw lnb

*set trace on
reg lnva lnk lnw lnb if balanced, robust cluster(newid)
est store mod03

xtreg lnva lnk lnw lnb if balanced, fe robust cluster(newid)
est store mod04

* Colocando EF de ano
xi: reg lnva lnk lnw lnb i.year if balanced, robust cluster(newid)
xi: xtreg lnva lnk lnw lnb i.year if balanced, fe robust cluster(newid)


sort newid year
* Estimando por Painéis Dinâmicos - GMM-Diff
* 2 lags
xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year, gmm(lnva lnk lnw lnb, laglim(2 .)) iv(i.year) noleveleq robust
md_ar1, nx(3) beta(e(b)) cov(e(V))

xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year if balanced, gmm(lnva lnk lnw lnb, laglim(2 .)) iv(i.year) noleveleq robust
md_ar1, nx(3) beta(e(b)) cov(e(V))


* 3 lags
xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year, gmm(lnva lnk lnw lnb, laglim(3 .)) iv(i.year) noleveleq robust
md_ar1, nx(3) beta(e(b)) cov(e(V))

* GMM-Sys
xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year, gmm(lnva lnk lnw lnb, laglim(2 .)) iv(i.year, e(level)) robust h(1)
md_ar1, nx(3) beta(e(b)) cov(e(V))

* GMM-Sys
xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year if balanced, gmm(lnva lnk lnw lnb, laglim(2 .)) iv(i.year, e(level)) robust h(1)
md_ar1, nx(3) beta(e(b)) cov(e(V))


xi: xtabond2 lnva l(0/1).lnk l(0/1).lnw l(0/1).lnb l.lnva i.year, gmm(lnva lnk lnw lnb, laglim(3 .)) iv(i.year, e(level)) robust h(1)
md_ar1, nx(3) beta(e(b)) cov(e(V))


sort plantid year
* Calculando o número de anos cada planta tem
by plantid: gen count = _N
su count
local maxt=r(max)
* Marcando as que passaram todo o período
gen survivor = count == `maxt'

* Marcando as observações do último ano
gen has95 = 1 if year == 1991
sort plantid has95

by plantid: replace has95 = 1 if has95[_n-1] == 1
replace has95 = 0 if has95 == .

* Marcando se tem gaps no ano
sort plantid year
by plantid: gen has_gaps = 1 if year[_n-1] != year-1 & _n != 1


sort plantid has_gaps
by plantid: replace has_gaps = 1 if has_gaps[_n-1] == 1
replace has_gaps = 0 if has_gaps == .

sort plantid year
by plantid: generate exit = survivor == 0 & has95 == 0 & has_gaps != 1 & _n == _N
replace exit = 0 if exit == 1 & year == 1991
*gen saida=0
opreg lnva, state(lnk) proxy(lnm) free(lnw lnb) exit(exit)
est store mod05

xi: opreg lnva, state(lnk) proxy(lnm) free(lnw lnb) exit(exit) cvars(i.year)


levpet lnva, free(lnw lnb) proxy(lnm) capital(lnk) val
est store mod06

acfest lnva, free(lnw lnb) state(lnk) proxy(lnm) va robust overid nbs(50)
est store mod07

estimates table  mod*

mat initols=e(b)
timer on 1
*gmm ACF_proc, nequations(1) nparameters(3) winitial(identity) instruments(lnk l.lnw l.lnb) fatprod(lnk lnw lnb) intinput(lnm) depvar(lnva) onestep from(initols)

gmm ACF_proc, nequations(1) nparameters(3) winitial(identity) instruments(lnk l.lnw l.lnb) fatprod(lnk lnw lnb) intinput(lnm) depvar(lnva) vce(bootstrap, reps(2) cluster(plantid) idcluster(newid)) onestep from(initols)
timer off 1
timer list 1
