*************************************************
* Código para estimação dos coeficientes
* Modelo Logit com dados agregados
* Claudio R. Lucinda
*************************************************

clear all
set seed 1992
discard
set matsize 400
*cd "C:\Users\Caio\Dropbox\BLP-Stata_Paper\BLp"
cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\"
adopath + "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Demanda\Data\"

fdause NevoData_OI.xpt, clear
rename v1 share
rename v2 price
rename v27 constant
*rename v28 price
rename v29 sugar
rename v30 mushy
rename v75 cdid
rename v76 id
su share price sugar mushy 
order cdid id share price constant sugar mushy

bysort cdid: egen inshare=total(share)
gen outshare=1-inshare

gen meanu=ln(share)-ln(outshare)

* Estimando por OLS: Basicão

reg meanu price sugar mushy

global nro_mkt=1
run "Codigo_03_1.do"
mat rename elasts elasts_OLS_1
mat li elasts_OLS_1

* OLS + EF região

xi: reg meanu price sugar mushy i.cdid
run "Codigo_03_1.do"
mat rename elasts elasts_OLS_2
mat li elasts_OLS_2

* OLS + EF marca

reg meanu price sugar mushy v3-v26
run "Codigo_03_1.do"
mat rename elasts elasts_OLS_3
mat li elasts_OLS_3

* IV

ivreg2 meanu sugar mushy (price=v31-v74), gmm2s robust first
run "Codigo_03_1.do"
mat rename elasts elasts_IV_1
mat li elasts_IV_1

* IV + EF região

xi: ivreg2 meanu sugar mushy i.cdid (price=v31-v74), gmm2s robust first 
run "Codigo_03_1.do"
mat rename elasts elasts_IV_2
mat li elasts_IV_2


ivreg2 meanu sugar mushy v3-v26 (price=v31-v50), gmm2s robust first
run "Codigo_03_1.do"
mat rename elasts elasts_IV_3
mat li elasts_IV_3

* BLP 
unab lista: v31-v74
*display "`lista'"
blp share sugar mushy v3-v26, stochastic(price) endog(price=`lista') markets(cdid) initsd(.25) elast(price,1) robust
mat li e(elast)		 
