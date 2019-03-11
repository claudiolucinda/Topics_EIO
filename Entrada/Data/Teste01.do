cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Entrada\Data\"

import excel "TireDlrs.xls", sheet("Tire Data") firstrow clear

drop if tire>1

cap program drop monentry
program monentry
version 1.0
args lnf s v f
quietly replace `lnf'=ln(normal(`s'*`v'-`f')) if $ML_y1==1
quietly replace `lnf'=ln(1-normal(`s'*`v'-`f')) if $ML_y1==0
end

ml model lf monentry (lambda:tire=opop ngrw pgrw octy,nocons offset(tpop)) ///
(beta:tire=old pinc lnhdd ffarm) (gammaL:tire=acre) 

ml search lambda -1 1 beta 0 1 gammaL -1 1

ml max

import excel "TireDlrs.xls", sheet("Tire Data") firstrow clear

cap program drop firmentry
program firmentry
version 1.0
args lnf s v f alpha2 alpha3 alpha4 alpha5 gamma2 gamma3 gamma4 gamma5
tempvar p2 p3 p4 p5
qui gen double `p2'=normal(`s'*(`v'-`alpha2')-`f'-`gamma2')
qui gen double `p3'=normal(`s'*(`v'-`alpha2'-`alpha3')-`f'-`gamma2'-`gamma3')
qui gen double `p4'=normal(`s'*(`v'-`alpha2'-`alpha3'-`alpha4')-`f'-`gamma2'-`gamma3'-`gamma4')
qui gen double `p5'=normal(`s'*(`v'-`alpha2'-`alpha3'-`alpha4'-`alpha5')-`f'-`gamma2'-`gamma3' -`gamma4'-`gamma5')
quietly replace `lnf'=ln(1-normal(`s'*(`v')-`f')) if $ML_y1==0
quietly replace `lnf'=ln(normal(`s'*(`v')-`f')-`p2') if $ML_y1==1
quietly replace `lnf'=ln(`p2'-`p3') if $ML_y1==2
quietly replace `lnf'=ln(`p3'-`p4') if $ML_y1==3
quietly replace `lnf'=ln(`p4'-`p5') if $ML_y1==4
quietly replace `lnf'=ln(`p5') if $ML_y1>=5
end

constraint 1 [alpha4]_cons=0

ml model lf firmentry (lambda:tire=opop ngrw pgrw octy,nocons offset(tpop)) ///
(beta:tire=old pinc lnhdd ffarm) (gammaL:tire=acre) (alpha2:tire=) ///
(alpha3:tire=) (alpha4:tire=) (alpha5:tire=) (gamma2:tire=) (gamma3:tire=) ///
(gamma4:tire=) (gamma5:tire=), constraint(1)

ml search lambda 0 50 beta 0 1 alpha2 0 1 alpha3 0 1 alpha4 0 1 alpha5 0 1 gamma2 0 1 gamma3 0 1 gamma4 0 1 gamma5 0 1 gammaL -1 1

ml max
