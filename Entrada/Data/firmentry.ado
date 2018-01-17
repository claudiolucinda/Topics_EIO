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

