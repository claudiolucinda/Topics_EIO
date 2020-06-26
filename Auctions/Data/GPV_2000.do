******************************************************
* Problem Set Auctions
* Claudio Lucinda
* USP
* 2017
******************************************************

clear

cd "G:\Meu Drive\Aulas\GV\Curso de OI - Pós\Mini Curso USP\Topics_EIO\Auctions\"

insheet using ".\Data\PS3Data.csv", clear

reshape long bidder, i(auction) j(bid_code)

sort bidder


gen cdf=_n/_N

kdensity bidder, generate(dpoints ddens) at(bidder)

gen valuation=bidder+cdf/ddens


twoway scatter valuation bidder, xtitle("bid") ytitle("Valuation") title("Bids X Valuation")

twoway kdensity valuation, n(1000) || kdensity bidder, n(1000)

* Calculando o Optimal Reserve Price

gen check=valuation-(1-cdf)/ddens
