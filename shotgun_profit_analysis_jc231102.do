* GAIA shotgun trials year 1

set more off
cd "C:\DATA\GAIA\shotgun"
ls 


* prepare stata file from raw data
import delimited "gaia-trials-year1.csv"

foreach v in country zone district kebele trial_type crop {
    rename `v' `v'name
    encode `v'name, gen(`v')
    order `v', before(`v'name)
}

save "gaia-trials-year1.dta", replace


codebook crop
/* 
 673         1  Beans
 101         2  Fababean
 878         3  Maize
 160         4  Soybean
 140         5  Wheat
*/

* analysis
reg yield_tha c.lime_tha##c.lime_tha i.district if crop==3


*reg yield_tha c.lime_tha##c.lime_tha##c.lime_tha ph soc clay tn m3al m3b m3ca m3fe m3k m3s m3mg m3mn m3na i.district if crop==3
reg yield_tha c.lime_tha##c.lime_tha ph soc clay tn m3al m3b m3ca m3fe m3k m3s m3mg m3mn m3na i.district if crop==3
margins, predict() at(lime=(0(1)7))
marginsplot

cap drop yhat 
predict yhat
tw scatter yhat lime

/* no, this is wrong - the net rev should be calclated relative to what the yields would be at zero lime 
local p_y = 100
local p_x = 70
cap drop netrev
gen netrev = (yield_tha*`p_y') + (lime_tha*`p_x')

reg netrev c.lime_tha##c.lime_tha ph soc clay tn m3al m3b m3ca m3fe m3k m3s m3mg m3mn m3na i.district if crop==3
margins, predict() at(lime=(0(1)7))
marginsplot

cap drop yhat 
predict yhat
tw scatter yhat lime
*/