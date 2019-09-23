
#FIXED PRICES
1. 
foreach file in "c:/temp/dat2010_banana_green" "c:/temp/dat2010_banana_ripe" "c:/temp/dat2010_beef" "c:/temp/dat2010_beer" "c:/temp/dat2010_bread" "c:/temp/dat2010_brews" "c:/temp/dat2010_bunscakes"  "c:/temp/dat2010_cassava_flour" "c:/temp/dat2010_cassava_fresh" "c:/temp/dat2010_charcoal" "c:/temp/dat2010_chicken" "c:/temp/dat2010_citrus" "c:/temp/dat2010_coconut" "c:/temp/dat2010_cooking_oil" "c:/temp/dat2010_dried_canned_fish" "c:/temp/dat2010_eggs" "c:/temp/dat2010_electricity" "c:/temp/dat2010_fish_seafood" "c:/temp/dat2010_fresh_milk" "c:/temp/dat2010_goat" "c:/temp/dat2010_greens" "c:/temp/dat2010_kerosene" "c:/temp/dat2010_maize_flour" "c:/temp/dat2010_maize_grain" "c:/temp/dat2010_maize_green" "c:/temp/dat2010_mangoes" "c:/temp/dat2010_millet_flour" "c:/temp/dat2010_millet_grain" "c:/temp/dat2010_onion" "c:/temp/dat2010_pasta" "c:/temp/dat2010_peanuts" "c:/temp/dat2010_petrol" "c:/temp/dat2010_pork" "c:/temp/dat2010_potatoes" "c:/temp/dat2010_pulses" "c:/temp/dat2010_rice_husked" "c:/temp/dat2010_salt" "c:/temp/dat2010_sugar" "c:/temp/dat2010_sugarcane" "c:/temp/dat2010_sweet_potato" "c:/temp/dat2010_tea" "c:/temp/dat2010_wheat" "c:/temp/dat2010_yam" {

use "`file'.dta", clear
display "use `file'.dta, clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_green price_maize_flour price_pulses price_rice_husked price_sweet_potato price_potatoes price_yam price_cooking_oil price_banana_green price_banana_ripe price_cassava_fresh price_citrus price_greens price_peanuts price_coconut price_mangoes price_salt price_gas price_kerosene price_brews
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}

2.
foreach file in "c:/temp/dat2010_dried_canned_veg" "c:/temp/dat2010_othervegstarch" {
use "`file'.dta", clear
display "use `file'.dta, clear"

quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_flour price_pulses price_rice_husked price_potatoes price_canned_milk price_salt price_sweet_potato price_cooking_oil price_banana_ripe price_greens  price_salt price_gas price_brews
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}


3. 
foreach file in "c:/temp/dat2010_canned_milk" "c:/temp/dat2010_gas" "c:/temp/dat2010_rice_paddy" {
use "`file'.dta", clear
display "use `file'.dta, clear"

quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_flour price_pulses price_rice_husked price_potatoes price_canned_milk price_salt price_sweet_potato price_cooking_oil price_banana_ripe price_greens  price_salt price_gas
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}

4. 

foreach file in "c:/temp/dat2010_winespirits" {
use "`file'", clear
display "use `file', clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk  price_cassava_flour price_pulses price_rice_husked price_sweet_potato  price_banana_ripe  price_greens price_kerosene  price_brews 
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}

================<<START<<2012<<
foreach file in "c:/temp/dat2012_banana_green" "c:/temp/dat2012_banana_ripe" "c:/temp/dat2012_beef" "c:/temp/dat2012_beer" "c:/temp/dat2012_bread" "c:/temp/dat2012_bunscakes"  "c:/temp/dat2012_cassava_flour" "c:/temp/dat2012_cassava_fresh" "c:/temp/dat2012_charcoal" "c:/temp/dat2012_chicken" "c:/temp/dat2012_citrus" "c:/temp/dat2012_coconut" "c:/temp/dat2012_cooking_oil" "c:/temp/dat2012_dried_canned_fish" "c:/temp/dat2012_eggs" "c:/temp/dat2012_electricity" "c:/temp/dat2012_fish_seafood" "c:/temp/dat2012_fresh_milk" "c:/temp/dat2012_goat" "c:/temp/dat2012_greens" "c:/temp/dat2012_kerosene" "c:/temp/dat2012_maize_flour" "c:/temp/dat2012_maize_grain" "c:/temp/dat2012_maize_green" "c:/temp/dat2012_mangoes" "c:/temp/dat2012_millet_flour"  "c:/temp/dat2012_onion" "c:/temp/dat2012_pasta" "c:/temp/dat2012_peanuts" "c:/temp/dat2012_petrol" "c:/temp/dat2012_pork" "c:/temp/dat2012_potatoes" "c:/temp/dat2012_pulses" "c:/temp/dat2012_rice_husked"  "c:/temp/dat2012_sugar" "c:/temp/dat2012_sugarcane" "c:/temp/dat2012_sweet_potato" "c:/temp/dat2012_tea" "c:/temp/dat2012_wheat" "c:/temp/dat2012_yam" {

use "`file'.dta", clear
display "use `file'.dta, clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_green price_maize_flour price_pulses price_rice_husked price_sweet_potato price_potatoes price_yam price_cooking_oil price_banana_green price_banana_ripe price_cassava_fresh price_citrus price_greens price_peanuts price_coconut price_mangoes price_salt price_gas price_kerosene price_brews
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}


2. 
foreach file in "c:/temp/dat2012_canned_milk" "c:/temp/dat2012_gas" "c:/temp/dat2012_rice_paddy" "c:/temp/dat2012_dried_canned_veg" "c:/temp/dat2012_brews" "c:/temp/dat2012_millet_grain" "c:/temp/dat2012_salt" {
use "`file'.dta", clear
display "use `file'.dta, clear"

quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_flour price_pulses price_rice_husked price_potatoes price_canned_milk price_salt price_sweet_potato price_cooking_oil price_banana_ripe price_greens  price_salt price_gas
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}

3. 

foreach file in "c:/temp/dat2012_winespirits"  {
use "`file'", clear
display "use `file', clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk  price_cassava_flour price_pulses price_rice_husked price_sweet_potato  price_banana_ripe  price_greens price_kerosene  price_brews 
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}


4.

foreach file in "c:/temp/dat2012_othervegstarch"  {
use "`file'", clear
display "use `file', clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk  price_brews 
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}



======================>>END>>2012==============


foreach file in   "c:/temp/dat2014_banana_green" "c:/temp/dat2014_banana_ripe" "c:/temp/dat2014_beef" "c:/temp/dat2014_beer" "c:/temp/dat2014_bread" "c:/temp/dat2014_bunscakes"  "c:/temp/dat2014_cassava_flour" "c:/temp/dat2014_cassava_fresh" "c:/temp/dat2014_charcoal" "c:/temp/dat2014_chicken" "c:/temp/dat2014_citrus" "c:/temp/dat2014_coconut" "c:/temp/dat2014_cooking_oil" "c:/temp/dat2014_dried_canned_fish" "c:/temp/dat2014_eggs" "c:/temp/dat2014_electricity" "c:/temp/dat2014_fish_seafood" "c:/temp/dat2014_fresh_milk" "c:/temp/dat2014_goat" "c:/temp/dat2014_greens" "c:/temp/dat2014_kerosene" "c:/temp/dat2014_maize_flour" "c:/temp/dat2014_maize_grain" "c:/temp/dat2014_maize_green" "c:/temp/dat2014_mangoes" "c:/temp/dat2014_millet_flour"  "c:/temp/dat2014_onion" "c:/temp/dat2014_pasta" "c:/temp/dat2014_peanuts" "c:/temp/dat2014_petrol" "c:/temp/dat2014_pork" "c:/temp/dat2014_potatoes" "c:/temp/dat2014_pulses" "c:/temp/dat2014_rice_husked"  "c:/temp/dat2014_sugar" "c:/temp/dat2014_sugarcane" "c:/temp/dat2014_sweet_potato" "c:/temp/dat2014_tea" "c:/temp/dat2014_wheat" "c:/temp/dat2014_yam" {

use "`file'.dta", clear
display "use `file'.dta, clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_flour price_pulses price_rice_husked price_potatoes price_canned_milk price_salt price_sweet_potato price_cooking_oil price_banana_ripe price_greens  

predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}
 

foreach file in  "c:/temp/dat2014_gas"   "c:/temp/dat2014_brews"  {
use "`file'.dta", clear
display "use `file'.dta, clear"

quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk price_bread price_cassava_flour price_maize_flour price_pulses price_rice_husked price_potatoes price_canned_milk price_sweet_potato price_cooking_oil price_banana_ripe price_greens  price_salt 
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}


foreach file in "c:/temp/dat2014_winespirits"  "c:/temp/dat2014_canned_milk" "c:/temp/dat2014_millet_grain"  "c:/temp/dat2014_salt"   {
use "`file'", clear
display "use `file', clear"
quietly probit hasex    i.expensiveregion price_beef price_chicken price_fresh_milk    price_sweet_potato  price_banana_ripe  price_greens price_kerosene  price_brews
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}


foreach file in "c:/temp/dat2014_dried_canned_veg" "c:/temp/dat2014_othervegstarch"  {
use "`file'", clear
display "use `file', clear"
quietly probit hasex hsize consu highest_educ age i.expensiveregion price_beef price_chicken price_fresh_milk   
predict phat, xb
gen mills = exp(-.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
gen millsn = exp(-.5*phat*phat)/(sqrt(2*_pi)*(1-normprob(phat)))
save "`file'_mills.dta"

}

** "c:/temp/dat2014_rice_paddy" - not found

=================>> END 2014 >>=========