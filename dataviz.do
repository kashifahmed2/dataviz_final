/*******************************************************************************
Author: Kashif Ahmed
Purpose: data viz final
Date Created: March 12, 2022
Worked in Stata 17, Mac
*******************************************************************************/

clear all


global dirpath = "/Users/kashifahmed/Documents/GitHub/dataviz_final/data"

cd "$dirpath"

import delimited "worldbankdata.csv", varnames(1) 

drop seriescode

gen id = _n

order id

reshape long yr, i(id) j(year)

rename seriesname variable

rename yr value

drop id

export delimited using "/Users/kashifahmed/Documents/GitHub/dataviz_final/data/worldbank_clean.csv", replace




