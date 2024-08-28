********************
version 17.0
clear all
cd "$directory"
********************

* Use the conciliator lvl database
import delimited "01_Data/02_Created/base_with_fake_drops.csv", clear
tempfile conciliator
save `conciliator'

replace false_drop_time_25_dropped = "" if false_drop_time_25_dropped == "NA"
destring false_drop_time_25_dropped, replace

* Define variables
 // inmediata corporativos solicitud_trabajador
local dep_vars false_drop_time_25_dropped


* Define locals for formatting
local labsize medlarge
local bigger_labsize large
local ylabel_options nogrid notick labsize(`labsize') angle(horizontal)
local xlabel_options nogrid notick labsize(`labsize')
local xtitle_options size(`labsize') margin(top)
local title_options size(`bigger_labsize') margin(bottom) color(black)
local manual_axis lwidth(thin) lcolor(black) lpattern(solid)
local plotregion plotregion(margin(sides) fcolor(white) lstyle(none) lcolor(white)) 
local graphregion graphregion(fcolor(white) lstyle(none) lcolor(white)) 
local T_line_options lwidth(thin) lcolor(gray) lpattern(dash)
// To show significance: hollow gray (gs7) will be insignificant from 0,
//  filled-in gray significant at 10%
//  filled-in black significant at 5%
local estimate_options_0  mcolor(gs7)   msymbol(Oh) msize(medsmall)
local estimate_options_90 mcolor(gs7)   msymbol(O)  msize(medsmall)
local estimate_options_95 mcolor(black) msymbol(O)  msize(medsmall)
local rcap_options_0  lcolor(gs7)   lwidth(thin)
local rcap_options_90 lcolor(gs7)   lwidth(thin)
local rcap_options_95 lcolor(black) lwidth(thin)


* Encode the conciliators
encode conciliador, gen(conciliador_id)



* Get the number of conciliators
qui tab conciliador_id
local num_conc = `r(r)'
di `num_conc'

* Identify the ommitted conciliator (the one with the most cases)
su conciliador_id if conciliador == "RODOLFO BRIBIESCA GARCÍA"
if `r(sd)' == 0 {
	local omitted = int(`r(mean)')
}

*different dep_vars using the outcome of the first hearing as outcome 



** Generate figures	
foreach dep_var in `dep_vars' {

	** Standardization
	su `dep_var'
	gen std_`dep_var' = (`dep_var'-`r(mean)')/`r(sd)'
	
	** wrt omitted
	reghdfe std_`dep_var' ib`omitted'.conciliador_id, noabsorb vce(robust) // vce(cluster conciliador_id)
	
	local df = e(df_r)
	levelsof conciliador_id if e(sample), local(levels) 
	
	matrix results = J(`num_conc', 4, .) /* empty matrix for results
	   4 cols are: (1) office, (2) beta, (3) std error, (4) pvalue */

	
	foreach row of local levels {
		matrix results[`row', 1] = `row'
		** Omited office
		if `row'!=`omitted' { 
		** Beta 
		matrix results[`row', 2] = (_b[`row'.conciliador_id] )
		** Standard error
		matrix results[`row', 3] = _se[`row'.conciliador_id]
		** P-value
		matrix results[`row', 4] = 2*ttail(`df', abs(_b[`row'.conciliador_id]/_se[`row'.conciliador_id]))
			}
	}
	
	matrix colnames results = "k" "beta" "se" "p"
	matlist results
	
	preserve
		clear
		svmat results, names(col) 
		drop if missing(k)

		** Confidence intervals (95%) (wrt the proportion of the mean)
		local alpha = .05 /* for 95% confidence intervals */
		gen rcap_lo = beta - (invttail(`df', `=`alpha'/2')*se )
		gen rcap_hi = beta + (invttail(`df', `=`alpha'/2')*se )

		** Omitted conciliator
		replace beta = 0 if missing(beta)
		replace se = 0 if missing(se)
		sort beta 
		gen orden = _n
		qui su orden if k == `omitted'
		local omitted_orden = `r(mean)'

		** GRAPH
		#delimit ;
		graph twoway 
			(scatter beta orden if p < 0.05,           `estimate_options_95') 
			(scatter beta orden if p >= 0.05 & p < 0.10, `estimate_options_90') 
			(scatter beta orden if p >= 0.10,          `estimate_options_0' ) 
			(rcap rcap_hi rcap_lo orden if p < 0.05,           `rcap_options_95')
			(rcap rcap_hi rcap_lo orden if p >= 0.05 & p < 0.10, `rcap_options_90')
			(rcap rcap_hi rcap_lo orden if p >= 0.10,          `rcap_options_0' )		
			, 
			title(" ", `title_options')
			ylabel(, `ylabel_options') 
			yline(0, `manual_axis')
			xtitle("Conciliator", `xtitle_options')
			xscale(range(`min_xaxis' `max_xaxis'))
			xline(`omitted_orden', `T_line_options')
			xscale(noline) /* because manual axis at 0 with yline above) */
			`plotregion' `graphregion'
			legend(off)  
		;
		
		#delimit cr
		graph export "04_Figures/fe_`dep_var'.pdf", replace
		
		** Save estimation
		rename k conciliador_id
		foreach vare of varlist beta se {
			rename `vare'  `vare'_`dep_var'
		}
		keep conciliador_id beta se
		tempfile temp_e
		save `temp_e'
	
	restore
	
	** Paste estimation
	merge m:1 conciliador_id using `temp_e', nogen
}

