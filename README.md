# Project Summary  
This project created information for the Oregon Department of Transportation (ODOT) Transportation System Action Plan (TSAP) which was sumamrized in a technical memo titled Pedestrain Injury and Social Equity in Oregon.  The technical memo can be accessed at the link below.  
https://www.oregon.gov/odot/Safety/Documents/Pedestrian_Safety_and_Social_Equity.pdf  

The data and scripts in this repository gather data from Fatal Accident Reporting System (FARS) and Census data and then calculates a age-adjusted 
population-based pedestrian death rates for each represented racial group to assess disparities.  These rates use the US population as the standard population and employ five years population data using 
person-years as the denominator.  The analysis script also calculates population-based injury rates for other modes including bicycle and motor vehicle.

# Script Details  
Two scripts are available in this repository.  The script Analyze_FARS_Race_prod.r combines the FARS data and Census population data to analyze the rate of injury per 100,000 person-years.  The script can be changed to analyze
any of the 50 state by toggling a few inputs (see in-line comments).  This script calculates confidence intervals so users can communicate the uncertainty in the measured outcomes.  This script also aggregates  Black, Indigenous, 
and People of Color race categories into a single group in order to measure the injury rate with more certainty than what is possible with disaggregate BIPOC groups (at least for Oregon).  

## Analyze_FARS_Race_prod.r
This script combines FARS person level fatal death data with Census population data to calculate age-adjusted population-based fatal injury rates by racial category.  The analysis uses the US populationa as the standard population 
to weight the rates by age cohort in order to make the composite rates by race comparable across the US.  A composite BIPOC rate is constructed to improve confidence in the point estimates for these non-White racial categories since 
some disaggegate BIPOC groups have small numbers of either population, injuries, or both.  


## download_prepare_census_population_data.r
This script uses availablr R packages to automatically access state level Census population data for all states.  These data include the population by age, gener and race which are necessary to calculate reliable
age-adjusted traffic injury death rates.  You will need to sign up for a free Census API key.  

# Contact
Principle Investigator: Josh Roll  josh.f.roll@odot.state.or.us  
