# Project Summary  
This project created information for the Oregon Department of Transportation (ODOT) Transportation System Action Plan (TSAP) which was sumamrized in a technical memo titled Pedestrain Injury and Social Equity in Oregon.  The technical memo can be accessed at the link below.  
https://www.oregon.gov/odot/Safety/Documents/Pedestrian_Safety_and_Social_Equity.pdf  

The data and scripts in this project gather data from Fatal Accident Reporting System (FARS) and Census data and then calculates a age-adjusted 
population-based pedestrian death rates for each represented racial group to assess disparities.  These rates use the US population as the standard population and employ five years population data using 
person-years as the denominator.  The analysis script also calculates population-based injury rate for other modes including bicycle, motor vehicle.

# Script Details  
Two scripts are available in this repository.  The script Analyze_FARS_Race_prod.r combines the FARS data and Census population data to analysze the rate of injury per 100,000 person-years.  The script can be changed to analyze
any of the 50 state by toggling a few inputs.  This script calculates confidence intervals so users can communicate the certainty in the measured outcomes.  This script also aggregates all Black, Indigenous, 
and People of Color race categories into a single group in order to measure the injury rate with more certainty than what is possible with disaggregate BIPOC groups.  

## Analyze_FARS_Race_prod.r
This script uses previosuly compiled data to calculate z-scores using percent of the Census tract population that is Black, Indigenous, and Persons of Color (BIPOC) and the percent of the population living in poverty.
Pedestrain injury rates (per 100,000 people) are calculated along with some travel and built environmental summaries.  Charts are generated to summarise pedestrain injury rates by index category for two injury severities 
including fatal and severe injuries and total pedestrain injuries.  Two periods of data are available including years from 2008 to 2012 and from 2014 to 2018.  REII is calculated for both periods for comparison.  Another item in  
the script summarizes measures for injuries, population, and travel and built environmental measures are created and charted. Lasly, dynamic maps are created that utilize the leaflet package to see how the REII appears 
spatially and also to explore the other data elements.  

## download_prepare_census_population_data.r
This script uses some R packages to automatically access state level Census population data for all states.  These data include the population by age, gener and race which are necessary to calculate reliable
age-adjusted traffic injury death rates.  You will need to sign up for a free Census API key.  

# Data


# Contact
Principle Investigator: Josh Roll  josh.f.roll@odot.state.or.us  
