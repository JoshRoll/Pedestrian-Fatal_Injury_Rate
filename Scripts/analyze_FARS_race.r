#Author: Josh Roll 
#Date: 11/12/2020
#Subject:  Analyze FARS data to understand ped fatal injuries by RACE
#Description: This script uses data from US Census and NHTSA FARS to calculate the rate of injury per 100,000 using age-adjusted methods to determine existing disparities for 
#ODOT Research Project SPR 841
#License: Apache 2.0
#Version notes:
#Version 4 - Now calcualting rates using person years instead of single years

#Load libraries
#------------------------------
	#Establish library path
	library(lubridate)
	library(tigris)
	library(dplyr)
	library(readxl)
	library(rgdal)
	library(sf)
	library(stringr)
	library(leaflet)
	library(htmlwidgets)
	library(tidyr)
	library(ggplot2)
	library(MASS)
	library(mfx)
	library(pscl)
	library(corrplot)
	library(readxl)
	library(Metrics)
	library(epitools)
	library(data.table)
	library(acs)
	library(scales)
	library(gridExtra)
	


#Set environmanetal 
#-----------------
  options( scipen = 10 )

#Define custom scripts functions
#------------------------------
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }	
		
	#Function to create bin labels
	label_Bins <- function(Breaks.){
		Labels_ <- list()
		for(i in 1:(length(Breaks.)-1)){
			Labels_[[i]] <- paste(Breaks.[[i]], Breaks.[[i + 1]], sep="-")
		}
		#Return result
		unlist(Labels_)
	}

#Load Data
#---------------------------------------------
	########################
	#State Level population data by age/Race (5 year ACS)
	State_Census.. <- assignLoad(file = "/Data/State_Age_Sex_2009_2018_5yr.RData")
		
	#Load US FARS Person table
	#############################
	Load_Person.. <- assignLoad(file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Crash/Data/FARS/Data/Processed/Person_2009-2018.RData")
	#Prepare FARS data - Aggregate data to create a composite BIPOC race category
	#Create data set classifying participants as BIPOC
	#Make a copy
	Person_Bipoc.. <- Load_Person..
	#Mutate disaggregate BIPOC races into composite BIPOC category
	Person_Bipoc.. <- filter(Person_Bipoc.. %>% mutate(Race_desc = ifelse(Race_desc%in%c("AIAN","Combined Other Asian Or Pacific Islander, Includes Data","Black","Asian","NHPI","Latino","Guamanian","Hawaiian (Includes Part-Hawaiian)",
		"Other Indian (Includes South and Central America, Since"), "BIPOC",
		ifelse(Race_desc == "White", "White",
		ifelse(Race_desc%in%c("Other", "Not A Fatality (Not Applicable)","Unknown"), "Other or Unknown",NA)))), Race_desc =="BIPOC")
	#Combine
	Load_Person.. <- rbind(Load_Person.. , Person_Bipoc..)
	
	#Define state of interest
	############################
	#FARS Data
	FARS_Select_State <- 41
	#Census population data state
	Population_Select_State <- "Oregon"
	
	#init a dataframe to store results
	Master_Rate.. <- data.frame()
	#Loop through all periods of data
	for(yr in c(2013,2018)){
		#Define Study Period
		Study_Year <- yr
		#FARS_Year. <- 2014:2018
		FARS_Period. <- c((yr -4):yr)		
		#Make a copy 
		Person.. <- Load_Person..
		#Select State data that are fatals (interested in another state define above)
		Select_State_Person.. <- filter(Person.., State == FARS_Select_State & Inj_sev==4)
		US_Person.. <- filter(Person.., Inj_sev==4)
		#Prepare Oregon FARS
		################
		Oregon_Fatal_Summary.. <- filter(Select_State_Person.., Year%in%FARS_Period.) %>% group_by(Race_desc,Mode,Age_Cohort) %>% summarise(Count = length(St_case))
		#Look just at pedestrians
		Select_State_Fatal.. <- filter(Oregon_Fatal_Summary..)
		#Create statewide fora all populations
		Select_State_Fatal.. <- rbind(Select_State_Fatal..,cbind(Select_State_Fatal.. %>% group_by(Mode, Age_Cohort) %>% summarise(Count = sum(Count,na.rm=T)), Race_desc = "Oregon")[,colnames(Select_State_Fatal..)])
		#Prepare US FARS
		US_Fatal_Summary.. <- filter(US_Person.., Year%in%FARS_Period.) %>% group_by(Race_desc,Mode,Age_Cohort) %>% summarise(Count = length(St_case))
		US_Fatal.. <- filter(US_Fatal_Summary..)
		#Create nationwide fora all populations
		US_Fatal.. <- rbind(US_Fatal..,cbind(US_Fatal.. %>% group_by(Mode, Age_Cohort) %>% summarise(Count = sum(Count,na.rm=T)), Race_desc = "US")[,colnames(Select_State_Fatal..)])
		#Alter study year for population 
		if(yr%in%2013){Study_Year <- 2009:2013}
		if(yr%in%2018){Study_Year <- 2014:2018}
		
		#Summarise Oregon Population
		##################### 
		#Pivote data
		Population_Summary.. <-	filter(State_Census.., State == Population_Select_State & Year%in%Study_Year) %>%
			pivot_longer(!c(State,GEOID,NAME,Year), names_to = "Age_Cohort", values_to = "Population")
		#Create new column for race
		Population_Summary.. <- mutate(Population_Summary.., 
			#Age cohort
			Age_Cohort_Update = ifelse(grepl("Age_Under5", Age_Cohort, ignore.case = T), "Age_Under5", 
				ifelse(grepl("Age_5_9", Age_Cohort, ignore.case = T), "Age_5_9",
				ifelse(grepl("Age_10_14", Age_Cohort, ignore.case = T),"Age_10_14",
				ifelse(grepl("Age_15_17", Age_Cohort, ignore.case = T),"Age_15_17",
				ifelse(grepl("Age_18_19", Age_Cohort, ignore.case = T),"Age_18_19",
				ifelse(grepl("Age_20_24", Age_Cohort, ignore.case = T),"Age_20_24",
				ifelse(grepl("Age_25_29", Age_Cohort, ignore.case = T),"Age_25_29",
				ifelse(grepl("Age_30_34", Age_Cohort, ignore.case = T),"Age_30_34",
				ifelse(grepl("Age_35_44", Age_Cohort, ignore.case = T),"Age_35_44",
				ifelse(grepl("Age_45_54", Age_Cohort, ignore.case = T),"Age_45_54",
				ifelse(grepl("Age_55_64", Age_Cohort, ignore.case = T),"Age_55_64",
				ifelse(grepl("Age_65_74", Age_Cohort, ignore.case = T),"Age_65_74",
				ifelse(grepl("Age_75_84", Age_Cohort, ignore.case = T),"Age_75_84",
				ifelse(grepl("Age_Over_84", Age_Cohort, ignore.case = T),"Age_Over_84","Other")))))))))))))),
			#Race
			Race_desc = ifelse(grepl("_W", Age_Cohort, ignore.case = F), "White", 
				ifelse(grepl("_B", Age_Cohort, ignore.case = F), "Black",
				ifelse(grepl("_NHPI", Age_Cohort, ignore.case = F),"NHPI",
				ifelse(grepl("_AIAN", Age_Cohort, ignore.case = F),"AIAN",
				ifelse(grepl("_A", Age_Cohort, ignore.case = F),"Asian",						
				ifelse(grepl("_L", Age_Cohort, ignore.case = F),"Latino","Other")))))),
			#Sex
			Sex = ifelse(grepl("_Male", Age_Cohort, ignore.case = T), "Male", 
			 ifelse(grepl("_Female", Age_Cohort, ignore.case = T), "Female","Both"
		)))
		#Do again for aggregate BIPOC Category
		Population_Summary_Bipoc.. <-	filter(State_Census.., State == Population_Select_State & Year%in% Study_Year) %>%
			pivot_longer(!c(State,GEOID,NAME,Year), names_to = "Age_Cohort", values_to = "Population")
		#Create new column for race
		Population_Summary_Bipoc.. <- mutate(Population_Summary_Bipoc.., 
			#Age cohort
			Age_Cohort_Update = ifelse(grepl("Age_Under5", Age_Cohort, ignore.case = T), "Age_Under5", 
				ifelse(grepl("Age_5_9", Age_Cohort, ignore.case = T), "Age_5_9",
				ifelse(grepl("Age_10_14", Age_Cohort, ignore.case = T),"Age_10_14",
				ifelse(grepl("Age_15_17", Age_Cohort, ignore.case = T),"Age_15_17",
				ifelse(grepl("Age_18_19", Age_Cohort, ignore.case = T),"Age_18_19",
				ifelse(grepl("Age_20_24", Age_Cohort, ignore.case = T),"Age_20_24",
				ifelse(grepl("Age_25_29", Age_Cohort, ignore.case = T),"Age_25_29",
				ifelse(grepl("Age_30_34", Age_Cohort, ignore.case = T),"Age_30_34",
				ifelse(grepl("Age_35_44", Age_Cohort, ignore.case = T),"Age_35_44",
				ifelse(grepl("Age_45_54", Age_Cohort, ignore.case = T),"Age_45_54",
				ifelse(grepl("Age_55_64", Age_Cohort, ignore.case = T),"Age_55_64",
				ifelse(grepl("Age_65_74", Age_Cohort, ignore.case = T),"Age_65_74",
				ifelse(grepl("Age_75_84", Age_Cohort, ignore.case = T),"Age_75_84",
				ifelse(grepl("Age_Over_84", Age_Cohort, ignore.case = T),"Age_Over_84","Other")))))))))))))),
			#Race
			Race_desc = ifelse(grepl("_W", Age_Cohort, ignore.case = F), "White", 
				ifelse(grepl("_B", Age_Cohort, ignore.case = F), "BIPOC",
				ifelse(grepl("_NHPI", Age_Cohort, ignore.case = F),"BIPOC",
				ifelse(grepl("_AIAN", Age_Cohort, ignore.case = F),"BIPOC",
				ifelse(grepl("_A", Age_Cohort, ignore.case = F),"BIPOC",						
				ifelse(grepl("_L", Age_Cohort, ignore.case = F),"BIPOC","Other")))))),
			#Sex
			Sex = ifelse(grepl("_Male", Age_Cohort, ignore.case = T), "Male", 
			 ifelse(grepl("_Female", Age_Cohort, ignore.case = T), "Female","Both"
		)))
		
		#Summarize
		Population_Summary_Bipoc.. <- Population_Summary_Bipoc.. %>% group_by( Race_desc, State, Age_Cohort_Update, Sex) %>% summarise(Population = sum(Population) )
		#Sum all years
		Population_Summary..  <- Population_Summary..  %>% group_by( Race_desc, State, Age_Cohort_Update, Sex) %>% summarise(Population = sum(Population) )
		#Combine
		Population_Summary.. <- rbind(Population_Summary..[,colnames(Population_Summary_Bipoc..)], filter(Population_Summary_Bipoc.., Race_desc !="White"))
		
		#Prepare standard population data (US)
		##########################################
		#Pivot data
		US_Population_Summary.. <-	 filter(State_Census.., Year%in%Study_Year) %>%
			pivot_longer(!c(State,GEOID,NAME,Year), names_to = "Age_Cohort", values_to = "Population") %>%
			#Update names
			mutate(
			#Age cohort
			Age_Cohort_Update = ifelse(grepl("Age_Under5", Age_Cohort, ignore.case = T), "Age_Under5", 
				ifelse(grepl("Age_5_9", Age_Cohort, ignore.case = T), "Age_5_9",
				ifelse(grepl("Age_10_14", Age_Cohort, ignore.case = T),"Age_10_14",
				ifelse(grepl("Age_15_17", Age_Cohort, ignore.case = T),"Age_15_17",
				ifelse(grepl("Age_18_19", Age_Cohort, ignore.case = T),"Age_18_19",
				ifelse(grepl("Age_20_24", Age_Cohort, ignore.case = T),"Age_20_24",
				ifelse(grepl("Age_25_29", Age_Cohort, ignore.case = T),"Age_25_29",
				ifelse(grepl("Age_30_34", Age_Cohort, ignore.case = T),"Age_30_34",
				ifelse(grepl("Age_35_44", Age_Cohort, ignore.case = T),"Age_35_44",
				ifelse(grepl("Age_45_54", Age_Cohort, ignore.case = T),"Age_45_54",
				ifelse(grepl("Age_55_64", Age_Cohort, ignore.case = T),"Age_55_64",
				ifelse(grepl("Age_65_74", Age_Cohort, ignore.case = T),"Age_65_74",
				ifelse(grepl("Age_75_84", Age_Cohort, ignore.case = T),"Age_75_84",
				ifelse(grepl("Age_Over_84", Age_Cohort, ignore.case = T),"Age_Over_84","Other")))))))))))))),
			#Race
			Race_desc = ifelse(grepl("_W", Age_Cohort, ignore.case = F), "White", 
				ifelse(grepl("_B", Age_Cohort, ignore.case = F), "Black",
				ifelse(grepl("_NHPI", Age_Cohort, ignore.case = F),"NHPI",
				ifelse(grepl("_AIAN", Age_Cohort, ignore.case = F),"AIAN",
				ifelse(grepl("_A", Age_Cohort, ignore.case = F),"Asian",						
				ifelse(grepl("_L", Age_Cohort, ignore.case = F),"Latino","Other")))))),
			#Sex
			Sex = ifelse(grepl("_Male", Age_Cohort, ignore.case = T), "Male", 
			 ifelse(grepl("_Female", Age_Cohort, ignore.case = T), "Female","Both"
			))) %>%
			#Sum all states for national figures
			group_by(Age_Cohort_Update, Race_desc, Sex) %>% summarise(Population = sum(Population,na.rm=T) ) %>%
			mutate(Age_Cohort = Age_Cohort_Update) 
			
		#Prepare standard population data (US) - Do for aggregate BIPOC----
		##########################################
		#Pivot data
		US_Population_Summary_Bipoc.. <-	 filter(State_Census.., Year%in%Study_Year) %>%
			pivot_longer(!c(State,GEOID,NAME,Year), names_to = "Age_Cohort", values_to = "Population") %>%
			#Update names
			mutate(
			#Age cohort
			Age_Cohort_Update = ifelse(grepl("Age_Under5", Age_Cohort, ignore.case = T), "Age_Under5", 
				ifelse(grepl("Age_5_9", Age_Cohort, ignore.case = T), "Age_5_9",
				ifelse(grepl("Age_10_14", Age_Cohort, ignore.case = T),"Age_10_14",
				ifelse(grepl("Age_15_17", Age_Cohort, ignore.case = T),"Age_15_17",
				ifelse(grepl("Age_18_19", Age_Cohort, ignore.case = T),"Age_18_19",
				ifelse(grepl("Age_20_24", Age_Cohort, ignore.case = T),"Age_20_24",
				ifelse(grepl("Age_25_29", Age_Cohort, ignore.case = T),"Age_25_29",
				ifelse(grepl("Age_30_34", Age_Cohort, ignore.case = T),"Age_30_34",
				ifelse(grepl("Age_35_44", Age_Cohort, ignore.case = T),"Age_35_44",
				ifelse(grepl("Age_45_54", Age_Cohort, ignore.case = T),"Age_45_54",
				ifelse(grepl("Age_55_64", Age_Cohort, ignore.case = T),"Age_55_64",
				ifelse(grepl("Age_65_74", Age_Cohort, ignore.case = T),"Age_65_74",
				ifelse(grepl("Age_75_84", Age_Cohort, ignore.case = T),"Age_75_84",
				ifelse(grepl("Age_Over_84", Age_Cohort, ignore.case = T),"Age_Over_84","Other")))))))))))))),
			#Race
			Race_desc = ifelse(grepl("_W", Age_Cohort, ignore.case = F), "White", 
				ifelse(grepl("_B", Age_Cohort, ignore.case = F), "BIPOC",
				ifelse(grepl("_NHPI", Age_Cohort, ignore.case = F),"BIPOC",
				ifelse(grepl("_AIAN", Age_Cohort, ignore.case = F),"BIPOC",
				ifelse(grepl("_A", Age_Cohort, ignore.case = F),"BIPOC",						
				ifelse(grepl("_L", Age_Cohort, ignore.case = F),"BIPOC","Other")))))),
			#Sex
			Sex = ifelse(grepl("_Male", Age_Cohort, ignore.case = T), "Male", 
			 ifelse(grepl("_Female", Age_Cohort, ignore.case = T), "Female","Both"
			))) %>%
			#Sum all states for national figures
			group_by(Age_Cohort_Update,Race_desc,Sex) %>% summarise(Population = sum(Population,na.rm=T)) %>%
			mutate(Age_Cohort = Age_Cohort_Update) 		
		#Sumamrise
		US_Population_Summary_Bipoc.. <- US_Population_Summary_Bipoc.. %>% group_by(Age_Cohort_Update, Race_desc, Sex) %>% summarise(Population = sum(Population) )
		#Sum all years
		#US_Population_Summary..  <- US_Population_Summary..  %>% group_by( Race_desc,Age_Cohort_Update, Sex) %>% summarise(Population = sum(Population) / length(Study_Year))
		#Combine 
		US_Population_Summary.. <- rbind(US_Population_Summary..[,colnames(US_Population_Summary_Bipoc..)], filter(US_Population_Summary_Bipoc.., Race_desc !="White")) %>% 
			mutate(Age_Cohort = Age_Cohort_Update)
		
		#Join population and crash data Oregon
		##############################
		#Remove and replace Age cohort column
		Population_Summary.. <- dplyr::select(mutate(Population_Summary.., Age_Cohort = Age_Cohort_Update), -c(Age_Cohort_Update))
		#Create oregon average
		Population_Summary.. <- rbind(Population_Summary..,cbind(filter(Population_Summary.., Race_desc !="BIPOC") %>% group_by(State, Age_Cohort, Sex) %>% 
			summarise(Population = sum(Population) ),Race_desc = "Oregon")[,c("Race_desc",  "State" ,  "Sex", "Population", "Age_Cohort")])
		#Join with fatal 
		Population_Summary.. <- left_join(filter(Population_Summary..,Sex =="Both"), Select_State_Fatal.., by = c("Age_Cohort", "Race_desc"))
		#Calcuate rate
		Population_Summary.. <- mutate(Population_Summary.., Rate = Count / (Population  /100000))
		#Create oregon population summary, create weights and add to data
		Population_Summary.. <- left_join(Population_Summary..,
			#Calculate weights
			mutate(cbind(filter(US_Population_Summary..) %>% group_by(Age_Cohort) %>% summarise(US_Population = sum(Population)),
				Total_Population = US_Population_Summary.. %>% group_by() %>% 
				summarise(Total_US_Population = sum(Population))),US_Wgt = US_Population / Total_US_Population)[,c("Age_Cohort","US_Population","US_Wgt")],
				by = "Age_Cohort")
		#Create population adjusted rates
		Population_Summary.. <- mutate(Population_Summary.., Adj_Rate = Rate * US_Wgt)
		#Remove age cohorts with zero traffic deaths
		Population_Summary.. <- Population_Summary..[!(is.na(Population_Summary..$Count)),]
		#Sum the weighted rates 
		OR_AgeAdj_Rate.. <- cbind(mutate(Population_Summary.. %>% group_by(Race_desc,Mode) %>% summarise(Rate = sum(Adj_Rate), Count = sum(Count),SE = sum(Adj_Rate) / sum(Count),
			UB = (sum(Adj_Rate) / sum(Count)) * 1.96, LB = (sum(Adj_Rate) / sum(Count)) * -1.96, Population = sum(Population)), Rate_LB = Rate + LB, Rate_UB = Rate + UB),Geography = "Oregon")
		
		#Do national rate calculation
		###########################
		#US_Population_Summary.. <- ungroup(US_Population_Summary..) 
		US_Population_Summary.. <- dplyr::select(mutate(ungroup(US_Population_Summary..) , Age_Cohort = Age_Cohort_Update), -c(Age_Cohort_Update)) 
		#US_Population_Summary.. <- mutate(US_Population_Summary.., Age_Cohort = Age_Cohort_Update)
		
		#Create US average
		US_Population_Summary.. <- rbind(US_Population_Summary..,cbind(US_Population_Summary.. %>% group_by( Age_Cohort, Sex) %>% 
			summarise(Population = sum(Population)),Race_desc = "US")[,colnames(US_Population_Summary..)])
		#Join with fatal 
		US_Population_Summary.. <- left_join(filter(US_Population_Summary..,Sex =="Both"), US_Fatal.., by = c("Age_Cohort", "Race_desc"))
		#Calcuate rate
		US_Population_Summary.. <- mutate(US_Population_Summary.., Rate = Count / (Population /100000))
		#Create US population summary, create weights and add to data
		US_Population_Summary.. <- left_join(US_Population_Summary..,
			#Calculate weights
			mutate(cbind(US_Population_Summary.. %>% group_by(Age_Cohort) %>% summarise(US_Population = sum(Population)),
				Total_Population = US_Population_Summary.. %>% group_by() %>% 
				summarise(Total_US_Population = sum(Population))),US_Wgt = US_Population / Total_US_Population)[,c("Age_Cohort","US_Population","US_Wgt")],
				by = "Age_Cohort")
		#Create population adjusted rates
		US_Population_Summary.. <- mutate(US_Population_Summary.., Adj_Rate = Rate * US_Wgt)
		#Remove age cohorts with zero traffic deaths
		US_Population_Summary.. <- US_Population_Summary..[!(is.na(US_Population_Summary..$Count)),]
		#Sum the weighted rates 
		US_AgeAdj_Rate.. <- cbind(mutate(US_Population_Summary.. %>% group_by(Race_desc, Mode) %>% summarise(Rate = sum(Adj_Rate), Count = sum(Count),SE = sum(Adj_Rate) / sum(Count),
			UB = (sum(Adj_Rate) / sum(Count)) * 1.96,LB = (sum(Adj_Rate) / sum(Count)) * -1.96, Population = sum(Population)), Rate_LB = Rate + LB, Rate_UB = Rate + UB),Geography = "US")
				
		#Store rate
		#################
		Master_Rate.. <- rbind(Master_Rate..,cbind(rbind(OR_AgeAdj_Rate..,US_AgeAdj_Rate..),Year = paste(FARS_Period.[1],"-",FARS_Period.[5],sep="")))
		
	}
			
	#Change names to better descriptions for charting
	#################################
	Master_Rate.. <- mutate(Master_Rate.., Race_desc = ifelse(Race_desc == "Oregon", "Oregon Average",
		ifelse(Race_desc == "US", "US Average",
		#Chnage to more descr. race categories
		ifelse(Race_desc == "AIAN", "American Indian &\n Alaskan Native",
		ifelse(Race_desc == "NHPI", "Native Hawaiian &\n Pacific Islander",
		ifelse(Race_desc == "Latino", "Latinx",
		ifelse(!(Race_desc%in%c("AIAN","Oregon","NHPI")), Race_desc,NA)))))),
		Race_desc = factor(Race_desc,c("American Indian &\n Alaskan Native","Asian","Black","Latinx","Native Hawaiian &\n Pacific Islander","White","BIPOC","Oregon Average","US Average")))
	
	#Store data
	save(Master_Rate.., file = "/Data/Injury_Rates.RData")
	
	#Explore results
	as.data.frame(filter(Master_Rate.. , Mode =="Pedestrian" & Geography =="Oregon")) [,c("Race_desc","Mode","Count","Population","Rate","Rate_LB","Rate_UB","SE","UB","LB","Geography","Year")]
	
	#Chart the results
	#-----------------------
	#Chart most current period of Fatal Pedestrian Injury Rates
	###########################
	dat <- 	mutate(filter(Master_Rate.., Year == "2014-2018" & Geography == "Oregon" & Mode == "Pedestrian" ))
	#dat <- P2
	dat$Rate_LB[dat$Rate_LB <0] <- 0
	Colors. <- colorRampPalette(c("skyblue", "lightgreen"))(nrow(dat))
	Plot1 <- ggplot(dat, aes(x = Race_desc, y = Rate)) + 
		#coord_flip() +
		geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Race_desc))  +
		geom_errorbar(aes(ymin=Rate_LB, ymax=Rate_UB), width=.2, position=position_dodge(.9)) +
		#scale_y_continuous(labels=percent) +
		geom_text(aes(x = Race_desc, y = 1, label = round(Rate,1)),fontface = "bold", size = 8, position = position_dodge(width = .5)) +
		labs(y = "Fatal Pedestrian Injuries per 100,000 Person-Years" , x = "Census/NHTSA Race Categories", caption ="Source: FARS & Census/n*Age-adjusted Rates") +
		ggtitle("Pedestrian /n Traffic Injury Death Rates* by Race/nOregon/n2014-2018") + 
		scale_fill_manual( values=Colors. ) +
		theme(text = element_text(size = 16)) +
		facet_wrap(~Mode, nrow = 1,scales = "free") +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		theme(legend.position = "none") +
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=20),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 	

	#Chart second period of Fatal Pedestrian Injury Rates
	dat <- 	mutate(filter(Master_Rate.., Year == "2009-2013" & Geography == "Oregon" & Mode == "Pedestrian" ))
	dat$Rate_LB[dat$Rate_LB <0] <- 0
	Colors. <- colorRampPalette(c("skyblue", "lightgreen"))(nrow(dat))
	Plot2 <- ggplot(dat, aes(x = Race_desc, y = Rate)) + 
		#coord_flip() +
		geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Race_desc))  +
		geom_errorbar(aes(ymin=Rate_LB, ymax=Rate_UB), width=.2, position=position_dodge(.9)) +
		#scale_y_continuous(labels=percent) +
		geom_text(aes(x = Race_desc, y = 1, label = round(Rate,1)),fontface = "bold", size = 8, position = position_dodge(width = .5)) +
		labs(y = "Fatal Pedestrian Injuries per 100,000 Person-Years" , x = "Census/NHTSA Race Categories", caption ="Source: FARS & Census/n*Age-adjusted Rates") +
		ggtitle("Pedestrian /n Traffic Injury Death Rates* by Race/nOregon/n2009-2013") + 
		scale_fill_manual( values=Colors. ) +
		theme(text = element_text(size = 16)) +
		facet_wrap(~Mode, nrow = 1,scales = "free") +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		theme(legend.position = "none") +
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=20),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 	
	
	#Arrange data into a single plot
	grid.arrange(Plot1, Plot2)
	
	#Line chart show Oregon - Shows pedestrian but select different user type for other modes
	#####################################
	#Select a user type to show
	user_type <- "Pedestrian"
	dat <- 	filter(Master_Rate.., Mode%in%user_type & Year%in%c("2009-2013","2014-2018") & Race_desc != "Native Hawaiian &\n Pacific Islander")
	dat$Rate_LB[dat$Rate_LB <0] <- 0
	dat2 <- mutate(filter(dat, Race_desc%in%c("Oregon Average","US Average")), Geography = paste(Geography, " Average"))
	dat <- filter(dat, !(Race_desc%in%c("Oregon Average","US Average")))
	for(i in 1:length(unique(dat$Race_desc))){dat2 <- rbind(dat2,mutate(dat2,Race_desc = unique(dat$Race_desc)[i]))}
	dat2 <- filter(dat2, !(Race_desc%in%c("Oregon Average","US Average")))
	user_type
	#dat2$Type <- "Average"
	dat <- rbind(dat, dat2)
	dat <- filter(dat, !(Geography%in%c("US","US  Average")))
	dat$Geography[dat$Geography%in%"Oregon"] <- "Rate for Race"
	dat$Geography[dat$Geography%in%"US"] <- "US Rate for Race"
	dat$Legend <- dat$Geography
	dat$Label <- round(dat$Rate,1)
	dat$Label[!(dat$Legend%in%"Rate for Race")] <- ""
	Colors. <- colorRampPalette(c("blue", "darkgreen"))(3)
	ggplot(dat, aes(x = Year, y = Rate, group = Legend)) + 
		geom_line(aes(x = Year, y = Rate, color = Legend), size = .75)  +
		geom_point(aes(x = Year, y = Rate, color = Legend), size = 4.5)  +
		geom_ribbon(aes(ymin = Rate_LB, ymax = Rate_UB, fill = Legend),alpha=0.3) +
		geom_text(aes(x = Year, y = Rate + 1, label = Label), fontface = "bold", size = 4) +
		facet_wrap(~Race_desc) + 
		scale_y_continuous( breaks=pretty_breaks(7), expand = c(0.15,0)) +
		# geom_text(aes(x = Year, y = Rate, label = round(Rate,1)),fontface = "bold", size = 4, position = position_dodge(width = .25)) +
		labs(y = "Deaths per 100,000 Person-Years" , x = "Analysis Period", caption ="*Age-adjusted Rates/nNHPI not shown due to small cell size") +
		ggtitle(paste(user_type,"\n Traffic Injury Death Rates* by Race/nOregon/nChange Over Time",sep="")) + 
		scale_color_manual( values=Colors. ) +
		theme(text = element_text(size = 16)) +
		#facet_wrap(~Year, nrow = 1) +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		guides(color=guide_legend(title="Legend")) +	  
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=20),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 

	#Cycle through select modes and construct charts for those modes
	#####################################
	for(user_type in c("Bicyclist","Motor Vehicle","Pedestrian")){
		#Show change over time
		####################################
		dat <- 	mutate(filter(Master_Rate.., Geography == "Oregon" & Mode == user_type))
		dat$Rate_LB[dat$Rate_LB <0] <- 0
		
		Colors. <- colorRampPalette(c("skyblue", "lightgreen"))(3)
		Plot3 <- ggplot(dat, aes(x = Race_desc, y = Rate, group = Year)) + 
			#coord_flip() +
			geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Year))  +
			geom_errorbar(aes(ymin=Rate_LB, ymax=Rate_UB), width=.2,  position = position_dodge(width = 0.5))+
			geom_text(aes(x = Race_desc, y = Rate, label = round(Rate,1)),fontface = "bold", size = 5, position = position_dodge(width = .5)) +
			labs(y = "Deaths per 100,000 Person-Years" , x = "Census/NHTSA Race Categories", caption ="*Age-adjusted Rates") +
			ggtitle(paste(user_type,"\n Traffic Injury Death Rates* by Race/nOregon/nChange Over Time",sep="")) + 
			scale_fill_manual( values=Colors. ) +
			theme(text = element_text(size = 16)) +
			#facet_wrap(~Year, nrow = 1) +
			#Center plot
			theme(plot.title = element_text(hjust = 0.5)) + 
			#theme(legend.position = "none") +
			theme(legend.text=element_text(size=12),legend.title=element_text(size=16)) +
			theme(axis.text.y=element_text(size=20),axis.title.y=element_text(size=16)) +
			theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
			theme(axis.title.x=element_text(size=16)) +
			theme(strip.text.x = element_text(size = 14)) 
		
		
		#Line chart show Oregon
		#####################################
		dat <- 	filter(Master_Rate.., Mode%in% user_type)
		dat$Rate_LB[dat$Rate_LB <0] <- 0
		dat2 <- mutate(filter(dat, Race_desc%in%c("Oregon Average","US Average")), Geography = paste(Geography, " Average"))
		dat <- filter(dat, !(Race_desc%in%c("Oregon Average","US Average")))
		for(i in 1:length(unique(dat$Race_desc))){dat2 <- rbind(dat2,mutate(dat2,Race_desc = unique(dat$Race_desc)[i]))}
		dat2 <- filter(dat2, !(Race_desc%in%c("Oregon Average","US Average")))
		
		#dat2$Type <- "Average"
		dat <- rbind(dat, dat2)
		dat <- filter(dat, !(Geography%in%c("US","US  Average")))
		dat$Geography[dat$Geography%in%"Oregon"] <- "Rate for Race"
		dat$Geography[dat$Geography%in%"US"] <- "US Rate for Race"
		dat$Legend <- dat$Geography
		Colors. <- colorRampPalette(c("blue", "darkgreen"))(3)
		Plot4 <- ggplot(dat, aes(x = Year, y = Rate, group = Legend)) + 
		  #coord_flip() +
		  #geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Year))  +
		  geom_line(aes(x = Year, y = Rate, color = Legend), size = .75)  +
		  geom_point(aes(x = Year, y = Rate, color = Legend), size = 4.5)  +
		  geom_ribbon(aes(ymin = Rate_LB, ymax = Rate_UB, fill = Legend),alpha=0.3) +

		  facet_wrap(~Race_desc) + 
		  scale_y_continuous( breaks=pretty_breaks(7), expand = c(0.15,0)) +
		 # geom_text(aes(x = Year, y = Rate, label = round(Rate,1)),fontface = "bold", size = 4, position = position_dodge(width = .25)) +
		  labs(y = "Deaths per 100,000" , x = "Analysis Period", caption ="*Age-adjusted Rates") +
		  ggtitle(paste(user_type,"\n Traffic Injury Death Rates* by Race/nOregon/nChange Over Time",sep="")) + 
		  scale_color_manual( values=Colors. ) +
		  theme(text = element_text(size = 16)) +
		  #facet_wrap(~Year, nrow = 1) +
		  #Center plot
		  theme(plot.title = element_text(hjust = 0.5)) + 
		  guides(color=guide_legend(title="Legend")) +	  
		  theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		  theme(axis.text.y=element_text(size=20),axis.title.y=element_text(size=16)) +
		  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		  theme(axis.title.x=element_text(size=16)) +
		  theme(strip.text.x = element_text(size = 14)) 
		
			  
		#Write out pdf
		pdf(file = paste("Reports/",user_type,"_FARS_by_Race.pdf",sep=""), height = 11, width = 18)
		print(Plot3)
		print(Plot4)
		#print(Plot5)
		dev.off()
}
	
	