#Author: Josh Roll 
#Date: 9/2020 
#License: Apache 2.0

#Load libraries
#-----------------------
	#Correct lib path
	library(ggplot2)
	library(dplyr)
	library(scales)
	library(tigris)
	library(tidycensus)
	library(acs)
	library(zipcode)
	library(arcgisbinding)
	library(tidyr)
	library(rgeos)
	library(rgdal)

	#install.packages("PUMSutils")
	
#Define custom scripts functions
#------------------------------
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }	

	#Write function to rename and prepare population and age census variable names
	############################
	all_age_race_rename <- function(dat){
		#Rename variables to meaningful names
		dat <- mutate(dat,
			#White 
			#######
			#Male---
			Age_Under5_Male_W = B01001H_003E, Age_5_9_Male_W = B01001H_004E, Age_10_14_Male_W = B01001H_005E, Age_15_17_Male_W = B01001H_006E, Age_18_19_Male_W = B01001H_007E, Age_20_24_Male_W = B01001H_008E,
			Age_25_29_Male_W = B01001H_009E, Age_30_34_Male_W = B01001H_010E, Age_35_44_Male_W = B01001H_011E, Age_45_54_Male_W = B01001H_012E, Age_55_64_Male_W = B01001H_013E,	
			Age_65_74_Male_W = B01001H_014E, Age_75_84_Male_W = B01001H_015E, Age_Over_84_Male_W = B01001H_016E, 
			#Female---
			Age_Under5_Female_W = B01001H_018E, Age_5_9_Female_W = B01001H_019E, Age_10_14_Female_W = B01001H_020E, Age_15_17_Female_W = B01001H_021E, Age_18_19_Female_W = B01001H_022E, Age_20_24_Female_W = B01001H_023E,
			Age_25_29_Female_W = B01001H_024E, Age_30_34_Female_W = B01001H_025E, Age_35_44_Female_W = B01001H_026E, Age_45_54_Female_W = B01001H_027E, Age_55_64_Female_W = B01001H_028E,	
			Age_65_74_Female_W = B01001H_029E, Age_75_84_Female_W = B01001H_030E, Age_Over_84_Female_W = B01001H_031E, 
			#Black
			#######
			#Male---
			Age_Under5_Male_B = B01001B_003E, Age_5_9_Male_B = B01001B_004E, Age_10_14_Male_B = B01001B_005E, Age_15_17_Male_B = B01001B_006E, Age_18_19_Male_B = B01001B_007E, Age_20_24_Male_B = B01001B_008E,
			Age_25_29_Male_B = B01001B_009E, Age_30_34_Male_B = B01001B_010E, Age_35_44_Male_B = B01001B_011E, Age_45_54_Male_B = B01001B_012E, Age_55_64_Male_B = B01001B_013E,	
			Age_65_74_Male_B = B01001B_014E, Age_75_84_Male_B = B01001B_015E, Age_Over_84_Male_B = B01001B_016E, 
			#Female---
			Age_Under5_Female_B = B01001B_018E, Age_5_9_Female_B = B01001B_019E, Age_10_14_Female_B = B01001B_020E, Age_15_17_Female_B = B01001B_021E, Age_18_19_Female_B = B01001B_022E, Age_20_24_Female_B = B01001B_023E,
			Age_25_29_Female_B = B01001B_024E, Age_30_34_Female_B = B01001B_025E, Age_35_44_Female_B = B01001B_026E, Age_45_54_Female_B = B01001B_027E, Age_55_64_Female_B = B01001B_028E,	
			Age_65_74_Female_B = B01001B_029E, Age_75_84_Female_B = B01001B_030E, Age_Over_84_Female_B = B01001B_031E, 
			#AIAN
			#######
			#Male---
			Age_Under5_Male_AIAN = B01001C_003E, Age_5_9_Male_AIAN = B01001C_004E, Age_10_14_Male_AIAN = B01001C_005E, Age_15_17_Male_AIAN = B01001C_006E, Age_18_19_Male_AIAN = B01001C_007E, Age_20_24_Male_AIAN = B01001C_008E,
			Age_25_29_Male_AIAN = B01001C_009E, Age_30_34_Male_AIAN = B01001C_010E, Age_35_44_Male_AIAN = B01001C_011E, Age_45_54_Male_AIAN = B01001C_012E, Age_55_64_Male_AIAN = B01001C_013E,	
			Age_65_74_Male_AIAN = B01001C_014E, Age_75_84_Male_AIAN = B01001C_015E, Age_Over_84_Male_AIAN = B01001C_016E, 
			#Female---
			Age_Under5_Female_AIAN = B01001C_018E, Age_5_9_Female_AIAN = B01001C_019E, Age_10_14_Female_AIAN = B01001C_020E, Age_15_17_Female_AIAN = B01001C_021E, Age_18_19_Female_AIAN = B01001C_022E, Age_20_24_Female_AIAN = B01001C_023E,
			Age_25_29_Female_AIAN = B01001C_024E, Age_30_34_Female_AIAN = B01001C_025E, Age_35_44_Female_AIAN = B01001C_026E, Age_45_54_Female_AIAN = B01001C_027E, Age_55_64_Female_AIAN = B01001C_028E,	
			Age_65_74_Female_AIAN = B01001C_029E, Age_75_84_Female_AIAN = B01001C_030E, Age_Over_84_Female_AIAN = B01001C_031E, 
			#Asian
			#######
			#Male---
			Age_Under5_Male_A = B01001D_003E, Age_5_9_Male_A = B01001D_004E, Age_10_14_Male_A = B01001D_005E, Age_15_17_Male_A = B01001D_006E, Age_18_19_Male_A = B01001D_007E, Age_20_24_Male_A = B01001D_008E,
			Age_25_29_Male_A = B01001D_009E, Age_30_34_Male_A = B01001D_010E, Age_35_44_Male_A = B01001D_011E, Age_45_54_Male_A = B01001D_012E, Age_55_64_Male_A = B01001D_013E,	
			Age_65_74_Male_A = B01001D_014E, Age_75_84_Male_A = B01001D_015E, Age_Over_84_Male_A = B01001D_016E, 
			#Female---
			Age_Under5_Female_A = B01001D_018E, Age_5_9_Female_A = B01001D_019E, Age_10_14_Female_A = B01001D_020E, Age_15_17_Female_A = B01001D_021E, Age_18_19_Female_A = B01001D_022E, Age_20_24_Female_A = B01001D_023E,
			Age_25_29_Female_A = B01001D_024E, Age_30_34_Female_A = B01001D_025E, Age_35_44_Female_A = B01001D_026E, Age_45_54_Female_A = B01001D_027E, Age_55_64_Female_A = B01001D_028E,	
			Age_65_74_Female_A = B01001D_029E, Age_75_84_Female_A = B01001D_030E, Age_Over_84_Female_A = B01001D_031E, 
			#NHPI
			#######
			#Male---
			Age_Under5_Male_NHPI = B01001E_003E, Age_5_9_Male_NHPI = B01001E_004E, Age_10_14_Male_NHPI = B01001E_005E, Age_15_17_Male_NHPI = B01001E_006E, Age_18_19_Male_NHPI = B01001E_007E, Age_20_24_Male_NHPI = B01001E_008E,
			Age_25_29_Male_NHPI = B01001E_009E, Age_30_34_Male_NHPI = B01001E_010E, Age_35_44_Male_NHPI = B01001E_011E, Age_45_54_Male_NHPI = B01001E_012E, Age_55_64_Male_NHPI = B01001E_013E,	
			Age_65_74_Male_NHPI = B01001E_014E, Age_75_84_Male_NHPI = B01001E_015E, Age_Over_84_Male_NHPI = B01001E_016E, 
			#Female---
			Age_Under5_Female_NHPI = B01001E_018E, Age_5_9_Female_NHPI = B01001E_019E, Age_10_14_Female_NHPI = B01001E_020E, Age_15_17_Female_NHPI = B01001E_021E, Age_18_19_Female_NHPI = B01001E_022E, Age_20_24_Female_NHPI = B01001E_023E,
			Age_25_29_Female_NHPI = B01001E_024E, Age_30_34_Female_NHPI = B01001E_025E, Age_35_44_Female_NHPI = B01001E_026E, Age_45_54_Female_NHPI = B01001E_027E, Age_55_64_Female_NHPI = B01001E_028E,	
			Age_65_74_Female_NHPI = B01001E_029E, Age_75_84_Female_NHPI = B01001E_030E, Age_Over_84_Female_NHPI = B01001E_031E, 
			#Latinx
			#######
			#Male---
			Age_Under5_Male_L = B01001I_003E, Age_5_9_Male_L = B01001I_004E, Age_10_14_Male_L = B01001I_005E, Age_15_17_Male_L = B01001I_006E, Age_18_19_Male_L = B01001I_007E, Age_20_24_Male_L = B01001I_008E,
			Age_25_29_Male_L = B01001I_009E, Age_30_34_Male_L = B01001I_010E, Age_35_44_Male_L = B01001I_011E, Age_45_54_Male_L = B01001I_012E, Age_55_64_Male_L = B01001I_013E,	
			Age_65_74_Male_L = B01001I_014E, Age_75_84_Male_L = B01001I_015E, Age_Over_84_Male_L = B01001I_016E, 
			#Female---
			Age_Under5_Female_L = B01001I_018E, Age_5_9_Female_L = B01001I_019E, Age_10_14_Female_L = B01001I_020E, Age_15_17_Female_L = B01001I_021E, Age_18_19_Female_L = B01001I_022E, Age_20_24_Female_L = B01001I_023E,
			Age_25_29_Female_L = B01001I_024E, Age_30_34_Female_L = B01001I_025E, Age_35_44_Female_L = B01001I_026E, Age_45_54_Female_L = B01001I_027E, Age_55_64_Female_L = B01001I_028E,	
			Age_65_74_Female_L = B01001I_029E, Age_75_84_Female_L = B01001I_030E, Age_Over_84_Female_L = B01001I_031E
		)
			
		#Sum male and female
		#---------------------------
		dat <- mutate(dat,
			#White
			Age_Under5_W = Age_Under5_Male_W + Age_Under5_Female_W, Age_5_9_W = Age_5_9_Male_W + Age_5_9_Female_W, Age_10_14_W = Age_10_14_Male_W + Age_10_14_Female_W,
				Age_15_17_W = Age_15_17_Male_W + Age_15_17_Female_W, Age_18_19_W = Age_18_19_Male_W + Age_18_19_Female_W, Age_20_24_W = Age_20_24_Male_W + Age_20_24_Female_W,
				Age_25_29_W = Age_25_29_Male_W + Age_25_29_Female_W, Age_30_34_W = Age_30_34_Male_W + Age_30_34_Female_W, Age_35_44_W = Age_35_44_Male_W + Age_35_44_Female_W,
				Age_45_54_W = Age_45_54_Male_W + Age_45_54_Female_W, Age_55_64_W = Age_55_64_Male_W + Age_55_64_Female_W, Age_65_74_W = Age_65_74_Male_W + Age_65_74_Female_W,
				 Age_75_84_W =  Age_75_84_Male_W +  Age_75_84_Female_W, Age_Over_84_W = Age_Over_84_Male_W + Age_Over_84_Female_W,
			#Black
			Age_Under5_B = Age_Under5_Male_B + Age_Under5_Female_B, Age_5_9_B = Age_5_9_Male_B + Age_5_9_Female_B, Age_10_14_B = Age_10_14_Male_B + Age_10_14_Female_B,
				Age_15_17_B = Age_15_17_Male_B + Age_15_17_Female_B, Age_18_19_B = Age_18_19_Male_B + Age_18_19_Female_B, Age_20_24_B = Age_20_24_Male_B + Age_20_24_Female_B,
				Age_25_29_B = Age_25_29_Male_B + Age_25_29_Female_B, Age_30_34_B = Age_30_34_Male_B + Age_30_34_Female_B, Age_35_44_B = Age_35_44_Male_B + Age_35_44_Female_B,
				Age_45_54_B = Age_45_54_Male_B + Age_45_54_Female_B, Age_55_64_B = Age_55_64_Male_B + Age_55_64_Female_B, Age_65_74_B = Age_65_74_Male_B + Age_65_74_Female_B,
				 Age_75_84_B =  Age_75_84_Male_B +  Age_75_84_Female_B, Age_Over_84_B = Age_Over_84_Male_B + Age_Over_84_Female_B,
			#AIAN
			Age_Under5_AIAN = Age_Under5_Male_AIAN + Age_Under5_Female_AIAN, Age_5_9_AIAN = Age_5_9_Male_AIAN + Age_5_9_Female_AIAN, Age_10_14_AIAN = Age_10_14_Male_AIAN + Age_10_14_Female_AIAN,
				Age_15_17_AIAN = Age_15_17_Male_AIAN + Age_15_17_Female_AIAN, Age_18_19_AIAN = Age_18_19_Male_AIAN + Age_18_19_Female_AIAN, Age_20_24_AIAN = Age_20_24_Male_AIAN + Age_20_24_Female_AIAN,
				Age_25_29_AIAN = Age_25_29_Male_AIAN + Age_25_29_Female_AIAN, Age_30_34_AIAN = Age_30_34_Male_AIAN + Age_30_34_Female_AIAN, Age_35_44_AIAN = Age_35_44_Male_AIAN + Age_35_44_Female_AIAN,
				Age_45_54_AIAN = Age_45_54_Male_AIAN + Age_45_54_Female_AIAN, Age_55_64_AIAN = Age_55_64_Male_AIAN + Age_55_64_Female_AIAN, Age_65_74_AIAN = Age_65_74_Male_AIAN + Age_65_74_Female_AIAN,
				 Age_75_84_AIAN =  Age_75_84_Male_AIAN +  Age_75_84_Female_AIAN, Age_Over_84_AIAN = Age_Over_84_Male_AIAN + Age_Over_84_Female_AIAN,
			#Asian
			Age_Under5_A = Age_Under5_Male_A + Age_Under5_Female_A, Age_5_9_A = Age_5_9_Male_A + Age_5_9_Female_A, Age_10_14_A = Age_10_14_Male_A + Age_10_14_Female_A,
				Age_15_17_A = Age_15_17_Male_A + Age_15_17_Female_A, Age_18_19_A = Age_18_19_Male_A + Age_18_19_Female_A, Age_20_24_A = Age_20_24_Male_A + Age_20_24_Female_A,
				Age_25_29_A = Age_25_29_Male_A + Age_25_29_Female_A, Age_30_34_A = Age_30_34_Male_A + Age_30_34_Female_A, Age_35_44_A = Age_35_44_Male_A + Age_35_44_Female_A,
				Age_45_54_A = Age_45_54_Male_A + Age_45_54_Female_A, Age_55_64_A = Age_55_64_Male_A + Age_55_64_Female_A, Age_65_74_A = Age_65_74_Male_A + Age_65_74_Female_A,
				 Age_75_84_A =  Age_75_84_Male_A +  Age_75_84_Female_A, Age_Over_84_A = Age_Over_84_Male_A + Age_Over_84_Female_A,
			#NHPI
			Age_Under5_NHPI = Age_Under5_Male_NHPI + Age_Under5_Female_NHPI, Age_5_9_NHPI = Age_5_9_Male_NHPI + Age_5_9_Female_NHPI, Age_10_14_NHPI = Age_10_14_Male_NHPI + Age_10_14_Female_NHPI,
				Age_15_17_NHPI = Age_15_17_Male_NHPI + Age_15_17_Female_NHPI, Age_18_19_NHPI = Age_18_19_Male_NHPI + Age_18_19_Female_NHPI, Age_20_24_NHPI = Age_20_24_Male_NHPI + Age_20_24_Female_NHPI,
				Age_25_29_NHPI = Age_25_29_Male_NHPI + Age_25_29_Female_NHPI, Age_30_34_NHPI = Age_30_34_Male_NHPI + Age_30_34_Female_NHPI, Age_35_44_NHPI = Age_35_44_Male_NHPI + Age_35_44_Female_NHPI,
				Age_45_54_NHPI = Age_45_54_Male_NHPI + Age_45_54_Female_NHPI, Age_55_64_NHPI = Age_55_64_Male_NHPI + Age_55_64_Female_NHPI, Age_65_74_NHPI = Age_65_74_Male_NHPI + Age_65_74_Female_NHPI,
				 Age_75_84_NHPI =  Age_75_84_Male_NHPI +  Age_75_84_Female_NHPI, Age_Over_84_NHPI = Age_Over_84_Male_NHPI + Age_Over_84_Female_NHPI,
			#LAtinx
			Age_Under5_L = Age_Under5_Male_L + Age_Under5_Female_L, Age_5_9_L = Age_5_9_Male_L + Age_5_9_Female_L, Age_10_14_L = Age_10_14_Male_L + Age_10_14_Female_L,
				Age_15_17_L = Age_15_17_Male_L + Age_15_17_Female_L, Age_18_19_L = Age_18_19_Male_L + Age_18_19_Female_L, Age_20_24_L = Age_20_24_Male_L + Age_20_24_Female_L,
				Age_25_29_L = Age_25_29_Male_L + Age_25_29_Female_L, Age_30_34_L = Age_30_34_Male_L + Age_30_34_Female_L, Age_35_44_L = Age_35_44_Male_L + Age_35_44_Female_L,
				Age_45_54_L = Age_45_54_Male_L + Age_45_54_Female_L, Age_55_64_L = Age_55_64_Male_L + Age_55_64_Female_L, Age_65_74_L = Age_65_74_Male_L + Age_65_74_Female_L,
				 Age_75_84_L =  Age_75_84_Male_L +  Age_75_84_Female_L, Age_Over_84_L = Age_Over_84_Male_L + Age_Over_84_Female_L
		)	
		#Return data frame
		dat
	}

#Set other environmental conditions
#--------------------------------
	#Set working directory
	setwd("")
	#Set Census API Key - DOnt; have a key go to the link to sig up for a free one https://api.census.gov/data/key_signup.html
	Developer_Key <- ""
	
#---------------------------------------------------------------------------------------------------------------	
#---------------------------------------------------------------------------------------------------------------	
#Pull population and age by race information for FARS analysis
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

	#Develop list of variables
	Vars. <- c(
		#White by age
		paste("B01001H_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Black by age
		paste("B01001B_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#American Indian
		paste("B01001C_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Asian
		paste("B01001D_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#NAtive Hawwain Pacific Islander
		paste("B01001E_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Hispanic
		paste("B01001I_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)]
	)
		
	#State - 5 year
	######################################
	#Define years
	Acs_Years. <- 2009:2018	
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "state", variables = Vars., output = "wide", year = acs_year, geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Run custom rename function
		Census.. <- mutate(all_age_race_rename(Census..), Year = acs_year, State = NAME)[,c(1:2,339:592)]
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "State_Age_Sex_2009_2018_5yr.RData")
	
	#State - 1 Year
	######################################
	#Define years
	Acs_Years. <- 2009:2018	
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "state", variables = Vars., output = "wide", year = acs_year,  survey = "acs1", geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Run custom rename function
		Census.. <- mutate(all_age_race_rename(Census..), Year = acs_year, State = NAME)[,c(1:2,339:592)]
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "State_Age_Sex_2009_2018_1yr.RData")
		
		