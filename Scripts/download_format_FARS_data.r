#Author: Josh Roll 
#Date: 10/8/2020
#Description: This script downloads and prepares FARS data for analysis of injury rates by Race.  It requires a few supporting files available in the repository


#Load libraries
#--------------------------
	
	library(zip)
	library(foreign)
	library(httr)
	library(readxl)
	library(Hmisc)
	library(dplyr)
	library(readxl)

#Define functions used below
#---------------------------------
	#Function to format column headers.  
	simpleCap <- function(x) {
	  s <- strsplit(x, " ")[[1]]
	  paste(toupper(substring(s, 1,1)), substring(s, 2),
		  sep="", collapse=" ")
	}		
	#County and state GSA codes - source:	https://www.gsa.gov/cdnstatic/FRPP_GLC_-_United_StatesDEC72020.xlsx
	Gsa_Codes.. <- read_excel("Suppoprting Data/Documentation/FRPP_GLC_-_United_StatesDEC72020.xlsx", sheet = "GeoLocation_UnitedStates")
	#Transofrm column names to support joining below
	colnames(Gsa_Codes.. )  <- gsub(" ","_",colnames(Gsa_Codes.. ))
	Gsa_Codes.. <- Gsa_Codes.. %>% mutate(State = as.numeric(State_Code), County = as.numeric(County_Code), City = as.numeric(City_Code)) %>% mutate(Key = paste(County, "-",State_Name,sep=""))
	Gsa_Codes.. <- Gsa_Codes..[!duplicated(Gsa_Codes..$Key),]


###########################################################################
#Step 1 - Download RAW Data ###############################################
###########################################################################
	
	#Define storage directory
	Storage_Directory <- "Data/From NHTSA FTP"
	#Check to see if file exists 
	if(!(file.exists(Storage_Directory))){dir.create(Storage_Directory)}
	#Define years 
	Years_To_Process. <- 1975:2019
	
	#Loop through each year 
	for(yr in Years_To_Process.){
		#Create directory name
		Directory <- paste(Storage_Directory,"/",yr,sep="")		
		#Chgeck to see if directory exists if not create and download data
		if(!(dir.exists(Directory))){
			dir.create(Directory)
			dir.create(paste(Directory,"/Csvs",sep=""))
			#Determine url based on year
			Url <- paste("https://www.nhtsa.gov/nhtsa-s3/download?p=nhtsa/downloads/FARS/",yr,"/National/FARS",yr,"NationalCSV.zip",sep="")
			setwd(Directory)
			Zip_Path <- paste(yr,sep="")
			download.file(Url,destfile = Zip_Path)
			#Unzip		
			temp <- tempfile()
			unzip(Zip_Path, exdir = temp)
			#List files and find .csv
			Zip_Files. <- list.files( temp)
			#Read then write all those files to network drive
			###############
			for(current_file in Zip_Files.){
				Temp_File.. <- read.csv(paste(temp, "/",current_file, sep=""))
				write.csv(Temp_File.., file = paste(Directory, "/Csvs/",current_file, sep=""),row.names = F)
			}			
		}
		#Print progress
		print(paste("Done Downloading and Writing to File ", yr," FARS Data",sep=""))
	}
		

###########################################################################
#Step 2 - Process Person Data for Race ####################################
###########################################################################

	#Define storage directory
	Storage_Directory <- "Data/From NHTSA FTP"
	
	#Load data dictionary files - these are custom files derived from the Fatality Analysis Reporting System (FARS) Analytical User’s Manual, 1975-2019 - https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/813023
	#Race----
	Race_dd.. <- read_excel("Supporting Data/Data_Dictionary_FARS.xlsx", sheet = "Race")
	Race_dd.. <- mutate(Race_dd.. , Race = as.numeric(Race))
	#Preson Type-----
	Person_Type_dd.. <- read_excel("Supporting Data/Data_Dictionary_FARS.xlsx", sheet = "Person_Type")
	Person_Type_dd.. <- mutate(Person_Type_dd.. , Per_typ = as.numeric(Per_typ))
	
	#Init a data frame to store all person records
	Master_Person.. <- data.frame()
	#Loop through each year
	for(yr in Years_To_Process.){
		#Load person table
		Load_Person.. <- read.csv(paste(Storage_Directory,"/",yr,"/Csvs/PERSON.csv", sep=""))
		colnames(Load_Person..) <- capitalize(tolower(colnames(Load_Person..)))
		#Lod crash table
		Crash.. <- read.csv(paste(Storage_Directory,"/",yr,"/Csvs/ACCIDENT.csv", sep=""))
		colnames(Crash..) <- capitalize(tolower(colnames(Crash..)))
		#Chang ename of some crash attributes to conform to later years
		##########################
		if(yr < 2015 & yr < 2019){
			Crash.. <- Crash.. %>% mutate(Func_sys = Road_fnc)
			Load_Person.. <- Load_Person.. %>% mutate(Func_sys = Road_fnc)
		}
		#Race is now(2019 data onward) a separate file that needs to be appended
		if(yr >= 2019){
			#Only use the first race to be consistent with pre-2019 data
			Race.. <- read.csv(paste(Storage_Directory,"/",yr,"/Csvs/RACE.csv", sep="")) %>% filter(ORDER == 1)
			colnames(Race..) <- capitalize(tolower(colnames(Race..)))
			#Append race data 
			Load_Person.. <- left_join(Load_Person.., Race..[,c("St_case","Per_no","Racename")], by = c("St_case","Per_no")) %>% mutate(Race = Racename)
		}
		#Append Crash details to person record
		Person.. <- left_join(Load_Person.., Crash..[,c("St_case","Latitude","Longitud","City","Drunk_dr")], by = "St_case")
		#Select attirbutes of interest
		Person.. <- dplyr::select(Person.., c(State,St_case,Latitude,Longitud,Race,Hispanic,Per_typ,Age, Sex, Inj_sev,County,City,Month,Day,Hour,Func_sys,Drunk_dr))
		
		#Decide using data dictionary
		##############
		if(yr < 2019){
			#Race---
			Person.. <- left_join(Person.. , dplyr::select(filter(Race_dd.., Year == yr),-Year),by = "Race")
		}
		if(yr >= 2019){
			#Race---
			Person.. <- Person.. %>% mutate(Race_desc = Race)
		}
		#Person type
		Person.. <- cbind(left_join(Person.. , dplyr::select(filter(Person_Type_dd.., Year == yr),-Year),by = "Per_typ"), Year = yr)
		#Store
		Master_Person.. <- rbind(Master_Person.., Person..)
		#Print progress
		print(paste("Done Preparing ", yr," Person File",sep=""))
	}


#####################################################################################
#Step 3 - Format data to easily join with Census data based on race, age cohort, etc.
#####################################################################################

	#Make copy
	Person.. <- Master_Person..
	#Convert race flag to hispanic
	Person..$Race_desc[!(Person..$Hispanic%in%c(7,99))] <- "Latino"
	#Condense asian races
	Person..$Race_desc[Person..$Race_desc%in%c("Asian Indian","Asian Or Pacific Islander, No Specific (Individual) Rac","Chinese" ,"Filipino", "Japanese", "Korean","Vietnamese")] <- "Asian"
	#Change names pf 
	Person..$Race_desc[Person..$Race_desc%in%c("Other Asian or Pacific Islander","Samoan")] <- "NHPI"
	#Other
	Person..$Race_desc[Person..$Race_desc%in%c("Multiple Races (Individual Races Not Specified; ex., “M","All Other Races")] <- "Other"
	Person..$Race_desc[Person..$Race_desc%in%c("American Indian (Includes Aleuts and Eskimos)")] <- "AIAN"
	
	#Create age cohorts
	#Do differently depending on FARS vintage
	#1st period of data
	Select_Person_P1.. <-  filter(Person.., Year <=2008)
	Select_Person_P1.. <- mutate(Select_Person_P1.., Age_Cohort =cut(Age, breaks =  c(-1,5,9,14,17,19,24,29,34,44,54,64,74,84,98,999), labels = c("Age_Under5","Age_5_9","Age_10_14", "Age_15_17", "Age_18_19",
		"Age_20_24", "Age_25_29", "Age_30_34", "Age_35_44",  "Age_45_54", "Age_55_64", "Age_65_74","Age_75_84", "Age_Over_84","Unknown")))
	#Second period of data
	Select_Person_P2.. <-  filter(Person.., Year > 2008)
	Select_Person_P2.. <- mutate(Select_Person_P2.., Age_Cohort =cut(Age, breaks = c(-1,5,9,14,17,19,24,29,34,44,54,64,74,84,120,1000), labels = c("Age_Under5","Age_5_9","Age_10_14", "Age_15_17", "Age_18_19",
		"Age_20_24", "Age_25_29", "Age_30_34", "Age_35_44",  "Age_45_54", "Age_55_64", "Age_65_74","Age_75_84", "Age_Over_84","Unknown")))	
	#Recombine
	Person.. <- rbind(Select_Person_P1..,Select_Person_P2..)
	#Prepare mode designation
	Person.. <- Person.. %>% mutate(Mode = case_when(
		Per_type_desc%in%c("Driver of a Motor Vehicle In-Transport","Driver of a Motor Vehicle In-Transport","Passenger of a Motor Vehicle In-Transport","Occupant of a Motor Vehicle Not In-Transport",
		"Unknown Occupant Type in a Motor Vehicle In-Tr") ~ "Motor Vehicle",
		Per_type_desc%in%c("Other Pedestrian (Includes Persons on Personal Conveyances","Pedestrian") ~ "Pedestrian",
		Per_type_desc%in%c("Bicyclist","Other Cyclist") ~ "Bicyclist",
		Per_type_desc%in%c("Person on Personal Conveyances (Since 2007)","Occupant of a Non-Motor Vehicle Transport Devi","Unknown Type of Non-Motorist","Persons In/On Buildings (Since 2007)",
		"Persons In/On Buildings (Since 2007)") ~ "Other"))	
	#Append county 
	####################
	Person.. <- left_join(Person.., Gsa_Codes..[,c("County","County_Name","State")], by = c("County", "State"))

	#Write out file
	###############################
	save(Person.., file = "Data/Person_2000-2019.RData")
	write.csv(Master_Person.., file = "Data/Person_2000-2019.csv",row.names = F)
	
	
	
	
	