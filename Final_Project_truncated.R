library(ggplot2)
library(viridis)
library(RColorBrewer)

#Given a dataset of Austin land lots, find an ideal site for a new corporate headquarters given a set of parameters from the company.

setwd("/path/to/dataset")
raw_data = read.csv("Austin_Lots.csv")

#For Geo_ID analysis later:
options(scipen = 999)
Geo_data = read.csv("Austin_Lots.csv")


#Get dim of raw data
dim(raw_data)

#remove unnecessary columns
raw_data = subset(raw_data, select = -c(created_by,date_creat,modified_b,date_modif))

dim(raw_data)


#rename vague column names
colnames(raw_data)[names(raw_data) == "land_base_"] = "parcel_id"
colnames(raw_data)[names(raw_data) == "land_base1"] = "land_type"
colnames(raw_data)[names(raw_data) == "objectid"] = "second_id"
colnames(raw_data)[names(raw_data) == "Airpt_dist"] = "airport_dist"
colnames(raw_data)[names(raw_data) == "zoning_o_3"] = "zoning_type"
colnames(raw_data)[names(raw_data) == "zcta5ce10"] = "zip"
colnames(raw_data)[names(raw_data) == "LAND_USE_2"] = "specific_land_desig"
colnames(raw_data)[names(raw_data) == "GENERAL_LA"] = "general_land_desig"
colnames(raw_data)[names(raw_data) == "ExTrail_1m"] = "exist_trail_1m"
colnames(raw_data)[names(raw_data) == "PpTrail_1m"] = "proposed_trail_1m"
colnames(raw_data)[names(raw_data) == "conf"] = "bike_conf"
colnames(raw_data)[names(raw_data) == "TotBdgArea"] = "total_building_area"
colnames(raw_data)[names(raw_data) == "Num_Bldgs"] = "number_of_buildings"
colnames(raw_data)[names(raw_data) == "MaxBdgArea"] = "max_building_area"
colnames(raw_data)[names(raw_data) == "tax_break2"] = "district_tax_break"
colnames(raw_data)[names(raw_data) == "bk_tx_brk"] = "block_tax_break"
colnames(raw_data)[names(raw_data) == "Housing__"] = "housing_opp"
colnames(raw_data)[names(raw_data) == "Education"] = "education_opp"
colnames(raw_data)[names(raw_data) == "Economic__"] = "economic_opp"
colnames(raw_data)[names(raw_data) == "Comprehens"] = "comprehensive_opp"
colnames(raw_data)[names(raw_data) == "Med_HH_Inc"] = "med_house_income_in_zip"
colnames(raw_data)[names(raw_data) == "Med_rent"] = "med_rent_in_zip"
colnames(raw_data)[names(raw_data) == "Med_home"] = "med_home_in_zip"
colnames(raw_data)[names(raw_data) == "Aff_rent_t"] = "aff_rent_in_zip"
colnames(raw_data)[names(raw_data) == "Aff_own_te"] = "aff_home_in_zip"
colnames(raw_data)[names(raw_data) == "Descriptio"] = "description"

colnames(raw_data)

### 
#Dealing with missing values (na, blanks, etc)
###

#Look for missing values
names(raw_data)[sapply(raw_data, anyNA)]

#Check if the data inputters used other common words and signifiers when inputting missing values.
any(raw_data=="")
any(raw_data==" ")
any(raw_data=="-")
any(raw_data=="--")
any(raw_data=="Blank")
any(raw_data=="blank")
any(raw_data=="na")
any(raw_data=="n/a")
any(raw_data=="N/A")


#Store block_id, lot_id, and description for comparison later
block_id = raw_data$block_id
lot_id = raw_data$lot_id
description = raw_data$description

#Convert "" and " " to NA
raw_data[raw_data ==""]= NA
raw_data[raw_data ==" "]= NA

#Recheck columns with NA
names(raw_data)[sapply(raw_data, anyNA)]


#I chose to remove the GEOID column because it had a single value.
#I will keep the blanks and NA's of the columns "block_id"  "lot_id" and "description" because they don't have requirements to be filled.
#For the other columns, I will remove rows that have an NA in those columns, in the dataset.


#Remove GEOID because it has many NA's and does not seem to have much meaningful data in it.
raw_data$GEOID <- NULL

#restore original block_id values
raw_data$block_id = block_id
raw_data$lot_id = lot_id
raw_data$description = description

#Omit NA columns
raw_data = na.omit(raw_data)

#Check if any NA's are left in the dataset
sum(is.na(raw_data))

dim(raw_data)

###
#Data	cleaning.
###

#Look for weird data points in dataset
unique(raw_data$land_type)
table(raw_data$land_type)

cleaned_data = raw_data
cleaned_data$land_type[which(cleaned_data$land_type == "Lot")] = "LOT"
cleaned_data$land_type[which(cleaned_data$land_type =="lott")] = "LOT"
cleaned_data$land_type[which(cleaned_data$land_type =="Parcel")] = "PARCEL"
cleaned_data$land_type[which(cleaned_data$land_type =="PCL")] = "PARCEL"
cleaned_data$land_type[which(cleaned_data$land_type =="Tract")] = "TRACT"

cleaned_data = droplevels(cleaned_data)
unique(cleaned_data$land_type)
table(cleaned_data$land_type)

cleaned_data$district_tax_break = as.numeric(gsub("\\$", "", cleaned_data$district_tax_break))

#Remove duplicate rows and rows w/o metadata
cleaned_data = cleaned_data[!duplicated(cleaned_data),]
cleaned_data$specific_land_desig = NULL

dim(cleaned_data)

###
#Fix column classes
###

sapply(cleaned_data,class)
#Change factors from numerical to factor
cleaned_data$parcel_id = as.factor(cleaned_data$parcel_id)
cleaned_data$second_id = as.factor(cleaned_data$second_id)
cleaned_data$district = as.factor(cleaned_data$district)
cleaned_data$zip = as.factor(cleaned_data$zip)

#Change categorical variables to specifically ordinal
cleaned_data$comprehensive_opp = factor(cleaned_data$comprehensive_opp, levels=c("Very Low","Low", "Moderate", "High", "Very High"), ordered=TRUE)
cleaned_data$economic_opp = factor(cleaned_data$comprehensive_opp, levels=c("Very Low","Low", "Moderate", "High", "Very High"), ordered=TRUE)
cleaned_data$housing_opp = factor(cleaned_data$comprehensive_opp, levels=c("Very Low","Low", "Moderate", "High", "Very High"), ordered=TRUE)
cleaned_data$education_opp = factor(cleaned_data$comprehensive_opp, levels=c("Very Low","Low", "Moderate", "High", "Very High"), ordered=TRUE)

###
#Data Exploration
###

#exploratory analysis of data
sapply(cleaned_data, sd)
sapply(cleaned_data, mean)

#looking at airport and city distance
summary(cleaned_data$airport_dist)
summary(cleaned_data$City_dist)

#Looking at bike columns
table(cleaned_data$bike_conf)
table(cleaned_data$bike_lanes)

#Look at parcels with the largest districk tax break
largest_d_tax = cleaned_data[which(cleaned_data$district_tax_break == max(cleaned_data$district_tax_break)),]

#Look at parcels with the highest comprehensive opportunity score:
highest_comp_opp = cleaned_data[which(cleaned_data$comprehensive_opp == "Very High"),]

#X130 Stats
summary(cleaned_data$X130_dist)

#Initial Observations

#Most parcels are close to the City Center, but relatively far from the airport.
#The majority of parcels have 0 existing urban trails, but most parcels have proposed urban trails in the pipeline.
#The parcels with the largest district tax breaks are reserved for airport and aviation facility remodeling, and are all in the same zip code and district, and are near each other in distance to the airport and city.
#District 14 contains all the parcels with a comprehensive opportunity score of "Very High."
#There are only 2 districts, 14 and 21. Perhaps District may not matter as much in this analysis.
#More parcels have a 0 or 1 score in bike riding comfort than a 2,3 or 4.
#The majority of parcels have bike lanes.
#Most parcels have very low block wide tax breaks.
#On average, parcels are 6 miles from Highway 130. The median distance is around 6 miles as well.


###
#Data Visualization
###


#Bike Lanes
ggplot(data = cleaned_data, aes(x = bike_lanes)) + 
  geom_histogram(color = "white",
                 binwidth = 1) +
       labs(x = "# of Lanes",
            y = "Frequency",
            title = "# of Bike Lanes in Parcels")

#Distance from Highway 130
ggplot(data = cleaned_data, aes(x = X130_dist)) + 
       geom_histogram(aes(y=..density..),
                      color = "white",
                      binwidth = 100) +
       labs(x = "Distance (m)",
            y = "Frequency",
            title = "Distance of Parcel from Highway 130") +
       geom_density(alpha=.5, fill="#ffb6c1") 

#Exist vs Proposed Urban Trails
ggplot(data = cleaned_data, aes(x = exist_trail_1m, y = proposed_trail_1m)) + 
       geom_point(shape = 20,
                  size = 1,
                  alpha = 1) +
       labs(x = "Existing",
            y = "Proposed",
            title = "Existing vs Proposed Urban Trails in Parcels")

#Rent vs Comp_opp
ggplot(data = cleaned_data, aes(y = med_rent_in_zip,x = comprehensive_opp, color = comprehensive_opp)) + 
       geom_boxplot(shape = 1,
                  size = 1,
                  alpha = 1,
                  show.legend = FALSE) +
       labs(x = "Comprehensive Opportunity",
            y = "Median Rent",
            title = "Median Rent for Parcels with Different Levels of Comprehensive Opportunity")


conf_vs_lane = ggplot(data = cleaned_data, y = bike_conf, x = bike_lanes,
                   aes(y = bike_conf, 
                       x = bike_lanes,
                       color = comprehensive_opp
                       )) 

conf_vs_lane + geom_point() +
  labs(x ="Bike Comfort rating of parcel",
       y = "# of Bike Lanes in parcel",
       title= "Bike Comfort vs Bike Lanes") +
  labs(color = "Bike Comfort Rating")


# Looking at comprehensive opportunity vs  city center distance
citydist_v_comp = ggplot(data = cleaned_data, x = comprehensive_opp, y = City_dist,
                   aes(x = comprehensive_opp, 
                       y = City_dist,
                       color = comprehensive_opp)) 

citydist_v_comp + geom_boxplot() +
  ggtitle(labs(title= "City distance vs. Comprehensive Opportunity")) +
  xlab("Distance from City Center") +
  ylab( "Comprehensive Opportunity") +
  labs(color = "Distance from City Center")

#Look at relationship between existing urban trails and proposed urban trails
exist_v_proposed = ggplot(data = cleaned_data,
                   aes(x = exist_trail_1m, 
                       y = proposed_trail_1m,
                       color = med_house_income_in_zip,
                       shape = district))

exist_v_proposed + geom_point() +
  ggtitle(labs(title= "Existing vs Proposed Urban Trails")) +
  xlab("Existing Urban Trails") +
  ylab( "Proposed Urban Trails") +
  labs(color = "Median Household Income in Zip Code")

#Clean strings for word parsers
cleaned_data$description = tolower(cleaned_data$description)

stop_words = c("a", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or","other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "to", "too", "was", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your", " ")

unique_words = function(column) {
  #use: get all unique words in a character column of a data frame
  #input: character data frame
  #Output: unique words found in that data frame
  words = c()
  for (item in column){
    split_value = unlist(unique(strsplit(item, " ")))
    words = c(words,split_value)
  }
  
  return(unique(words))
}

all_words = unique_words(cleaned_data$description)

filtered_words = setdiff(all_words,stop_words)
filtered_words[1:10]

#Get top 10 most frequent non stop words
words = c()
for (item in cleaned_data$description) {
    split_value = unlist(strsplit(item, " "))
    split_value = setdiff(split_value,stop_words)
    words = c(words,split_value)
}

Freq_table = as.data.frame(sort(table(words)))
tail(Freq_table,n = 12)

###
#Site Selection
###

#Remove	any	parcels	that	are	not in	the	metro	bus	service	area.
filtered_data = subset(cleaned_data, Bus_area == 1)

#Remove	any	parcels	that	have	an	area	under	300	square	meters.
filtered_data = subset(filtered_data, Shape_Area > 300)

#Remove	any	parcels	with	a	residential	zoning	area (use	the	zoning_o_3 column and	the	residential	general	zoning	category). 
filtered_data = subset(filtered_data, !grepl("LA|RR|SF|MF|MH",zoning_type)) 

dim(filtered_data)


###
#Narrowing down options
###
# get zip codes where commercial and recreational parcels are located
enrich_zips = as.vector(unique(filtered_data$zip[which(filtered_data$general_land_desig ==300 |
                                                           filtered_data$general_land_desig == 680 |
                                                           filtered_data$general_land_desig == 710)]
                                   ))

#There were no education parcels.
edu_zips = as.vector(unique(filtered_data$zip[which(filtered_data$general_land_desig == 640)]))
length(edu_zips)

#Screen for parcels with office construction and are near commercial enrichment areas with urban trails and good education

office_near_enrichment = subset(filtered_data, grepl("office", description) & 
                                  (grepl("construct", description) | grepl("create", description)) & 
                                  (zip %in% enrich_zips) & 
                                  (education_opp == "Moderate" | education_opp == "High" | education_opp =="Very                                       High") &
                                  (exist_trail_1m >0) &
                                  (proposed_trail_1m >0) &
                                  (bike_lanes >0) 
)
      
#Screen for  undeveloped or office parcels, good bike comfort, and good education opp, with urban walkways development, and affordable houses
undevelop_or_office = subset(filtered_data, (bike_conf == 0 | bike_conf == 1) &
                (education_opp == "Moderate" | education_opp == "High" | education_opp == "Very High") &
                (bike_lanes > 0) &
                (exist_trail_1m > 0)  &
                (proposed_trail_1m > 0) & 
                (zip %in% enrich_zips)
              ) 
'''
The best parcels were undeveloped or office parcels, that were in the same zip code as commercial areas, had at least one urban walkway and one bike lane, and had decent to great education opportunity, as per the preferences given by GlobalTechSync. I also cared about affordability of houses for tech employees, and ease of access from any major highway. 

I took "employee enrichment to mean commercial and parks, First thing I did was find zip codes that contained commercial, park or cultural services designated parcels.
I also tried looking for zip codes for parcels that were designated as education (640) but there were none in my dataset.

Different paths I took for filtering out parcels:
1) Looked for parcels with nearby office construction that were also near enrichment areas (using the terms "office", "construct", "create"), with bike lanes, urban trailes

2) Looked for parcels that were good for bikes, had urban trails, good education, near enrichment facilities.

I then put the two dataframes together, and looked at these final 780 parcels.
I filtered out parcels that were not in an undeveloped or office designation, and had 26 left.

At this point, all the parcels had at least one urban walkway, and were Moderate, High, or Very High in Education Opportunity. These parcels also were in the same zip codes as enrichment facilities.
The other preferences they had were:
Near a highway
Ability to own a house
Near Office Construction
Tax Breaks
Good Internet

I believe that being near a highway, have decent tax breaks, and being able to own a house are very important, and having nearby offices are important but not as much as the other three criteria. I also looked at number of buildings currently in the parcel already. I do not know how to quantify good internet with this dataset so I will not use it in my rankings.


final_candidates = rbind(office_near_enrichment, undevelop_or_office)
final_candidates = subset(final_candidates, general_land_desig == 900 |general_land_desig == 400 )

Final_FIDS = c(7881,17264,1070,9421,18603,8544,15761,1697,6875,11011)

top_10 = subset(final_candidates, FID %in% Final_FIDS)



Final_FIDS

#For all of these plots, red indicates a parcel in the Top 10.
ggplot(data = filtered_data, aes(x = district_tax_break, y = block_tax_break)) + 
  geom_point(aes(x = district_tax_break, 
                 y = block_tax_break),
                 alpha =.2) +
  geom_point(data = top_10,
             color = "red") + 
  labs(x = "District Tax Breaks",
            y = "Block Tax Breaks",
            title = "Tax Breaks") 

ggplot(data = filtered_data, aes(x = exist_trail_1m, y = proposed_trail_1m)) + 
  geom_point(aes(x = exist_trail_1m, y = proposed_trail_1m),
               alpha =.5) +
  geom_point(data = top_10,
             color = "red") +
  labs(x = "Existing Trails",
            y = "Proposed Trails",
            title = "Nearby Trails",
            color = "Top 10")

ggplot(data = filtered_data, aes(x = EWC_dist)) + 
  geom_density(aes(x = EWC_dist),
               alpha =.2,
               fill = "black") +
  geom_histogram(aes(x = EWC_dist),
               alpha =.2,
               fill = "black",
               binwidth = 2) +  
  geom_histogram(data= top_10,
                 color = "red",
                 binwidth = 1) +
       labs(x = "District Tax Breaks",
            y = "Frequency",
            title = "District Tax Breaks")


ggplot(data = filtered_data, aes(x = education_opp)) +
      geom_bar(data = top_10,
               color= "#FFB6C1") +
       labs(x = "Education Opportunity",
            y = "Frequency",
            title = "Final 10 Education Opportunity")


ggplot(data = filtered_data, aes(x = bike_conf)) +
      geom_bar(data = top_10,
               color= "#FFB6C1") +
       labs(x = "Bike Comfortability Rating",
            y = "Frequency",
            title = "Bike Comfort Ratings of Top 10 Parcels")

ggplot(data = filtered_data, aes(x = zip)) +
      geom_bar(data = top_10,
               color= "#FFB6C1") +
       labs(x = "Zip Code",
            y = "Frequency",
            title = "Zip Codes of Top 10 Parcels")


