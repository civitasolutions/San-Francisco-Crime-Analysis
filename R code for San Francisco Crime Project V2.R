#clearing R environment
rm(list = ls())

#### LIBRARIES ####
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(patchwork) #package to create layout of multiple figures on single page
library(plotly) #package to create interactive graphs
library(jsonlite)
library(lubridate)
library(RColorBrewer) #package used to set color palettes
library(scales) #package used to add commas to text in ggplot
library(sf) # For working with spatial data
library(ggpubr) #used to output table
library(grid) #used for adding text annotation to table

#cleaning dataset
data<-read.csv('/Users/josephrios/Desktop/Data Science Portfolio Projects/SF Crime Data/SF Crime Dataset Updated Daily.csv')
data_cleaned<-data.frame(data[,c(-1,-13,-24)]) #removing three rows not needed
str(data_cleaned) #understanding how variables are read in to inform variable type conversions


#converting time variables into timestamps in R
data_cleaned$time_crime_occurred<-as.POSIXct(data_cleaned$incident_datetime,"%Y-%m-%dT%H:%M:%S",tz="America/Los_Angeles")
data_cleaned$time_crime_reported<-as.POSIXct(data_cleaned$report_datetime,"%Y-%m-%dT%H:%M:%S",tz="America/Los_Angeles")

#extracting year from time when crime was reported to occur
data_cleaned$year_crime_reported<-year(data_cleaned$time_crime_reported)



##############################
# OVERALL CRIME DATA BY YEAR #
##############################

year_crime_reported_filtered<-subset(data_cleaned,year_crime_reported!=2025) #dropping observations for yearly analysis
#### OVERALL CRIME #####
#examining frequencies of when crime was reported to occur by month
overall_counts_year<-table(year_crime_reported_filtered$year_crime_reported)
#extract the levels
levels_year<-names(overall_counts_year)
# Convert counts to a regular vector (it's initially a named vector)
overall_counts_year <- as.vector(as.numeric(overall_counts_year))
crime_by_year<-data.frame(levels_year,overall_counts_year)

#month over month growth rates of crime
growth_rate = (crime_by_year) %>%
  # first sort by year
  arrange(crime_by_year$levels_year) %>%
  mutate(Diff_year = (crime_by_year$overall_counts_year - lag(crime_by_year$overall_counts_year))/ crime_by_year$overall_counts_year*100)  # Difference in time (just in case there are gaps)



###################################################
#   OVERALL CRIME DATA BY MONTH LONGITIDUNAL LINE PLOT      #
###################################################
#extracting month and year combination for plotting
month_by_year_crime_ordered<-data_cleaned$time_crime_reported[order(data_cleaned$time_crime_reported)]
data_cleaned$month_by_year_crime_formatted<-factor(format(month_by_year_crime_ordered,"%Y-%m"))

month_by_year_crime_reported_filtered<-data_cleaned$month_by_year_crime_formatted[data_cleaned$month_by_year_crime_formatted!='2025-02']
month_by_year_crime_reported_final<-droplevels(month_by_year_crime_reported_filtered) #dropped february 2025 because not full month
#month_by_year_crime_reported_ordered<-month_by_year_crime_reported_dropped[order(format(month_by_year_crime_reported_dropped,"%Y-%m" ))] #dropped february 2025 because not full month


#### OVERALL CRIME #####
#examining frequencies of when crime was reported to occur by month
overall_counts_month_year<-table(month_by_year_crime_reported_final)
#extract the levels
levels_month_year<-names(overall_counts_month_year)
# Convert counts to a regular vector (it's initially a named vector)
overall_counts_month_year <- as.vector(as.numeric(overall_counts_month_year))
crime_by_month_year<-data.frame(levels_month_year,overall_counts_month_year)



#descriptives of crimes per month and year
min(crime_by_month_year$overall_counts_month_year) #10,925 crimes per month
max(crime_by_month_year$overall_counts_month_year) #10,925 crimes per month

mean(crime_by_month_year$overall_counts_month_year) #10,925 crimes per month
median(crime_by_month_year$overall_counts_month_year) #11,221 crimes per month
sd(crime_by_month_year$overall_counts_month_year) #1,405 sd crimes per month
mean(crime_by_month_year$overall_counts_month_year)/30 #364 average crimes per day assuming 30 days per mont
median(crime_by_month_year$overall_counts_month_year)/30 #374 median crimes per day assuming 30 days per mont
#month over month growth rates of crime
growth_rate = (crime_by_month_year) %>%
  # first sort by year
  arrange(crime_by_month_year$levels_month_year) %>%
  mutate(Diff_month = (crime_by_month_year$overall_counts_month_year - lag(crime_by_month_year$overall_counts_month_year))/ crime_by_month_year$overall_counts_month_year*100)  # Difference in time (just in case there are gaps)

#creating dataframe for plotting
growth_rate_crime_by_month_year<-data.frame(name=growth_rate[,1],value=growth_rate[,3])


#plotting crimes by month per year
count_crime_plot<-ggplot(crime_by_month_year, aes(x = levels_month_year,y = as.numeric(overall_counts_month_year))) +
  geom_point(size=2.5,color="#00B3C1") + 
  geom_line(size=1,group=1,color="#003542") +
  scale_x_discrete(labels = c("Jan 2018","Feb 2018", "Mar 2018", "Apr 2018","May 2018","Jun 2018","Jul 2018","Aug 2018","Sep 2018","Oct 2018","Nov 2018", "Dec 2018",
                              "Jan 2019","Feb 2019", "Mar 2019", "Apr 2019","May 2019","Jun 2019","Jul 2019","Aug 2019","Sep 2019","Oct 2019","Nov 2019", "Dec 2019",
                              "Jan 2020","Feb 2020", "Mar 2020", "Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020","Sep 2020","Oct 2020","Nov 2020", "Dec 2020",
                              "Jan 2021","Feb 2021", "Mar 2021", "Apr 2021","May 2021","Jun 2021","Jul 2021","Aug 2021","Sep 2021","Oct 2021","Nov 2021", "Dec 2021",
                              "Jan 2022","Feb 2022", "Mar 2022", "Apr 2022","May 2022","Jun 2022","Jul 2022","Aug 2022","Sep 2022","Oct 2022","Nov 2022", "Dec 2022",
                              "Jan 2023","Feb 2023", "Mar 2023", "Apr 2023","May 2023","Jun 2023","Jul 2023","Aug 2023","Sep 2023","Oct 2023","Nov 2023", "Dec 2023",
                              "Jan 2024","Feb 2024", "Mar 2024", "Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024", "Dec 2024",
                              "Jan 2025"
                              )) +
  scale_y_continuous(limits=c(5000,15000),breaks = c(5000, 6000,7000,8000,9000,10000,11000,12000,13000,14000,15000),
                     labels = c("5,000", "6,000","7,000","8,000","9,000","1,000","1,100","1,200","1,300","1,400","1,500")) +
  labs(title = "Number of Crimes Reported in San Francisco by Month and Year",
       x = "Month and Year",
       y = "Number of Crimes Reported",
       caption="Data Source: San Francisco Police Department Incident Report (2018 to Present) Obtained from SFDATA on Feb 14, 2025") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


#plotting growth rates by month per year
growth_crime_plot<-ggplot(growth_rate_crime_by_month_year, aes(x = name,y = as.numeric(value))) +
  geom_point(size=2.5,color="#003542") + 
  geom_line(size=1,group=1,color="#00B3C1") +
  scale_x_discrete(labels = c("Jan 2018","Feb 2018", "Mar 2018", "Apr 2018","May 2018","Jun 2018","Jul 2018","Aug 2018","Sep 2018","Oct 2018","Nov 2018", "Dec 2018",
                              "Jan 2019","Feb 2019", "Mar 2019", "Apr 2019","May 2019","Jun 2019","Jul 2019","Aug 2019","Sep 2019","Oct 2019","Nov 2019", "Dec 2019",
                              "Jan 2020","Feb 2020", "Mar 2020", "Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020","Sep 2020","Oct 2020","Nov 2020", "Dec 2020",
                              "Jan 2021","Feb 2021", "Mar 2021", "Apr 2021","May 2021","Jun 2021","Jul 2021","Aug 2021","Sep 2021","Oct 2021","Nov 2021", "Dec 2021",
                              "Jan 2022","Feb 2022", "Mar 2022", "Apr 2022","May 2022","Jun 2022","Jul 2022","Aug 2022","Sep 2022","Oct 2022","Nov 2022", "Dec 2022",
                              "Jan 2023","Feb 2023", "Mar 2023", "Apr 2023","May 2023","Jun 2023","Jul 2023","Aug 2023","Sep 2023","Oct 2023","Nov 2023", "Dec 2023",
                              "Jan 2024","Feb 2024", "Mar 2024", "Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024", "Dec 2024",
                              "Jan 2025"
  )) +
  scale_y_continuous(limits=c(-25,25),breaks = c(-25,-20, -15,-10,-5,0,5,10,15,20,25),
                     labels = c("-25","-20", "-15","-10","-5","0","5","10","15","20","25")) +
  labs(title = "Growth Rate of Crimes Reported in San Francisco by Month and Year",
       x = "Month and Year",
         y = "Growth Rate",
       caption="Data Source: San Francisco Police Department Incident Report (2018 to Present) Obtained from SFDATA on Feb 14, 2025") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


count_crime_plot/growth_crime_plot



######################################################################################
#  MANIPULATING CRIME DATE FOR CREATING LONGTIDUNAL LINE PLOT BY CRIME SUBCATEGORY   #
######################################################################################

 #### CRIME SUBCATGORIES #####

# STEP 1: dummy code crime type subcategories
#categorizing violent and property crimes with dummy variables
data_cleaned$violent_crime<-ifelse(data_cleaned$incident_category=="Homicide" | data_cleaned$incident_category=="Rape" | data_cleaned$incident_category=="Robbery" | data_cleaned$incident_category=="Assault" | data_cleaned$incident_category=="Sex Offense" | data_cleaned$incident_category=="Human Trafficking (A), Commercial Sex Acts" | data_cleaned$incident_category=="Human Trafficking (B), Involuntary Servitude"| data_cleaned$incident_category=="Human Trafficking, Commercial Sex Acts",1,NA)
data_cleaned$property_crime<-ifelse(data_cleaned$incident_category=="Burglary" | data_cleaned$incident_category=="Larceny" | data_cleaned$incident_category=="Motor Vehicle Theft" | data_cleaned$incident_category=="Arson" | data_cleaned$incident_category=="Theft" | data_cleaned$incident_category=="Stolen Property" | data_cleaned$incident_category=="Motor Vehicle Theft?"| data_cleaned$incident_category=="Vandalism",1,NA)
data_cleaned$public_order_crime<-ifelse(data_cleaned$incident_category=="Disorderly Conduct" | data_cleaned$incident_category=="Gambling" | data_cleaned$incident_category=="Liquor Laws" | data_cleaned$incident_category=="Prostitution" | data_cleaned$incident_category=="Weapons Carrying Etc" | data_cleaned$incident_category=="Weapons Offence" | data_cleaned$incident_category=="Weapons Offence",1,NA)
data_cleaned$drug_offense_crime<-ifelse(data_cleaned$incident_category=="Drug Offense" | data_cleaned$incident_category=="Drug Violation",1,NA)

#adding crime type variable
data_cleaned<-data_cleaned %>% mutate(crime_type = case_when(
  violent_crime==1 ~ "Violent Crime",
  property_crime==1 ~ "Property Crime",
  public_order_crime==1 ~ "Public Order Crime",
  drug_offense_crime==1 ~ "Drug Offense Crime",
  TRUE ~ "Other"
))




#STEP 2: calculating annual growth rates by crime
aa1<-table(data_cleaned$year_crime_reported,data_cleaned$violent_crime) #removing 2025 data because it is not a full year and would skew results
levels_year<-as.vector(names(aa1[,1]))
violent_counts_year<-as.vector(as.numeric(table(data_cleaned$year_crime_reported,data_cleaned$violent_crime))) #removing 2025 data because it is not a full year and would skew results
property_counts_year<-as.vector(as.numeric(table(data_cleaned$year_crime_reported,data_cleaned$property_crime))) #removing 2025 data because it is not a full year and would skew results
public_order_counts_year<-as.vector(as.numeric(table(data_cleaned$year_crime_reported,data_cleaned$public_order_crime))) #removing 2025 data because it is not a full year and would skew results
drug_offense_counts_year<-as.vector(as.numeric(table(data_cleaned$year_crime_reported,data_cleaned$drug_offense_crime))) #removing 2025 data because it is not a full year and would skew results

#creating dataframe for lag calculations
crime_type_by_year<-data.frame(levels_year,violent_counts_year,property_counts_year,public_order_counts_year,drug_offense_counts_year)
crime_type_by_year<-crime_type_by_year [-8,] #dropping feb 2025 because it's an incomplete month 

growth_rate_year = (crime_type_by_year) %>%
  # first sort by year
  arrange(crime_type_by_year$levels_year) %>%
  mutate(Diff_year_violent = paste0(round((crime_type_by_year$violent_counts_year - lag(crime_type_by_year$violent_counts_year))/ crime_type_by_year$violent_counts_year*100,2),"%"),
         Diff_year_property = paste0(round((crime_type_by_year$property_counts_year - lag(crime_type_by_year$property_counts_year))/ crime_type_by_year$property_counts_year*100,2),"%"),
         Diff_year_public_order = paste0(round((crime_type_by_year$public_order_counts_year - lag(crime_type_by_year$public_order_counts_year))/ crime_type_by_year$public_order_counts_year*100,2),"%"),
         Diff_year_drug_offense = paste0(round((crime_type_by_year$drug_offense_counts_year - lag(crime_type_by_year$drug_offense_counts_year))/ crime_type_by_year$drug_offense_counts_year*100,2),"%"),
         )  # Difference in time (just in case there are gaps)
#creating table
growth_type_table<-growth_rate_year[,c(1,6,7,8,9)]
colnames(growth_type_table)<-c("Year","Violent Crime","Property Crime",
                               "Public Order Crime","Drug Offense Crime")

table_annual_growth_crime_type <- ggtexttable(growth_type_table[-1,], 
                     rows = NULL) 


table_annual_growth_crime_type<-table_annual_growth_crime_type %>%
  tab_add_title(text = "Annual Growth Rate by Crime Type", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Year 2018 is not included as it was the base year.", size = 10)

print(table_annual_growth_crime_type)



# Print the table
print(table_annual_growth_crime_type)





# STEP 3: create frequencies based on month year of crime
aa<-table(data_cleaned$month_by_year_crime_formatted,data_cleaned$violent_crime) #removing 2025 data because it is not a full year and would skew results
levels_month_year1<-as.vector(names(aa[,1]))
violent_counts_month_year<-as.vector(as.numeric(table(data_cleaned$month_by_year_crime_formatted,data_cleaned$violent_crime))) #removing 2025 data because it is not a full year and would skew results
property_counts_month_year<-as.vector(as.numeric(table(data_cleaned$month_by_year_crime_formatted,data_cleaned$property_crime))) #removing 2025 data because it is not a full year and would skew results
public_order_counts_month_year<-as.vector(as.numeric(table(data_cleaned$month_by_year_crime_formatted,data_cleaned$public_order_crime))) #removing 2025 data because it is not a full year and would skew results
drug_offense_counts_month_year<-as.vector(as.numeric(table(data_cleaned$month_by_year_crime_formatted,data_cleaned$drug_offense_crime))) #removing 2025 data because it is not a full year and would skew results

# STEP 4: creating dataframe for plotting in ggplot
#creating dataframe for plotting in ggplot
#extract the levels
crime_type_by_month_year<-data.frame(levels_month_year1,violent_counts_month_year,property_counts_month_year,public_order_counts_month_year,drug_offense_counts_month_year)
crime_type_by_month_year<-crime_type_by_month_year [-86,] #dropping feb 2025 because it's an incomplete month 



ggplot() +
  
  geom_point(data=crime_type_by_month_year, aes(x = levels_month_year1,y = violent_counts_month_year),size=2.5,shape=8,color="#003542") + 
geom_point(data=crime_type_by_month_year, aes(x = levels_month_year1,y = property_counts_month_year),size=2.5,shape=15,color="#00b3c1") + 
  geom_point(data=crime_type_by_month_year, aes(x = levels_month_year1,y = public_order_counts_month_year),size=2.5,shape=16,color="grey") + 
  geom_point(data=crime_type_by_month_year, aes(x = levels_month_year1,y = drug_offense_counts_month_year),size=2.5,shape=17,color="#da4554") + 
  geom_line(data=crime_type_by_month_year, aes(x = levels_month_year1,y = violent_counts_month_year),size=.5,linetype="solid",color="#003542",group=1) +
geom_line(data=crime_type_by_month_year, aes(x = levels_month_year1, y = property_counts_month_year),size=.5,linetype="solid",color="#00b3c1",group=1) +
  geom_line(data=crime_type_by_month_year, aes(x = levels_month_year1,y = public_order_counts_month_year),size=.5,linetype="longdash",color="grey",group=1) +
  geom_line(data=crime_type_by_month_year, aes(x = levels_month_year1,y = drug_offense_counts_month_year),size=.5,linetype="twodash",color="#da4554",group=1) +

  scale_x_discrete(labels = c("Jan 2018","Feb 2018", "Mar 2018", "Apr 2018","May 2018","Jun 2018","Jul 2018","Aug 2018","Sep 2018","Oct 2018","Nov 2018", "Dec 2018",
                              "Jan 2019","Feb 2019", "Mar 2019", "Apr 2019","May 2019","Jun 2019","Jul 2019","Aug 2019","Sep 2019","Oct 2019","Nov 2019", "Dec 2019",
                              "Jan 2020","Feb 2020", "Mar 2020", "Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020","Sep 2020","Oct 2020","Nov 2020", "Dec 2020",
                              "Jan 2021","Feb 2021", "Mar 2021", "Apr 2021","May 2021","Jun 2021","Jul 2021","Aug 2021","Sep 2021","Oct 2021","Nov 2021", "Dec 2021",
                              "Jan 2022","Feb 2022", "Mar 2022", "Apr 2022","May 2022","Jun 2022","Jul 2022","Aug 2022","Sep 2022","Oct 2022","Nov 2022", "Dec 2022",
                              "Jan 2023","Feb 2023", "Mar 2023", "Apr 2023","May 2023","Jun 2023","Jul 2023","Aug 2023","Sep 2023","Oct 2023","Nov 2023", "Dec 2023",
                              "Jan 2024","Feb 2024", "Mar 2024", "Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024", "Dec 2024",
                              "Jan 2025" )) +
  scale_y_continuous(limits=c(0,2200),breaks = c(0,100,200,300,400,500, 600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200),
                     labels = c("0","100","200","300","400","500", "600","700","800","900","1,000","1,100","1,200","1,300","1,400","1,500","1,600","1,700","1,800","1,900","2,000","2,100","2,200")) + 
  labs(title = "Type of Crimes Reported in San Francisco by Month Year, and Type",
       x = "Month and Year",
       y = "Number of Crimes Reported",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold")) + #modifying main title by bolding and centering
  #adding manual legend
  annotate("point", x = 76, y = 1950, color = "#003542", shape=8,size = 3) +  # Legend "key" for group A
  annotate("text", x = 77, y = 1950, label = "Violence", hjust = 0) + # Legend label
  annotate("point", x = 76, y = 2050, color = "#00b3c1", shape=15,size = 3) +   # Legend "key" for group B
  annotate("text", x = 77, y = 2050, label = "Property", hjust = 0)  + # Legend label
  annotate("point", x = 76, y = 1750, color = "grey", shape=16,size = 3) +   # Legend "key" for group B
  annotate("text", x = 77, y = 1750, label = "Public Order", hjust = 0)  + # Legend label
  annotate("point", x = 76, y = 1850, color = "#da4554", shape=17,size = 3) +   # Legend "key" for group B
  annotate("text", x = 77, y = 1850, label = "Drugs", hjust = 0)  + # Legend label
  # Add a "legend title"
  annotate("text", x = 75, y = 2150, label = "Crime Type", fontface = "bold", hjust = 0)
  
  
  


###############################################################
#     DIFFERENCES IN CRIME BY TIME, DAY OF WEEK, AND MONTH    #
###############################################################

#extracting month from time when crime was reported to occur
data_cleaned$month_crime_reported<-month(data_cleaned$time_crime_reported,label=TRUE)


table_month_crime<-table(data_cleaned$month_crime_reported)
#extract the levels
levels_month<-factor(names(table_month_crime),levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# Convert counts to a regular vector (it's initially a named vector)
month_crime <- as.vector(as.numeric(table_month_crime))
#dataframe for plotting
crime_by_month<-data.frame(levels_month,month_crime)

month_plot<-ggplot(data = crime_by_month, aes(x = levels_month,y=month_crime)) + # Need to put data in a dataframe
  geom_col(color="#003542",fill="#00b3c1") +
  geom_text(aes(label=comma(month_crime)),hjust=.5,vjust=1.5,size=3)+
  scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  scale_y_continuous(limits=c(0,100000),breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000),
                     labels = c("0","10,000","20,000","30,000","40,000","50,000","60,000","70,000","80,000","90,000","100,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Month",
       x = "Month",
       y = "Number of Crimes Reported",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))




##### CREATING HISTOGRAM FOR CRIME REPORTED BY DAY OF WEEK #####

#extracting day of week from time when crime was reported to occur
data_cleaned$day_crime_reported<-wday(data_cleaned$time_crime_reported,label=TRUE)
#examining frequencies of when crime was reported to occur by year
table_day_crime<-table(data_cleaned$day_crime_reported)
#extract the levels
levels_days<-factor(names(table_day_crime),levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
# Convert counts to a regular vector (it's initially a named vector)
day_crime <- as.vector(as.numeric(table_day_crime))
#dataframe for plotting
crime_by_day<-data.frame(levels_days,day_crime)

day_plot<-ggplot(data = crime_by_day, aes(x = levels_days,y=day_crime)) + # Need to put data in a dataframe
  geom_col(fill="#da4554",color="#003542") +
  geom_text(aes(label=comma(day_crime)),hjust=.5,vjust=1.5,size=3,color="white")+
  scale_x_discrete(labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) +
  scale_y_continuous(limits=c(0,150000),breaks = c(0,25000,50000,75000,100000,125000,150000),
                     labels = c("0","25,000","50,000","75,000","100,000","125,000","150,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Day of Week ",
       x = "Day of Week",
       y = "Number of Crimes Reported",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))





  ##### CREATING HISTOGRAM FOR CRIME REPORTED BY HOUR OF DAY #####
#hour of day time reported
data_cleaned$hour_crime_reported<-hour(data_cleaned$time_crime_reported)

#examining frequencies of when crime was reported to occur by year
table_hour_day_crime<-table(data_cleaned$hour_crime_reported)
#extract the levels
levels_hours<-factor(names(table_hour_day_crime),levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
# Convert counts to a regular vector (it's initially a named vector)
hour_day_crime <- as.vector(as.numeric(table_hour_day_crime))

crime_by_hour_day<-data.frame(levels_hours,hour_day_crime)


hour_plot<-ggplot(data = crime_by_hour_day, aes(x = levels_hours,y=hour_day_crime)) + # Need to put data in a dataframe
  geom_col(color="#003542",fill="#00b3c1") +
  geom_text(aes(label=comma(hour_day_crime)),hjust=.5,vjust=1.5,size=2.5,color="white")+
  scale_x_discrete(labels = c("00:00 - 00:59","01:00 - 01:59","02:00 - 02:59","03:00 - 03:59","04:00 - 04:59","05:00 - 05:59","06:00 - 06:59","07:00 - 07:59","08:00 - 08:59","09:00 - 09:59","10:00 - 10:59","11:00 - 11:59", 
                              "12:00 - 12:59","13:00 - 13:59","14:00 - 14:59","15:00 - 15:59","16:00 - 16:59","17:00 - 17:59","18:00 - 18:59","19:00 - 19:59","20:00 - 20:59","21:00 - 21:59","22:00 - 22:59","23:00 - 23:59")) +
  scale_y_continuous(limits=c(0,65000),breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000),
                     labels = c("0","5,000","10,000","15,000","20,000","25,000","30,000","35,000","40,000","45,000","50,000","55,000","60,000","65,000","70,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Hour of Day",
       x = "Hour of Day",
       y = "Number of Crimes Reported",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))


#Combining multiple figures into a single figure
#guides='collect' allows for the reduction of multiple legends that are the same
patch_graph<-hour_plot+day_plot+month_plot
  
  
patch_graph 


##########
# CREATING STACKED BAR CHARTS OF TIME BY CRIME TYPE

#calculating aggregate crime counts by hour and crime type
hour_crime_type_counts <- data_cleaned %>%
  group_by(hour_crime_reported,crime_type) %>%
  summarize(
    total_crime_hour_crime_type=n())

#dropping "other" category
hour_crime_type_counts_filtered <- hour_crime_type_counts %>% filter(crime_type != "Other")
print(hour_crime_type_counts_filtered)

# Reorder Subcategories
hour_crime_type_counts_filtered$crime_type <- factor(hour_crime_type_counts_filtered$crime_type, levels = c("Property Crime","Violent Crime","Drug Offense Crime","Public Order Crime")) # Desired order

# Basic Stacked Bar Chart
ggplot(hour_crime_type_counts_filtered, aes(x = factor(hour_crime_reported,levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")), y = total_crime_hour_crime_type, fill = crime_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#003542","#da4554","#00b3c1","grey")) + # stat = "identity" is crucial for pre-calculated values
  scale_x_discrete(labels = c("00:00 - 00:59","01:00 - 01:59","02:00 - 02:59","03:00 - 03:59","04:00 - 04:59","05:00 - 05:59","06:00 - 06:59","07:00 - 07:59","08:00 - 08:59","09:00 - 09:59","10:00 - 10:59","11:00 - 11:59", 
                              "12:00 - 12:59","13:00 - 13:59","14:00 - 14:59","15:00 - 15:59","16:00 - 16:59","17:00 - 17:59","18:00 - 18:59","19:00 - 19:59","20:00 - 20:59","21:00 - 21:59","22:00 - 22:59","23:00 - 23:59")) +
  scale_y_continuous(limits=c(0,16000),breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000),
                     labels = c("0","1,000","2,000","3,000","4,000","5,000","6,000","7,000","8,000","9,000","10,000","11,000","12,000","13,000","14,000","15,000","16,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Hour of Day and Type of Crime",
       x = "Hour of Day",
       y = "Number of Crimes Reported",
       fill = "Crime Type",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))







#calculating aggregate crime counts by day and crime type
day_crime_type_counts <- data_cleaned %>%
  group_by(day_crime_reported,crime_type) %>%
  summarize(
    total_crime_day_crime_type=n())

#dropping "other" category
day_crime_type_counts_filtered <- day_crime_type_counts %>% filter(crime_type != "Other")
print(day_crime_type_counts_filtered)

# Reorder Subcategories
day_crime_type_counts_filtered$crime_type <- factor(day_crime_type_counts_filtered$crime_type, levels = c("Property Crime","Violent Crime","Drug Offense Crime","Public Order Crime")) # Desired order

ggplot(day_crime_type_counts_filtered, aes(x = factor(day_crime_reported,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")), y = total_crime_day_crime_type, fill = crime_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#003542","#da4554","#00b3c1","grey")) + # stat = "identity" is crucial for pre-calculated values
  scale_x_discrete(labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) +
  scale_y_continuous(limits=c(0,40000),breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000),
                     labels = c("0","5,000","10,000","15.000","20,000","25,000","30,000","35,000","40,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Day of Week and Crime Type ",
       x = "Day of Week",
       y = "Number of Crimes Reported",
       fill = "Crime Type",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))


#calculating aggregate crime counts by month and crime type
month_crime_type_counts <- data_cleaned %>%
  group_by(month_crime_reported,crime_type) %>%
  summarize(
    total_crime_month_crime_type=n())

#dropping "other" category
month_crime_type_counts_filtered <- month_crime_type_counts %>% filter(crime_type != "Other")
print(month_crime_type_counts_filtered)

# Reorder Subcategories
month_crime_type_counts_filtered$crime_type <- factor(month_crime_type_counts_filtered$crime_type, levels = c("Property Crime","Violent Crime","Drug Offense Crime","Public Order Crime")) # Desired order

ggplot(month_crime_type_counts_filtered, aes(x = factor(month_crime_reported,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")), y = total_crime_month_crime_type, fill = crime_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#003542","#da4554","#00b3c1","grey")) + # stat = "identity" is crucial for pre-calculated values
  scale_x_discrete(labels = c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  scale_y_continuous(limits=c(0,25000),breaks = c(0,5000,10000,15000,20000,25000),
                     labels = c("0","5,000","10,000","15,000","20,000","25,000")) + 
  labs(title = "Number of Crimes Reported in San Francisco by Month and Crime Type",
       x = "Month",
       y = "Number of Crimes Reported",
       fill = "Crime Type",
       caption="Data Source: San Francisco Police Department Incident Report: 2018 to Present") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold")) 


###################################################
#                                                 #
#    TOP CRIME NEIGHBORHOODS IN SAN FRANCISCO     #
#                                                 #
###################################################

#STEP 1: CALCULATE CRIMES BY NEIGHBORHOOD

#calculating aggregate crime counts by month and crime type
neighborhood_crime_counts <- data_cleaned %>%
  group_by(analysis_neighborhood) %>%
  summarize(
    total_crime=n(),
    violence=sum(violent_crime,na.rm=TRUE),
    property=sum(property_crime,na.rm=TRUE),
    public_order=sum(public_order_crime,na.rm=TRUE),
    drugs=sum(drug_offense_crime,na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_crime_percent=format(round((total_crime/sum(total_crime))*100,2),nsmall=2), #calculates total crime by tract
    violent_crime_percent=format(round((violence/sum(violence))*100,2),nsmall=2),
    property_crime_percent=format(round((property/sum(property))*100,2),nsmall=2),
    public_order_crime_percent=format(round((public_order/sum(public_order))*100,2),nsmall=2),
    drug_offense_crime_percent=format(round((drugs/sum(drugs))*100,2),nsmall=2)
  )

dense_rank(neighborhood_crime_counts$total_crime_percent)

# 2. Rank the groups based on their percent (descending)
ranked_neighborhoods_overall_crime <- neighborhood_crime_counts %>%
  mutate(rank = dense_rank(desc(total_crime_percent))) %>% #descending order
  arrange(rank)

ranked_neighborhoods_violent_crime <- neighborhood_crime_counts %>%
  mutate(rank = dense_rank(desc(violent_crime_percent))) %>% #descending order
  arrange(rank)

ranked_neighborhoods_property_crime <- neighborhood_crime_counts %>%
  mutate(rank = dense_rank(desc(property_crime_percent))) %>% #descending order
  arrange(rank)

ranked_neighborhoods_drug_crime <- neighborhood_crime_counts %>%
  mutate(rank = dense_rank(desc(drug_offense_crime_percent))) %>% #descending order
  arrange(rank)

ranked_neighborhoods_public_order_crime <- neighborhood_crime_counts %>%
  mutate(rank = dense_rank(desc(public_order_crime_percent))) %>% #descending order
  arrange(rank)



# 3. Get the top 5 neighborhoods; using slice function to account for ties
top_5_neighborhoods_overall_crime <- ranked_neighborhoods_overall_crime %>%
  slice(1:5) %>% select(analysis_neighborhood,total_crime_percent, total_crime) %>%
  mutate(total_crime_percent=paste0(total_crime_percent,"%"),total_crime=comma(total_crime)) %>%
  rename('Neighborhood'=analysis_neighborhood,'Overall Crime Percentage in City' = total_crime_percent, 'Number of Crimes'=total_crime) 

top_5_neighborhoods_violent_crime <- ranked_neighborhoods_violent_crime %>%
  slice(1:5) %>% select(analysis_neighborhood,violent_crime_percent,violence) %>%
  mutate(violent_crime_percent=paste0(violent_crime_percent,"%"),violence=comma(violence)) %>%
  rename('Neighborhood'=analysis_neighborhood,'Violent Crime Percentage in City' = violent_crime_percent, 'Number of Violent Crimes'=violence) 


top_5_neighborhoods_property_crime <- ranked_neighborhoods_property_crime %>%
  slice(1:5) %>% select(analysis_neighborhood,property_crime_percent,property) %>%
  mutate(property_crime_percent=paste0(property_crime_percent,"%"),property=comma(property)) %>%
  rename('Neighborhood'=analysis_neighborhood,'Property Crime Percentage in City' = property_crime_percent, 'Number of Property Crimes'=property) 


top_5_neighborhoods_drug_crime <- ranked_neighborhoods_drug_crime %>%
  slice(1:5) %>% select(analysis_neighborhood,drug_offense_crime_percent,drugs) %>%
  mutate(drug_offense_crime_percent=paste0(drug_offense_crime_percent,"%"),drugs=comma(drugs)) %>%
  rename('Neighborhood'=analysis_neighborhood,'Drug Offense Crime Percentage in City' = drug_offense_crime_percent, 'Number of Drug Offense Crimes'=drugs) 


top_5_neighborhoods_public_order_crime <- ranked_neighborhoods_public_order_crime %>%
  slice(1:5) %>% select(analysis_neighborhood,public_order_crime_percent,public_order)  %>%
  mutate(public_order_crime_percent=paste0(public_order_crime_percent,"%"),public_order=comma(public_order)) %>%
  rename('Neighborhood'=analysis_neighborhood,'Public Order Crime Percentage in City' = public_order_crime_percent, 'Number of Public Order Crimes'=public_order) 



#STEP 2: CREATE TABLE TO OUTPUT

table_neighborhood_crime <- ggtexttable(top_5_neighborhoods_overall_crime, 
                                              rows = NULL,
                                        theme = ttheme(padding = unit(c(15, 5), "mm")))  %>%
  tab_add_title(text = "San Francisco Neighborhoods with Highest Overall Crime Rates", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Reflects top five neighborhoods in city from January 1, 2018 to February 15, 2025.", size = 10)

print(table_neighborhood_crime)

table_neighborhood_property_crime <- ggtexttable(top_5_neighborhoods_property_crime, 
                                        rows = NULL,
                                        theme = ttheme(padding = unit(c(15, 5), "mm")))  %>%
  tab_add_title(text = "San Francisco Neighborhoods with Highest Property Crime Rates", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Reflects top five neighborhoods in city from January 1, 2018 to February 15, 2025.", size = 10)

print(table_neighborhood_property_crime)


table_neighborhood_violent_crime <- ggtexttable(top_5_neighborhoods_violent_crime, 
                                                 rows = NULL,
                                                 theme = ttheme(padding = unit(c(15, 5), "mm")))  %>%
  tab_add_title(text = "San Francisco Neighborhoods with Highest Violent Crime Rates", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Reflects top five neighborhoods in city from January 1, 2018 to February 15, 2025.", size = 10)

print(table_neighborhood_violent_crime)

table_neighborhood_drug_offense_crime <- ggtexttable(top_5_neighborhoods_drug_crime, 
                                                rows = NULL,
                                                theme = ttheme(padding = unit(c(15, 5), "mm")))  %>%
  tab_add_title(text = "San Francisco Neighborhoods with Highest Drug Offense Crime Rates", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Reflects top five neighborhoods in city from January 1, 2018 to February 15, 2025.", size = 10)

print(table_neighborhood_drug_offense_crime)

table_neighborhood_public_order_crime <- ggtexttable(top_5_neighborhoods_public_order_crime, 
                                                     rows = NULL,
                                                     theme = ttheme(padding = unit(c(15, 5), "mm")))  %>%
  tab_add_title(text = "San Francisco Neighborhoods with Highest Public Order Crime Rates", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Reflects top five neighborhoods in city from January 1, 2018 to February 15, 2025.", size = 10)

print(table_neighborhood_public_order_crime)

