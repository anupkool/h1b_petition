#To set the Working Directory user to place local directory here.
setwd("C:/Users/hp/Desktop/ITMD 527/Final Project/h-1b-visa")

# To check the working Directory has been set. 
getwd()

library(ggplot2) # For Data visualization
#install.packages("readr",  repos= 'http://cran.us.r-project.org')
library(readr) # CSV file I/O, e.g. the read_csv function
#install.packages("dplyr", repos= 'http://cran.us.r-project.org')
library(dplyr)

mydata <- read.csv("h1ball.csv", header = T)
head(mydata)


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
install.packages("ggmap",  repos= 'http://cran.us.r-project.org')
library(ggmap)
install.packages("ggrepel",  repos= 'http://cran.us.r-project.org')
library(ggrepel)
install.packages("lazyeval",  repos= 'http://cran.us.r-project.org')
library(lazyeval)

colnames(mydata)

job_filter <- function(df,input_vec) {
  # Function to filter only the rows from dataframe 
  # with Job titles provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # A new column JOB_INPUT_CLASS is created to identify the Job Type
  # If multiple job type inputs match with a single row in the dataframe df, the 
  # output contains them in different rows each with distinct JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(input_vec) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
  }
  return(unique(new_df))
}

  
find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics that I will be using in my data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}


find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics that I will be using in my data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}

plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))

  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
    group_by_(.dots=c(x_feature,fill_feature)) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
      # metrics I will be using in my data analysis   
      summarise(TotalApps = n(),
                CertiApps = sum(certified), 
                Wage = median(PREVAILING_WAGE),
                Share = CertiApps/850))
}

plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
  # Function to plot output
  # Inputs:
  # df            : dataframe output of plot_input()
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # xlabb         : x label
  # ylabb         : y label
  # Output        : ggplot object
  
  # Prevents numbers on plot transforming into scientific notation
  options(scipen = 999)
  
  g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
    geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
    coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
  
  return(g)
}


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}



plot_input <- function(df, x_feature, fill_feature = "YEAR", metric = "TotalApps",filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))

  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
    group_by_(.dots=c(x_feature,fill_feature)) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
      # metrics I will be using in my data analysis   
      summarise(TotalApps = n(),
                CertiApps = sum(certified), 
                Wage = median(PREVAILING_WAGE),
                Share = CertiApps/850))
}


plot_output <- function(df, x_feature,fill_feature = "YEAR",metric, xlabb,ylabb) {  
  # Function to plot output
  # Inputs:
  # df            : dataframe output of plot_input()
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # xlabb         : x label
  # ylabb         : y label
  # Output        : ggplot object
  
  # Prevents numbers on plot transforming into scientific notation
  options(scipen = 999)
  
  g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
    geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
    coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
  
  return(g)
}


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}


# Converting the Year into character 

mydata%>%
mutate(YEAR = as.character(YEAR)) -> mydata

input <- plot_input(mydata, "EMPLOYER_NAME", "YEAR", "TotalApps",filter = TRUE, Ntop = 5)

emp_plot <- plot_output(input, 'EMPLOYER_NAME','YEAR','TotalApps', 'EMPLOYER','TOTAL NO. of APPLICATIONS') + theme(axis.title = element_text(size = rel(1.5)),
          axis.text.y = element_text(size=rel(1)))

emp_plot


map_gen <- function(df,metric,USA,...) {
  
  # Creating Map Dataframe
  df %>%
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    group_by(WORKSITE,lat,lon) %>%
    summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)) -> map_df
   
  # # Lat-Long Limits
  # df %>%
  #   summarise(lat_min = min(lat,na.rm=TRUE),
  #             lat_max = max(lat,na.rm=TRUE),
  #             long_min = min(lon,na.rm=TRUE),
  #             long_max = max(lon,na.rm=TRUE)) -> geo_coord

  # Finding top Locations for metric
  top_locations <- unlist(find_top(df,"WORKSITE",metric, ...))
  
  # First layer    : USA Map
  # Second layer   : geom_point() with point alpha and size varying with metric
  # Third layer    : points mapping to top locations using ggrepel package
  g <- ggplot(USA, aes(x=long, y=lat)) + 
    geom_polygon() + xlab("Longitude (deg)") + ylab("Latitude(deg)") + 
    geom_point(data=map_df, aes_string(x="lon", y="lat", label = "WORKSITE", alpha = metric, size = metric), color="yellow") + 
    geom_label_repel(data=map_df %>% filter(WORKSITE %in% top_locations),aes_string(x="lon", y="lat",label = "WORKSITE"),
                     fontface = 'bold', color = 'black',
                     box.padding = unit(0.0, "lines"),
                     point.padding = unit(1.0, "lines"),
                     segment.color = 'grey50',
                     force = 3) +
    # Zoom into the specific location input
    #coord_map(ylim = c(max(geo_coord$lat_min - 5,23), min(geo_coord$lat_max - 5,50)),xlim=c(max(geo_coord$long_min - 5,-130),min(geo_coord$long_max + 5,-65))) +
    # Using the whole USA map
    coord_map(ylim = c(23,50),xlim=c(-130,-65)) +
    get_theme()
  
  return(g)
}


USA = map_data(map = "usa")

map <- map_gen(job_filter(mydata,job_list),"TotalApps",USA,Ntop = 3)

map


# Common Employer
employer <- plot_output(input, 'EMPLOYER_NAME','YEAR','Share', 'EMPLOYER','% SHARE') +  theme(axis.title = element_text(size = rel(1.5)),
          axis.text.y = element_text(size=rel(0.5)))

employer

# Finding the top 5 employers with the most petitions
top_employers <- unlist(find_top(mydata,"EMPLOYER_NAME","TotalApps",Ntop = 5))

#Finding the most common Job Titles with Full-Time Positions
mydata %>%
  filter(EMPLOYER_NAME %in% top_employers & FULL_TIME_POSITION == 'Y') %>%
  group_by(JOB_TITLE) %>%
  summarise(COUNT = n()) %>%
  arrange(desc(COUNT)) -> common_jobs

common <- ggplot(common_jobs[1:15,], aes(x=reorder(JOB_TITLE,COUNT),y=COUNT)) +
   geom_bar(stat = "identity", fill = "blue") + coord_flip() +
   xlab("JOB TITLE") + ylab("TOTAL NO. OF APPLICATIONS") + get_theme() 

common

mydata %>%
  filter(EMPLOYER_NAME %in% top_employers, JOB_TITLE %in% unlist(common_jobs$JOB_TITLE[1:20])) %>%
  group_by(JOB_TITLE) -> job_wages_df

wage <- ggplot(job_wages_df, aes(x=reorder(JOB_TITLE,PREVAILING_WAGE,median),y=PREVAILING_WAGE)) +
   geom_boxplot(fill="green") + xlab("JOB TITLE") + ylab("WAGE (USD)") +
  get_theme() + coord_flip(ylim=c(0,150000)) 

wage

split_first <- function(word, split = " ") {
  # Function to obtain first value in a  strsplit
  # Inputs:
  # word      : word to be split
  # split     : split parameter to be passed to strsplit
  return(strsplit(word,split= split)[[1]][1])
}


split_first <- function(word, split = " ") {
  # Function to obtain first value in a  strsplit
  # Inputs:
  # word      : word to be split
  # split     : split parameter to be passed to strsplit
  return(strsplit(word,split= split)[[1]][1])
}

mydata$EMPLOYER_COMPACT = sapply(mydata$EMPLOYER_NAME,split_first,split = " ")

job_list <- c("Programmer","Computer","Software","Systems","Developer")

employer_list <- toupper(c("IBM","Infosys","Wipro","Tata","Deloitte","Amazon","Google","Microsoft","Facebook"))

job_filter(mydata,job_list) %>%
  filter(EMPLOYER_COMPACT %in% employer_list) %>%
  group_by(EMPLOYER_COMPACT) -> employer_df

mostemp <- ggplot(employer_df, aes(x=reorder(EMPLOYER_COMPACT,PREVAILING_WAGE,median),y= PREVAILING_WAGE)) +
  geom_boxplot(fill="green") + xlab("EMPLOYER") + ylab("WAGE (USD)") +
  get_theme() + coord_flip(ylim=c(0,150000))

mostemp

job_list <- c("Data Scientist")

data_science_df <- plot_input(job_filter(mydata,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")

scientist <- plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")
            
scientist


sciwage <- ggplot(job_filter(mydata,job_list), aes(x=JOB_INPUT_CLASS,y= PREVAILING_WAGE)) +
  geom_boxplot(aes(fill=YEAR)) + xlab("JOB TITLE") + ylab("WAGE (USD)") +
  get_theme() + coord_cartesian(ylim=c(25000,200000))

sciwage
 
mydata %>%
mutate(SOC_NAME = toupper(SOC_NAME)) -> mydata

job_filter(mydata,job_list) %>%
filter(!is.na(SOC_NAME)) %>%
group_by(SOC_NAME) %>%
summarise(TotalApps = n(), Wage = median(PREVAILING_WAGE)) %>%
filter(TotalApps > 10) %>% 
arrange(desc(Wage))

data_science_soc_df <- plot_input(job_filter(mydata,job_list),
                              "SOC_NAME",
                              "YEAR",
                               filter = TRUE,
                               Ntop = 10)


plot_output(data_science_soc_df, "SOC_NAME","YEAR", "TotalApps", "INDUSTRY", "NO. OF APPLICATIONS")


plot_output(data_science_soc_df, "SOC_NAME","YEAR", "Wage", "INDUSTRY", "WAGE (USD)")



# set colors
mycolors <- c("#FF7F11","#058C42","#FF3F00","#5D2E8C","#590925","#581908","#B80C09",
              "#276FBF","#337357","#B6D7B9","#8338EC","#0F4C5C","#FB8B24","#E16036",
              "#420039","#7A8B99","#8DB580","#00B295","#502419","#BB7E5D")

case.status <- as.data.frame(mydata %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>%
                        summarise(PROPORTION = round(n()*100/nrow(mydata),1)))

ggplot(data = case.status, aes(x = reorder(CASE_STATUS, PROPORTION), 
                               y = PROPORTION, fill = CASE_STATUS)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(PROPORTION,"%")), hjust = 1) + 
    labs(x = "Case Status", y = "Percent", title = "Status of petitioned applications") + 
    scale_fill_manual(values = mycolors) +
    scale_y_continuous(breaks = seq(0,100,10)) +
    coord_flip()


mydata$WORKSITE <- factor(mydata$WORKSITE)
top_employer <- as.data.frame(mydata %>% group_by(WORKSITE) %>%
                summarise(count = n(), percent = round(count*100/nrow(mydata),1)) %>% 
                arrange(desc(count))%>% 
                top_n(15, wt = count))

ggplot(data = top_employer, aes(x = reorder(WORKSITE, percent),
                                y = percent, fill = WORKSITE)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
    labs(x = "WORKSITE", y = "Petitions made(in percentage)") + 
    scale_y_continuous(breaks = seq(0,7,1)) +
    scale_fill_manual(values = mycolors) +
    theme(legend.position = "none") +
    coord_flip()


denied_jobs <- mydata %>% filter(CASE_STATUS == "DENIED") %>%
    group_by(JOB_TITLE) %>% summarise(JOBS_DENIED_COUNT = n()) %>%
    arrange(desc(JOBS_DENIED_COUNT)) %>% top_n(7)
denied_jobs


accepted_jobs <- mydata %>% filter(CASE_STATUS == "CERTIFIED") %>%
    group_by(JOB_TITLE) %>% summarise(JOBS_CERTIFIED_COUNT = n()) %>%
    arrange(desc(JOBS_CERTIFIED_COUNT)) %>% top_n(7)
accepted_jobs