#Asher Stout 300432820
#INFO281 Project

library(tidyverse)




# --------LOAD AND FORMAT DATA RELATED TO TRADE BALANCE--------
# Formats the passed economic data into an acceptable format for the purposes of this project
# @param toFormat the data to format
# @return the formatted data, with irrelevant columns discarded
format.project.econdata <- function(toFormat) {
  if (!is.data.frame(toFormat)) {
    stop("Unexpected Argument Format.", call. = T)
  }
  formatted = subset(
    toFormat,
    select = c(
      "Reporter_code",
      "Reporter_desc",
      "Flow_desc",
      "Indicator_desc",
      "Unit_desc",
      "Year",
      "Value"
    )
  )
}


#Declare constants for economic file loading
address <-
  "C:/Users/Asher (GOD)/Desktop/VUW/2019_tri_3/INFO281/project_material/data/"
econfiles <-
  c(
    "Developing_Nations IM-EX[GOOD] (2008-2017).csv",
    "Developing_Nations IM-EX[GOOD-TYPE] (2008-2017).csv",
    "Developing_Nations IM-EX[SERVICE] (2008-2017).csv"
  )


#Load in and format raw economic data recursively
econ_iterator <- 1L
while (econ_iterator <= length(econfiles))
{
  econfile <-
    read_csv(paste(address, econfiles[econ_iterator], sep = ""))
  econfile <- format.project.econdata(econfile)
  if (econ_iterator == 1L) {
    econdata <- econfile
  }
  else{
    econdata <- full_join(econdata, econfile)
  }
  econ_iterator = econ_iterator + 1
}
econdata <- spread(econdata, Indicator_desc, Value)
econdata <-
  econdata[!(econdata$Reporter_desc == "Kiribati"), ] #remove Kiribati due to lack of relevant inequality data


#Rename columns to better reflect data
colnames(econdata) <-
  c(
    "Country_code",
    "Country",
    "Trade_flow",
    "Units",
    "Year",
    "Agricultural_products_(GOOD)",
    "Commercial_services_(SERVICE)",
    "Fuels/Mining_products_(GOOD)",
    "Good-related_services_(SERVICE)",
    "Manufactured_products_(GOOD)",
    "Other_commercial_services_(SERVICE)",
    "Total_products_(GOOD)",
    "Transport_services_(SERVICE)",
    "Travel_service_(SERVICE)"
  )




# --------LOAD, FORMAT, AND EXPORT DATA RELATED TO INEQUALITY--------
#Load in inqeuality data
inqdata <-
  read_csv(
    paste(
      address,
      "Developing_Nations Inequality[POVERTY] (2008-2017).csv",
      sep = ""
    ),
    col_types = "ccccdddddddddd"
  )


#Format data, removing irrelevant columns and null rows
inqdata <-
  gather(
    inqdata,
    "2008 [YR2008]",
    "2009 [YR2009]",
    "2010 [YR2010]",
    "2011 [YR2011]",
    "2012 [YR2012]",
    "2013 [YR2013]",
    "2014 [YR2014]",
    "2015 [YR2015]",
    "2016 [YR2016]",
    "2017 [YR2017]",
    key = "Year",
    value = "Index"
  )
inqdata <-
  separate(inqdata,
           Year,
           sep = " ",
           into = c("Year", "year-code"))
inqdata <-
  subset(inqdata,
         select = c("Series Name", "Country Name", "Country Code", "Year", "Index"))
inqdata <-
  inqdata[(
    inqdata$`Series Name` == "Poverty headcount ratio at national poverty lines (% of population)" |
      inqdata$`Series Name` == "GINI index (World Bank estimate)" |
      inqdata$`Series Name` == "Income share held by highest 10%"
  ), ]
inqdata <- na.omit(inqdata)
inqdata <- spread(inqdata, 'Series Name', Index)
inqdata <-
  inqdata[!(inqdata$`Country Name` == "Kiribati"), ] #remove Kiribati due to lack of relevant inequality data


#Rename columns to better reflect data and ensure columns are of correct type
colnames(inqdata) <-
  c(
    "Country",
    "Country_code",
    "Year",
    "GINI_index",
    "Income_share_10%",
    "Poverty_headcount_ratio_%_(national_lines)"
  )
inqdata$Year <- as.numeric(as.character(inqdata$Year))


#Write inequality data to a formatted file
write_csv(
  inqdata,
  paste(
    address,
    "Developing_Nations Inequality[FORMATTED] (2008-2017).csv",
    sep = ""
  )
)




# --------CALCULATE ADDITIONAL ECONOMIC DATA AND EXPORT--------
#Calculate trade balances


#Write econdata to a new file
write_csv(
  econdata,
  paste(
    address,
    "Developing_Nations IM-EX[FORMATTED] (2008-2017).csv",
    sep = ""
  )
)
