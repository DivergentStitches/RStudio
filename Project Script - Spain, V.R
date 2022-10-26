library(readr)
library(dplyr)
library(ggplot2)
library(pracma)
library(tidyr)
library(ggplot2)

############### FUNCTIONS #####################################

# Define error handling message
myerror <- function(error) {
  error
}

mydisplay <- function(msg) {
  print(msg)
}

AdoptionDate <- function(ADate = "")   {                             # Declare function
  if(ADate != "") {                                                  # Let user know what's being searched for
      msg <- sprintf("Searching for adoptions for date %s.", ADate)    
      #resultRows <- Dogs2[grep(Dogs2, pattern = ADate, x = Intake_Date, fixed = TRUE])  # Search for date
      #if (resultRows == is.na()) {
      #  mydisplay("No dogs adopted on that date.")
      #}
      Dogs2 <- filter(Dogs, Intake_Date==ADate)
      #Dogs2 <- Dogs2[resultRows, ]
      if(nrow(Dogs2) == 0) {                                               # Messages
        msg <- "0 adoptions found."
        mydisplay(msg)
      } else {
        msg <- print(sprintf("%s adoptions found.", nrow(Dogs2)))
         return(Dogs2[, c("Name_intake", "Breed_intake")])
      }}}


###############################################################

my_na <- c("", "NA")

Dogs <- tryCatch(read_csv("Adoption_Dogs.csv", na = my_na), error = myerror)

# Remove NAs
Dogs <- Dogs[complete.cases(Dogs), ]

# Renaming columns and formatting Intake_Date to be only the year

names(Dogs)[names(Dogs) == "DateTime_intake"] <- "Intake_Date"
names(Dogs)[names(Dogs) == "DateAdopted"] <- "Adoption_Date"
Dogs$'Animal ID' <- factor(Dogs$'Animal ID')
Dogs$Intake_Date <- format(as.Date(Dogs$Intake_Date, format="%d/%m/%Y"),"%m/%Y")

Dogs2 <- filter(Dogs, Intake_Date=="01/2015" | Intake_Date=="02/2015" | Intake_Date=="03/2015" | Intake_Date=="04/2015" | Intake_Date=="05/2015" | Intake_Date=="06/2015" | Intake_Date=="07/2015" | Intake_Date=="08/2015" | Intake_Date=="09/2015" | Intake_Date=="10/2015" | Intake_Date=="11/2015" | Intake_Date=="12/2015"| Intake_Date=="01/2016" | Intake_Date=="02/2016" | Intake_Date=="03/2016" | Intake_Date=="04/2016" | Intake_Date=="05/2016" | Intake_Date=="06/2016" | Intake_Date=="07/2016" | Intake_Date=="08/2016" | Intake_Date=="09/2016" | Intake_Date=="10/2016" | Intake_Date=="11/2016" | Intake_Date=="12/2016"| Intake_Date=="01/2017" | Intake_Date=="02/2017" | Intake_Date=="03/2017" | Intake_Date=="04/2017" | Intake_Date=="05/2017" | Intake_Date=="06/2017" | Intake_Date=="07/2017" | Intake_Date=="08/2017" | Intake_Date=="09/2017" | Intake_Date=="10/2017" | Intake_Date=="11/2017" | Intake_Date=="12/2017")  


Dogs2 <- AdoptionDate("01/2015")

################## PLOT ###############################

# Designate which columns for new df

my_cols_Dogs3 <- cols(
  Name_intake = col_character(),
  Breed_intake = col_character(),
  DateTime_intake = col_character()
  )

my_na <- c("", "NA")

Dogs3 <- read_csv("Adoption_Dogs.csv", col_types = my_cols_Dogs3, na = my_na)

Dogs3$`Animal ID` <- NULL
Dogs3$Found_Location <- NULL
Dogs3$Intake_Type <- NULL
Dogs3$Animal_Type <- NULL
Dogs3$Age <- NULL
Dogs3$Color_intake <- NULL
Dogs3$Name_Adoption <- NULL
Dogs3$DateAdopted <- NULL
Dogs3$Age_upon_Outcome <- NULL
Dogs3$gender_outcome <- NULL
Dogs3$fixed_outcome <- NULL
Dogs3$Age_Bucket <- NULL
Dogs3$StayLength_Days <- NULL
Dogs3$Days_Stayed_Group <- NULL
Dogs3$Outcome_Type <- NULL

# Rename Intake Date column, format to just mm/YYYY, and then split into Month and Year columns

names(Dogs3)[names(Dogs3) == "DateTime_intake"] <- "Intake_Date"
Dogs3$Intake_Date <- format(as.Date(Dogs3$Intake_Date, format="%d/%m/%Y"),"%m/%Y")
Dogs3 <- filter(Dogs3, Intake_Date=="01/2015" | Intake_Date=="02/2015" | Intake_Date=="03/2015" | Intake_Date=="04/2015" | Intake_Date=="05/2015" | Intake_Date=="06/2015" | Intake_Date=="07/2015" | Intake_Date=="08/2015" | Intake_Date=="09/2015" | Intake_Date=="10/2015" | Intake_Date=="11/2015" | Intake_Date=="12/2015"| Intake_Date=="01/2016" | Intake_Date=="02/2016" | Intake_Date=="03/2016" | Intake_Date=="04/2016" | Intake_Date=="05/2016" | Intake_Date=="06/2016" | Intake_Date=="07/2016" | Intake_Date=="08/2016" | Intake_Date=="09/2016" | Intake_Date=="10/2016" | Intake_Date=="11/2016" | Intake_Date=="12/2016"| Intake_Date=="01/2017" | Intake_Date=="02/2017" | Intake_Date=="03/2017" | Intake_Date=="04/2017" | Intake_Date=="05/2017" | Intake_Date=="06/2017" | Intake_Date=="07/2017" | Intake_Date=="08/2017" | Intake_Date=="09/2017" | Intake_Date=="10/2017" | Intake_Date=="11/2017" | Intake_Date=="12/2017")  
Dogs3$Month <- sapply(strsplit(as.character(Dogs3$Intake_Date), "/"), "[", 1)
Dogs3$Year <- sapply(strsplit(as.character(Dogs3$Intake_Date), "/"), "[", 2)
Dogs3$Month <- as.numeric(Dogs3$Month)
Dogs3$Year <- as.numeric(Dogs3$Year)

# Plot adoptions by date to find the best time of year for the event

p <- qplot(Year, Month, data = Dogs3, geom = "jitter", log = "y") +
  labs(title = 'Dog Adoptions 2014-2017')

p

#######################################################




