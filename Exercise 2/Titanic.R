library(dplyr)
library(tidyr)

passengers <- read.csv("titanic_original.csv", stringsAsFactors = FALSE)

for (i in 1:length(passengers$embarked)){
  if(passengers$embarked [i]== "" | is.null(passengers$embarked[i])){
    passengers$embarked[i] <- "S"
  }
}
sum_age <- 0
number <- 0
for (i in 1:length(passengers$age)){
  if(!is.na(passengers$age[i])){
    sum_age <- sum_age + passengers$age[i]
    number <- number+1
  }
}

mean_age <- sum_age / number

for (i in 1:length(passengers$age)){
  if(is.na(passengers$age[i])){
    passengers$age[i] <- mean_age
  }
}

for (i in 1:length(passengers$boat)){
  if(passengers$boat[i] == ""){
    passengers$boat[i] = 'NA'
  }
}

passengers$has_cabin_number <- 0

for (i in 1:length(passengers$cabin)){
  if(passengers$cabin[i] == ""){
    passengers$has_cabin_number[i] <- 0
  } else {
    passengers$has_cabin_number[i] <- 1
  }
}

write.csv(passengers, file = "C:/Users/913106/Desktop/Springboard Bootcamp/Data Wrangling/Exercise 2/titanic_clean.csv", row.names = FALSE)
