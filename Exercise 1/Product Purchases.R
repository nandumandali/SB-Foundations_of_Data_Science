library(dplyr)
library(tidyr)

purchases <- read.csv("refine_original.csv")


purchases$company <- tolower(purchases$company)
  


for (i in 1:length(purchases$company)){
  if(grepl("^p",purchases$company[i])){
    purchases$company[i]<- "philips"
  } else if(grepl("^a",purchases$company[i])) {
    purchases$company[i]<- "akzo"
  } else if(grepl("^v",purchases$company[i])) {
    purchases$company[i]<- "van houten"
  } else  {
    purchases$company[i]<- "unilever"
  }
}

purchases2 <- separate(purchases, Product.code...number, c("product_code", "product_number"), sep="-")

purchases3 <- unite(purchases2, "full_address", address, city, country, sep=", ")

select_category <- function(code){
 
    if(code == "p") {
      return("Smartphone")
    } else if (code == "v"){
      return("TV")
    } else if (code == "x") {
      return("Laptop")
    } else {
      return("Tablet")
    }
}

for (i in 1:length(purchases3$product_code)){
  purchases3$product_Category[i] = select_category(purchases3$product_code[i])
  }

purchases4 <- purchases3[c(1,2,3,6,4,5)]

purchases4$company_philips <- 0
purchases4$company_akzo <- 0
purchases4$company_van_houten <- 0
purchases4$company_unilever <- 0
purchases4$product_smartphone <- 0
purchases4$product_tv <- 0
purchases4$product_laptop <- 0
purchases4$product_tablet <- 0


dummy_variable<- function(code, name){
  
  if(code == name) {
    return(1)
  } else {
    return(0)
  }
}

for (i in 1:length(purchases4$company)){
  purchases4$company_philips[i] = dummy_variable(purchases4$company[i],"philips")
  purchases4$company_akzo[i] = dummy_variable(purchases4$company[i],"akzo")
  purchases4$company_van_houten[i] = dummy_variable(purchases4$company[i],"van houten")
  purchases4$company_unilever[i] = dummy_variable(purchases4$company[i],"unilever")
  purchases4$product_smartphone[i] = dummy_variable(purchases4$product_Category[i], "Smartphone")
  purchases4$product_tv[i] = dummy_variable(purchases4$product_Category[i], "TV")
  purchases4$product_laptop[i] = dummy_variable(purchases4$product_Category[i], "Laptop")
  purchases4$product_tablet[i] = dummy_variable(purchases4$product_Category[i], "Tablet")
}

write.csv(purchases4, file = "C:/Users/913106/Desktop/Springboard Bootcamp/Data Wrangling/refine_clean.csv", row.names = FALSE)

