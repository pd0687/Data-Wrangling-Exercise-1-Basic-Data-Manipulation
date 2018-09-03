########################################################
### step 0: load the data
########################################################

setwd("C:/Springboard/Intro to Data Science/Data Wrangling/Basic data manipulation")
products <- read.csv("refine_original.csv")

library(dplyr)

products <- tbl_df(products)
colnames(products)[2] <- "productcode"

########################################################
### step 1: clean up brand names
########################################################

products <- products %>% mutate(company = sapply(company, tolower))

products$company <- sub(pattern = "^u.*", replacement = "unilever", 
                        x = products$company)
products$company <- sub(pattern = "^p.*", replacement = "philips", 
                        x = products$company)
products$company <- sub(pattern = "^f.*", replacement = "philips",
                        x = products$company)
products$company <- sub(pattern = "^a.*", replacement = "akzo",
                        x = products$company)

########################################################
### step 2: separate product code and number
########################################################

products <- products %>% mutate(product_code = sub(pattern = "-.*", 
                        replacement = "", x = productcode))
products <- products %>% mutate(product_number = sub(pattern = ".*-",
                        replacement = "", x = productcode))
products <- transform(products, product_number = as.numeric(product_number))

########################################################
### step 3: add product categories
########################################################

products$category <- ifelse(products$product_code == "p","Smartphone",
                        ifelse(products$product_code == "v","TV",
                        ifelse(products$product_code == "x","Laptop","Tablet")))

########################################################
### step 4: add full address for geocoding
########################################################

products <- products %>% mutate(full_address = paste(address, city, country,
                        sep = ", "))

########################################################
### step 5: create dummy variables for company and product category
########################################################

products <- products %>% 
  mutate(company_philips = ifelse(company == "philips",1,0))
products <- products %>% 
  mutate(company_akzo = ifelse(company == "akzo",1,0))
products <- products %>% 
  mutate(company_van_houten = ifelse(company == "van houten",1,0))
products <- products %>% 
  mutate(company_unilever = ifelse(company == "unilever",1,0))

products <- products %>% 
  mutate(product_smartphone = ifelse(category == "Smartphone",1,0))
products <- products %>% 
  mutate(product_laptop = ifelse(category == "Laptop",1,0))
products <- products %>% 
  mutate(product_tv = ifelse(category == "TV",1,0))
products <- products %>% 
  mutate(product_tablet = ifelse(category == "Tablet",1,0))

########################################################
### step 6: write to CSV
########################################################

products %>% write.csv("refine_clean.csv")

