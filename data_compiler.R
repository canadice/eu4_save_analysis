# Compiles the following information using function information_finder from value_scraper
source("value_scraper.R")

country_information_compiler <- function(x){
  name <- str_extract(x[1], "[A-Z]{3}")
  continent <- information_finder(x, "continent")
  gov_rank <- information_finder(x, "government_rank")
  development <- information_finder(x, "raw_development")
  great_power <- information_finder(x, "great_power_score")
  cur_treasury <- information_finder(x, "treasury=")
  est_month_income <- information_finder(x, "estimated_monthly_income")
  mil_strength <- information_finder(x, "military_strength=")
  manpower <- information_finder(x, "max_manpower=")
  cur_army_size <- information_finder(x, "num_of_regulars")
  religion <- information_finder(x, "^\t\treligion=")
  capital <- information_finder(x, "^\t\tcapital=")
  
  data <- cbind(name, continent, gov_rank, development, great_power, cur_treasury, 
                est_month_income, mil_strength, manpower, cur_army_size, religion, capital)
  
  return(data)
}

province_information_compiler <- function(x){
  capital <- str_extract(x[1], "[0-9]+")
  hre <- information_finder(x, "hre=")
  
  data <- cbind(capital, hre)
  
  return(data)
}
