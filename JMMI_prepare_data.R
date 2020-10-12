library(readxl)
library(survey)
library(openxlsx)
library(tidyr)
library(dplyr)
library(tidyverse)

`%notin%` <- Negate(`%in%`)
usd_rate <- 76.9

### Abbreviations ###
# curr_data == Current round data
# prev_data == Previous round data
# _cr_ == Current round
# _pr_ == Previous round

# Current Round Data
curr_data<-read_excel("input/data/clean/curr_data/AFG2002_JMMI_R5_September_2020_clean.xlsx", na =  c("", "NA","N/A"))  %>% type.convert(as.is = T) %>% 
 filter(time_validity == "Valid")
curr_data <- curr_data %>% mutate_at(vars(ends_with("_price")), as.numeric)
curr_data <- curr_data %>% mutate_at(vars(ends_with("_unit_specify")), as.numeric)


# Previous Round Data
prev_data <- read.csv("input/data/clean/prev_data/AFG2002_JMMI_August_2020_recoded.csv", na.strings = c("", "NA","N/A"), stringsAsFactors = F) 

prev_data <- prev_data %>% mutate(
  lowest_lintils_beans = case_when(
    pulses_lentils_price < pulses_beans_price ~ pulses_lentils_price,
    # is.na(pulses_beans_price) & !is.na(pulses_lentils_price) ~ pulses_beans_price,
    TRUE ~ pulses_beans_price
  ),
  lowest_lintils_beans_final = case_when(
    is.na(pulses_beans_price) ~ pulses_lentils_price,
    TRUE ~ lowest_lintils_beans
    
  ),
  pulses_merged_price = case_when(
    lowest_lintils_beans < pulses_split_peas_price ~ lowest_lintils_beans,
    TRUE ~ pulses_split_peas_price
  ),
  pulses_merged_price_final = case_when(
    is.na(pulses_split_peas_price) ~ lowest_lintils_beans_final,
    TRUE ~ pulses_merged_price
  )
)


## Normalize prices

#kindling 
curr_data$kindling_price <- curr_data$kindling_price / curr_data$kindling_unit_specify

# firewood 
curr_data$firewood_price <- curr_data$firewood_price / curr_data$firewood_unit_specify

# Safe Water
curr_data$safe_water_price <- curr_data$safe_water_price / curr_data$safe_water_unit_specify

# Safe Water, per 20 Ltr
curr_data$safe_water_price_20L <- (curr_data$safe_water_price / curr_data$safe_water_unit_specify) * 20

# Bleach 
curr_data$bleach_price <- curr_data$bleach_price / curr_data$bleach_unit_specify

# washing Detergent price 
curr_data$washing_detergent_price <- curr_data$washing_detergent_price / curr_data$washing_detergent_unit_specify

# Sanitary pad - problematic 
curr_data$sanitary_pad_price <- curr_data$sanitary_pad_price / curr_data$sanitary_pad_unit_specify

#tooth paste 
curr_data$toothpaste_price <- curr_data$toothpaste_price / curr_data$toothpaste_unit_specify

# toothbrush adult price
curr_data$toothbrush_adult_price <- curr_data$toothbrush_adult_price / curr_data$toothbrush_adult_unit_specify

# cotton per unit 
curr_data$cotton_cloth_price <- curr_data$cotton_cloth_price / curr_data$cotton_cloth_unit_specify 

## egg price
curr_data$eggs_price <- curr_data$eggs_price / curr_data$eggs_unit_specify

# onions
curr_data$onions_price <- curr_data$onions_price / curr_data$onions_unit_specify

# potatoes
curr_data$potatoes_price <- curr_data$potatoes_price / curr_data$potatoes_unit_specify

# tomatoes
curr_data$tomatoes_price <- curr_data$tomatoes_price / curr_data$tomatoes_unit_specify

# salt
curr_data$salt_price <- curr_data$salt_price / curr_data$salt_unit_specify

# split peas
curr_data$pulses_split_peas_price <- curr_data$pulses_split_peas_price / curr_data$pulses_split_peas_unit_specify

# beans
curr_data$pulses_beans_price <- curr_data$pulses_beans_price / curr_data$pulses_beans_unit_specify

# lentils
curr_data$pulses_lentils_price <- curr_data$pulses_lentils_price / curr_data$pulses_lentils_unit_specify

# rice
curr_data$local_rice_price <- curr_data$local_rice_price / curr_data$local_rice_unit_specify

# local wheat
curr_data$wheat_local_price <- curr_data$wheat_local_price / curr_data$wheat_local_unit_specify

# imported wheat
curr_data$wheat_imported_price <- curr_data$wheat_imported_price / curr_data$wheat_imported_unit_specify

# veg oil
curr_data$veg_oil_price <- curr_data$veg_oil_price / curr_data$veg_oil_unit_specify

# sugar
curr_data$sugar_price <-  curr_data$sugar_price / curr_data$sugar_unit_specify 

# cooking fuel
curr_data$cooking_fuel_price <- curr_data$cooking_fuel_price / curr_data$cooking_fuel_unit_specify


# Constructing new variables
curr_data$financial_services_sum <- curr_data$financial_services.banks + curr_data$financial_services.mobile_money_agents +
  curr_data$financial_services.formal_transfer_services + curr_data$financial_services.hawala +
  curr_data$financial_services.microfinance_institutions + curr_data$financial_services.village_banks +
  curr_data$financial_services.credit_unions + curr_data$financial_services.informal_savings_groups + 
  curr_data$financial_services.local_businesses + curr_data$financial_services.members_community + curr_data$financial_services.other

curr_data <- curr_data %>% mutate(
  financial_services_cat = case_when(
    financial_services_sum == 0 ~ "none",
    financial_services_sum == 1 ~ "one",
    financial_services_sum > 1 ~ "two_or_more",
  ), 
  pulses_merged_availability = case_when(
    pulses_lentils_availability == "available" | pulses_beans_availability == "available" | pulses_split_peas_availability == "available" ~ "available",
    pulses_lentils_availability == "limited" | pulses_beans_availability == "limited" | pulses_split_peas_availability == "limited" ~ "limited",
    pulses_lentils_availability == "unavailable" | pulses_beans_availability == "unavailable" | pulses_split_peas_availability == "unavailable" ~ "unavailable"
 
  ),
  lowest_lintils_beans = case_when(
    pulses_lentils_price < pulses_beans_price ~ pulses_lentils_price,
    TRUE ~ pulses_beans_price
  ),
  lowest_lintils_beans_final = case_when(
    is.na(pulses_beans_price) ~ pulses_lentils_price,
    TRUE ~ lowest_lintils_beans
    
  ),
  pulses_merged_price = case_when(
    lowest_lintils_beans < pulses_split_peas_price ~ lowest_lintils_beans,
    TRUE ~ pulses_split_peas_price
  ),
  pulses_merged_price_final = case_when(
    is.na(pulses_split_peas_price) ~ lowest_lintils_beans_final,
    TRUE ~ pulses_merged_price
  ),
  pulses_merged_stock_last = case_when(
    pulses_lentils_stock_last == "2month" | pulses_beans_stock_last == "2month" | pulses_split_peas_stock_last == "2month" ~ "2month",
    pulses_lentils_stock_last == "1month" | pulses_beans_stock_last == "1month" | pulses_split_peas_stock_last == "1month" ~ "1month",
    pulses_lentils_stock_last == "2_4weeks" | pulses_beans_stock_last == "2_4weeks" | pulses_split_peas_stock_last == "2_4weeks" ~ "2_4weeks",
    pulses_lentils_stock_last == "1_2weeks" | pulses_beans_stock_last == "1_2weeks" | pulses_split_peas_stock_last == "1_2weeks" ~ "1_2weeks",
    pulses_lentils_stock_last == "1week" | pulses_beans_stock_last == "1week" | pulses_split_peas_stock_last == "1week" ~ "1week",
    pulses_lentils_stock_last == "dont_know" | pulses_beans_stock_last == "dont_know" | pulses_split_peas_stock_last == "dont_know" ~ "dont_know",
  ),
  pulses_merged_past_demand = case_when(
    pulses_lentils_past_demand == "increase" | pulses_beans_past_demand == "increase" | pulses_split_peas_past_demand == "increase" ~ "increase",
    pulses_lentils_past_demand == "decrease" | pulses_beans_past_demand == "decrease" | pulses_split_peas_past_demand == "decrease" ~ "decrease",
    pulses_lentils_past_demand == "no_change" | pulses_beans_past_demand == "no_change" | pulses_split_peas_past_demand == "no_change" ~ "no_change",
    pulses_lentils_past_demand == "dont_know" | pulses_beans_past_demand == "dont_know" | pulses_split_peas_past_demand == "dont_know" ~ "dont_know"
  ),
  food_demand_inc_total = case_when(
    wheat_local_past_demand == "increase" | wheat_imported_past_demand == "increase" | local_rice_past_demand == "increase" |
      veg_oil_past_demand == "increase" | pulses_lentils_past_demand == "increase" | pulses_beans_past_demand == "increase" |
      pulses_split_peas_past_demand == "increase" | salt_past_demand == "increase" | sugar_past_demand == "increase" |
      tomatoes_past_demand == "increase" | potatoes_past_demand == "increase" | onions_past_demand == "increase" |
      eggs_past_demand == "increase" ~ "reported_increase",
    TRUE ~ "no_increase"
  ),
  food_demand_dec_total = case_when(
    wheat_local_past_demand == "decrease" | wheat_imported_past_demand == "decrease" | local_rice_past_demand == "decrease" |
      veg_oil_past_demand == "decrease" | pulses_lentils_past_demand == "decrease" | pulses_beans_past_demand == "decrease" |
      pulses_split_peas_past_demand == "decrease" | salt_past_demand == "decrease" | sugar_past_demand == "decrease" |
      tomatoes_past_demand == "decrease" | potatoes_past_demand == "decrease" | onions_past_demand == "decrease" |
      eggs_past_demand == "decrease" ~ "reported_decrease",
    TRUE ~ "no_decrease"
  ),
  nfi_demand_inc_total = case_when(
    cotton_cloth_past_demand == "increase" | toothbrush_adult_past_demand == "increase" | toothpaste_past_demand == "increase" |
      sanitary_pad_past_demand == "increase" | washing_detergent_past_demand == "increase" | soap_past_demand == "increase" |
      bleach_past_demand == "increase" | safe_water_past_demand == "increase" |
      firewood_past_demand == "increase" | kindling_past_demand == "increase" |
      cooking_fuel_past_demand == "increase" ~ "reported_increase",
    TRUE ~ "no_increase"
  ),
  nfi_demand_dec_total = case_when(
    cotton_cloth_past_demand == "decrease" | toothbrush_adult_past_demand == "decrease" | toothpaste_past_demand == "decrease" |
      sanitary_pad_past_demand == "decrease" | washing_detergent_past_demand == "decrease" | soap_past_demand == "decrease" |
      bleach_past_demand == "decrease" | safe_water_past_demand == "decrease" |
      firewood_past_demand == "decrease" | kindling_past_demand == "decrease" |
      cooking_fuel_past_demand == "decrease" ~ "reported_decrease",
    TRUE ~ "no_decrease"
  ), 
  difficulty_roads_comp = case_when(
    difficulty_roads.no_none == 1 ~ "no",
    difficulty_roads.no_none != 1 & difficulty_roads_new != "yes" ~ "yes_prior_last_30_days",
    difficulty_roads.no_none != 1 & difficulty_roads_new == "yes" ~ "yes_new_last_30_days"
  )
  
)

# convert prices to USD
curr_data$wheat_local_price_USD <- curr_data$wheat_local_price / usd_rate
curr_data$wheat_imported_price_USD <- curr_data$wheat_imported_price / usd_rate
curr_data$local_rice_price_USD <- curr_data$local_rice_price / usd_rate
curr_data$veg_oil_price_USD <- curr_data$veg_oil_price / usd_rate
curr_data$pulses_merged_price_final_USD <- curr_data$pulses_merged_price_final / usd_rate
curr_data$salt_price_USD <- curr_data$salt_price / usd_rate
curr_data$sugar_price_USD <- curr_data$sugar_price / usd_rate
curr_data$tomatoes_price_USD <- curr_data$tomatoes_price / usd_rate
curr_data$potatoes_price_USD <- curr_data$potatoes_price / usd_rate
curr_data$onions_price_USD <- curr_data$onions_price / usd_rate
curr_data$eggs_price_USD <- curr_data$eggs_price / usd_rate
curr_data$cotton_cloth_price_USD <- curr_data$cotton_cloth_price / usd_rate
curr_data$toothbrush_adult_price_USD <- curr_data$toothbrush_adult_price / usd_rate
curr_data$toothpaste_price_USD <- curr_data$toothpaste_price / usd_rate
curr_data$sanitary_pad_price_USD <- curr_data$sanitary_pad_price / usd_rate
curr_data$washing_detergent_price_USD <- curr_data$washing_detergent_price / usd_rate
curr_data$soap_price_USD <- curr_data$soap_price / usd_rate
curr_data$bleach_price_USD <- curr_data$bleach_price / usd_rate
curr_data$safe_water_price_USD <- curr_data$safe_water_price / usd_rate
curr_data$safe_water_price_20L_USD <- curr_data$safe_water_price_20L / usd_rate
curr_data$firewood_price_USD <- curr_data$firewood_price / usd_rate
curr_data$kindling_price_USD <- curr_data$kindling_price / usd_rate
curr_data$cooking_fuel_price_USD <- curr_data$cooking_fuel_price / usd_rate


############ MEDIAN Calculations #################

### National ###

# aggregate current round prices at National level  - median
curr_data_national_median <- curr_data %>% 
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_cr_median = median(salt_price, na.rm = T),
    sugar_price_cr_median = median(sugar_price, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price, na.rm = T),
    onions_price_cr_median = median(onions_price, na.rm = T),
    eggs_price_cr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price, na.rm = T),
    soap_price_cr_median = median(soap_price, na.rm = T),
    bleach_price_cr_median = median(bleach_price, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L, na.rm = T),
    firewood_price_cr_median = median(firewood_price, na.rm = T),
    kindling_price_cr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_cr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_cr_median = median(usd_afn_sellrate, na.rm = T)
  )

# aggregate previous round prices at national level  - median
prev_data_national_median <- prev_data %>% 
  summarise(
    wheat_local_price_pr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_pr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_pr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_pr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_pr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_pr_median = median(salt_price, na.rm = T),
    sugar_price_pr_median = median(sugar_price, na.rm = T),
    tomatoes_price_pr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_pr_median = median(potatoes_price, na.rm = T),
    onions_price_pr_median = median(onions_price, na.rm = T),
    eggs_price_pr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_pr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_pr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_pr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_pr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_pr_median = median(washing_detergent_price, na.rm = T),
    soap_price_pr_median = median(soap_price, na.rm = T),
    bleach_price_pr_median = median(bleach_price, na.rm = T),
    safe_water_price_pr_median = median(safe_water_price, na.rm = T),
    firewood_price_pr_median = median(firewood_price, na.rm = T),
    kindling_price_pr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_pr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_pr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_pr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_pr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_pr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_pr_median = median(usd_afn_sellrate, na.rm = T)
  )


# merge previous round and current round Regional level - median
national_median_merged <- cbind( curr_data_national_median, prev_data_national_median)


# Price difference National level - median
national_median_merged_price_diff <- national_median_merged %>% 
  mutate(
    wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr_median) / wheat_local_price_pr_median,
    wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr_median) / wheat_imported_price_pr_median,
    local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr_median ) / local_rice_price_pr_median,
    veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr_median ) / veg_oil_price_pr_median, 
    pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr_median  ) / pulses_merged_price_final_pr_median, 
    salt_price_diff_perc = (salt_price_cr_median - salt_price_pr_median) / salt_price_pr_median, 
    sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr_median ) / sugar_price_pr_median, 
    tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr_median ) / tomatoes_price_pr_median, 
    potatoes_price_diff_perc = (potatoes_price_cr_median - potatoes_price_pr_median ) / potatoes_price_pr_median, 
    onions_price_diff_perc = (onions_price_cr_median - onions_price_pr_median ) / onions_price_pr_median, 
    eggs_price_diff_perc = (eggs_price_cr_median - eggs_price_pr_median ) / eggs_price_pr_median, 
    cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr_median ) / cotton_cloth_price_pr_median, 
    toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr_median ) / toothbrush_adult_price_pr_median, 
    toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr_median  ) / toothpaste_price_pr_median, 
    sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr_median) / sanitary_pad_price_pr_median, 
    washing_detergent_price_diff_perc = (washing_detergent_price_cr_median - washing_detergent_price_pr_median  ) / washing_detergent_price_pr_median , 
    soap_price_diff_perc = (soap_price_cr_median - soap_price_pr_median) / soap_price_pr_median, 
    bleach_price_diff_perc = (bleach_price_cr_median - bleach_price_pr_median  ) / bleach_price_pr_median, 
    safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr_median ) / safe_water_price_pr_median, 
    firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr_median ) / firewood_price_pr_median, 
    kindling_price_diff_perc = (kindling_price_cr_median - kindling_price_pr_median ) / kindling_price_pr_median, 
    cooking_fuel_price_diff_perc = (cooking_fuel_price_cr_median - cooking_fuel_price_pr_median) / cooking_fuel_price_pr_median,
    pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr_median) / pulses_lentils_price_pr_median,
    pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr_median) / pulses_beans_price_pr_median,
    pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr_median) / pulses_split_peas_price_pr_median,
    usd_afn_buyrate_diff_perc = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median) / usd_afn_buyrate_pr_median,
    usd_afn_sellrate_diff_perc = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median) / usd_afn_sellrate_pr_median,
    usd_afn_buyrate_diff_abs = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median), 
    usd_afn_sellrate_diff_abs = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median)
  )

national_median_merged_price_diff_rounded <- national_median_merged_price_diff %>% 
  mutate_at(vars(ends_with("_diff_perc")), funs(round( . * 100, 2)))

# prices in USD
# aggregate current round prices at National level - median
curr_data_national_median_USD <- curr_data %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price_USD, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price_USD, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price_USD, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price_USD, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final_USD, na.rm = T),
    salt_price_cr_median = median(salt_price_USD, na.rm = T),
    sugar_price_cr_median = median(sugar_price_USD, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price_USD, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price_USD, na.rm = T),
    onions_price_cr_median = median(onions_price_USD, na.rm = T),
    eggs_price_cr_median = median(eggs_price_USD, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price_USD, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price_USD, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price_USD, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price_USD, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price_USD, na.rm = T),
    soap_price_cr_median = median(soap_price_USD, na.rm = T),
    bleach_price_cr_median = median(bleach_price_USD, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L_USD, na.rm = T),
    firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
    kindling_price_cr_median = median(kindling_price_USD, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price_USD, na.rm = T),
  )

### Region ###

# aggregate current round prices at Regional level  - median
curr_data_region_median <- curr_data %>% 
  group_by(afg_region) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_cr_median = median(salt_price, na.rm = T),
    sugar_price_cr_median = median(sugar_price, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price, na.rm = T),
    onions_price_cr_median = median(onions_price, na.rm = T),
    eggs_price_cr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price, na.rm = T),
    soap_price_cr_median = median(soap_price, na.rm = T),
    bleach_price_cr_median = median(bleach_price, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L, na.rm = T),
    firewood_price_cr_median = median(firewood_price, na.rm = T),
    kindling_price_cr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_cr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_cr_median = median(usd_afn_sellrate, na.rm = T)
  )

# aggregate previous round prices at Regional level  - median
prev_data_region_median <- prev_data %>% 
  group_by(afg_region) %>%
  summarise(
    wheat_local_price_pr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_pr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_pr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_pr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_pr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_pr_median = median(salt_price, na.rm = T),
    sugar_price_pr_median = median(sugar_price, na.rm = T),
    tomatoes_price_pr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_pr_median = median(potatoes_price, na.rm = T),
    onions_price_pr_median = median(onions_price, na.rm = T),
    eggs_price_pr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_pr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_pr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_pr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_pr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_pr_median = median(washing_detergent_price, na.rm = T),
    soap_price_pr_median = median(soap_price, na.rm = T),
    bleach_price_pr_median = median(bleach_price, na.rm = T),
    safe_water_price_pr_median = median(safe_water_price, na.rm = T),
    firewood_price_pr_median = median(firewood_price, na.rm = T),
    kindling_price_pr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_pr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_pr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_pr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_pr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_pr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_pr_median = median(usd_afn_sellrate, na.rm = T)
  )

# merge previous round and current round Regional level - median
region_median_merged <- curr_data_region_median %>% left_join(prev_data_region_median, by = "afg_region")

# Price difference Regional level - median
region_median_merged_price_diff <- region_median_merged %>% 
  mutate(
    wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr_median) / wheat_local_price_pr_median,
    wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr_median) / wheat_imported_price_pr_median,
    local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr_median ) / local_rice_price_pr_median,
    veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr_median ) / veg_oil_price_pr_median, 
    pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr_median  ) / pulses_merged_price_final_pr_median, 
    salt_price_diff_perc = (salt_price_cr_median - salt_price_pr_median) / salt_price_pr_median, 
    sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr_median ) / sugar_price_pr_median, 
    tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr_median ) / tomatoes_price_pr_median, 
    potatoes_price_diff_perc = (potatoes_price_cr_median - potatoes_price_pr_median ) / potatoes_price_pr_median, 
    onions_price_diff_perc = (onions_price_cr_median - onions_price_pr_median ) / onions_price_pr_median, 
    eggs_price_diff_perc = (eggs_price_cr_median - eggs_price_pr_median ) / eggs_price_pr_median, 
    cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr_median ) / cotton_cloth_price_pr_median, 
    toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr_median ) / toothbrush_adult_price_pr_median, 
    toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr_median  ) / toothpaste_price_pr_median, 
    sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr_median) / sanitary_pad_price_pr_median, 
    washing_detergent_price_diff_perc = (washing_detergent_price_cr_median - washing_detergent_price_pr_median  ) / washing_detergent_price_pr_median , 
    soap_price_diff_perc = (soap_price_cr_median - soap_price_pr_median) / soap_price_pr_median, 
    bleach_price_diff_perc = (bleach_price_cr_median - bleach_price_pr_median  ) / bleach_price_pr_median, 
    safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr_median ) / safe_water_price_pr_median, 
    firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr_median ) / firewood_price_pr_median, 
    kindling_price_diff_perc = (kindling_price_cr_median - kindling_price_pr_median ) / kindling_price_pr_median, 
    cooking_fuel_price_diff_perc = (cooking_fuel_price_cr_median - cooking_fuel_price_pr_median) / cooking_fuel_price_pr_median,
    pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr_median) / pulses_lentils_price_pr_median,
    pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr_median) / pulses_beans_price_pr_median,
    pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr_median) / pulses_split_peas_price_pr_median,
    usd_afn_buyrate_diff_perc = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median) / usd_afn_buyrate_pr_median,
    usd_afn_sellrate_diff_perc = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median) / usd_afn_sellrate_pr_median,
    usd_afn_buyrate_diff_abs = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median), 
    usd_afn_sellrate_diff_abs = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median)
  )

region_median_merged_price_diff_rounded <- region_median_merged_price_diff %>% 
  mutate_at(vars(ends_with("_diff_perc")), funs(round( . * 100, 2)))

# prices in USD
# aggregate current round prices at Regional level - median
curr_data_region_median_USD <- curr_data %>%
  group_by(afg_region) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price_USD, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price_USD, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price_USD, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price_USD, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final_USD, na.rm = T),
    salt_price_cr_median = median(salt_price_USD, na.rm = T),
    sugar_price_cr_median = median(sugar_price_USD, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price_USD, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price_USD, na.rm = T),
    onions_price_cr_median = median(onions_price_USD, na.rm = T),
    eggs_price_cr_median = median(eggs_price_USD, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price_USD, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price_USD, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price_USD, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price_USD, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price_USD, na.rm = T),
    soap_price_cr_median = median(soap_price_USD, na.rm = T),
    bleach_price_cr_median = median(bleach_price_USD, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L_USD, na.rm = T),
    firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
    kindling_price_cr_median = median(kindling_price_USD, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price_USD, na.rm = T),
  )

### Province ###

# aggregate current round prices at province level  - median
curr_data_provice_median <- curr_data %>% 
  group_by(afg_prov) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_cr_median = median(salt_price, na.rm = T),
    sugar_price_cr_median = median(sugar_price, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price, na.rm = T),
    onions_price_cr_median = median(onions_price, na.rm = T),
    eggs_price_cr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price, na.rm = T),
    soap_price_cr_median = median(soap_price, na.rm = T),
    bleach_price_cr_median = median(bleach_price, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L, na.rm = T),
    firewood_price_cr_median = median(firewood_price, na.rm = T),
    kindling_price_cr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_cr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_cr_median = median(usd_afn_sellrate, na.rm = T)
    #Below calculation is exceptional and it's only for round 5th, should be removed after this round.
  ) %>% mutate(
  cotton_cloth_price_cr_median = case_when(
    afg_prov == "sar_e_pul" & cotton_cloth_price_cr_median > 200 ~ curr_data_region_median$cotton_cloth_price_cr_median[curr_data_region_median$afg_region == "northern"],
    afg_prov == "jawzjan" & cotton_cloth_price_cr_median > 200 ~ curr_data_region_median$cotton_cloth_price_cr_median[curr_data_region_median$afg_region == "northern"],
    afg_prov == "paktya" & cotton_cloth_price_cr_median < 31 ~ curr_data_region_median$cotton_cloth_price_cr_median[curr_data_region_median$afg_region == "south_eastern"],
    TRUE ~ cotton_cloth_price_cr_median
  ),
  sanitary_pad_price_cr_median = case_when(
    afg_prov == "paktya" & sanitary_pad_price_cr_median < 20 ~ curr_data_region_median$sanitary_pad_price_cr_median[curr_data_region_median$afg_region == "south_eastern"],
    afg_prov == "parwan" & sanitary_pad_price_cr_median < 20 ~ curr_data_region_median$sanitary_pad_price_cr_median[curr_data_region_median$afg_region == "central"],
    TRUE ~ sanitary_pad_price_cr_median
  )
)

# aggregate previous round prices at province level  - median
prev_data_provice_median <- prev_data %>% 
  group_by(afg_prov) %>%
  summarise(
    wheat_local_price_pr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_pr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_pr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_pr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_pr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_pr_median = median(salt_price, na.rm = T),
    sugar_price_pr_median = median(sugar_price, na.rm = T),
    tomatoes_price_pr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_pr_median = median(potatoes_price, na.rm = T),
    onions_price_pr_median = median(onions_price, na.rm = T),
    eggs_price_pr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_pr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_pr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_pr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_pr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_pr_median = median(washing_detergent_price, na.rm = T),
    soap_price_pr_median = median(soap_price, na.rm = T),
    bleach_price_pr_median = median(bleach_price, na.rm = T),
    safe_water_price_pr_median = median(safe_water_price, na.rm = T),
    firewood_price_pr_median = median(firewood_price, na.rm = T),
    kindling_price_pr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_pr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_pr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_pr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_pr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_pr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_pr_median = median(usd_afn_sellrate, na.rm = T)
  )

# merge previous round and current round - median
province_median_merged <- curr_data_provice_median %>% left_join(prev_data_provice_median, by = "afg_prov")

# Price difference province level - median
province_median_merged_price_diff <- province_median_merged %>% 
  mutate(
    wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr_median) / wheat_local_price_pr_median,
    wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr_median) / wheat_imported_price_pr_median,
    local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr_median ) / local_rice_price_pr_median,
    veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr_median ) / veg_oil_price_pr_median, 
    pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr_median  ) / pulses_merged_price_final_pr_median, 
    salt_price_diff_perc = (salt_price_cr_median - salt_price_pr_median) / salt_price_pr_median, 
    sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr_median ) / sugar_price_pr_median, 
    tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr_median ) / tomatoes_price_pr_median, 
    potatoes_price_diff_perc = (potatoes_price_cr_median - potatoes_price_pr_median ) / potatoes_price_pr_median, 
    onions_price_diff_perc = (onions_price_cr_median - onions_price_pr_median ) / onions_price_pr_median, 
    eggs_price_diff_perc = (eggs_price_cr_median - eggs_price_pr_median ) / eggs_price_pr_median, 
    cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr_median ) / cotton_cloth_price_pr_median, 
    toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr_median ) / toothbrush_adult_price_pr_median, 
    toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr_median  ) / toothpaste_price_pr_median, 
    sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr_median) / sanitary_pad_price_pr_median, 
    washing_detergent_price_diff_perc = (washing_detergent_price_cr_median - washing_detergent_price_pr_median  ) / washing_detergent_price_pr_median , 
    soap_price_diff_perc = (soap_price_cr_median - soap_price_pr_median) / soap_price_pr_median, 
    bleach_price_diff_perc = (bleach_price_cr_median - bleach_price_pr_median  ) / bleach_price_pr_median, 
    safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr_median ) / safe_water_price_pr_median, 
    firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr_median ) / firewood_price_pr_median, 
    kindling_price_diff_perc = (kindling_price_cr_median - kindling_price_pr_median ) / kindling_price_pr_median, 
    cooking_fuel_price_diff_perc = (cooking_fuel_price_cr_median - cooking_fuel_price_pr_median) / cooking_fuel_price_pr_median,
    pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr_median) / pulses_lentils_price_pr_median,
    pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr_median) / pulses_beans_price_pr_median,
    pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr_median) / pulses_split_peas_price_pr_median,
    usd_afn_buyrate_diff_perc = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median) / usd_afn_buyrate_pr_median,
    usd_afn_sellrate_diff_perc = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median) / usd_afn_sellrate_pr_median,
    usd_afn_buyrate_diff_abs = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median), 
    usd_afn_sellrate_diff_abs = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median)
  )

province_median_merged_price_diff_rounded <- province_median_merged_price_diff %>% 
  mutate_at(vars(ends_with("_diff_perc")), funs(round( . * 100, 2)))  

# prices in USD
# aggregate current round prices at Province level - median
curr_data_province_median_USD <- curr_data %>%
  group_by(afg_prov) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price_USD, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price_USD, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price_USD, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price_USD, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final_USD, na.rm = T),
    salt_price_cr_median = median(salt_price_USD, na.rm = T),
    sugar_price_cr_median = median(sugar_price_USD, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price_USD, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price_USD, na.rm = T),
    onions_price_cr_median = median(onions_price_USD, na.rm = T),
    eggs_price_cr_median = median(eggs_price_USD, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price_USD, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price_USD, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price_USD, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price_USD, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price_USD, na.rm = T),
    soap_price_cr_median = median(soap_price_USD, na.rm = T),
    bleach_price_cr_median = median(bleach_price_USD, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L_USD, na.rm = T),
    firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
    kindling_price_cr_median = median(kindling_price_USD, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price_USD, na.rm = T),
  )

### District Level ###

# aggregate current round prices at District level  - median
curr_data_district_median <- curr_data %>% 
  group_by(afg_dist) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_cr_median = median(salt_price, na.rm = T),
    sugar_price_cr_median = median(sugar_price, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price, na.rm = T),
    onions_price_cr_median = median(onions_price, na.rm = T),
    eggs_price_cr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price, na.rm = T),
    soap_price_cr_median = median(soap_price, na.rm = T),
    bleach_price_cr_median = median(bleach_price, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L, na.rm = T),
    firewood_price_cr_median = median(firewood_price, na.rm = T),
    kindling_price_cr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_cr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_cr_median = median(usd_afn_sellrate, na.rm = T)
  )

# aggregate previous round prices at District level  - median
prev_data_district_median <- prev_data %>% 
  group_by(afg_dist) %>%
  summarise(
    wheat_local_price_pr_median = median(wheat_local_price, na.rm = T),
    wheat_imported_price_pr_median = median(wheat_imported_price, na.rm = T),
    local_rice_price_pr_median = median(local_rice_price, na.rm = T),
    veg_oil_price_pr_median = median(veg_oil_price, na.rm = T),
    pulses_merged_price_final_pr_median = median(pulses_merged_price_final, na.rm = T),
    salt_price_pr_median = median(salt_price, na.rm = T),
    sugar_price_pr_median = median(sugar_price, na.rm = T),
    tomatoes_price_pr_median = median(tomatoes_price, na.rm = T),
    potatoes_price_pr_median = median(potatoes_price, na.rm = T),
    onions_price_pr_median = median(onions_price, na.rm = T),
    eggs_price_pr_median = median(eggs_price, na.rm = T),
    cotton_cloth_price_pr_median = median(cotton_cloth_price, na.rm = T),
    toothbrush_adult_price_pr_median = median(toothbrush_adult_price, na.rm = T),
    toothpaste_price_pr_median = median(toothpaste_price, na.rm = T),
    sanitary_pad_price_pr_median = median(sanitary_pad_price, na.rm = T),
    washing_detergent_price_pr_median = median(washing_detergent_price, na.rm = T),
    soap_price_pr_median = median(soap_price, na.rm = T),
    bleach_price_pr_median = median(bleach_price, na.rm = T),
    safe_water_price_pr_median = median(safe_water_price, na.rm = T),
    firewood_price_pr_median = median(firewood_price, na.rm = T),
    kindling_price_pr_median = median(kindling_price, na.rm = T),
    cooking_fuel_price_pr_median = median(cooking_fuel_price, na.rm = T),
    pulses_lentils_price_pr_median = median(pulses_lentils_price, na.rm = T),
    pulses_beans_price_pr_median = median(pulses_beans_price, na.rm = T),
    pulses_split_peas_price_pr_median = median(pulses_split_peas_price, na.rm = T),
    usd_afn_buyrate_pr_median = median(usd_afn_buyrate, na.rm = T),
    usd_afn_sellrate_pr_median = median(usd_afn_sellrate, na.rm = T)
  )

# merge previous round and current round District level - median
district_median_merged <- curr_data_district_median %>% left_join(prev_data_district_median, by = "afg_dist")

# Price difference District level - median
district_median_merged_price_diff <- district_median_merged %>% 
  mutate(
    wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr_median) / wheat_local_price_pr_median,
    wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr_median) / wheat_imported_price_pr_median,
    local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr_median ) / local_rice_price_pr_median,
    veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr_median ) / veg_oil_price_pr_median, 
    pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr_median  ) / pulses_merged_price_final_pr_median, 
    salt_price_diff_perc = (salt_price_cr_median - salt_price_pr_median) / salt_price_pr_median, 
    sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr_median ) / sugar_price_pr_median, 
    tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr_median ) / tomatoes_price_pr_median, 
    potatoes_price_diff_perc = (potatoes_price_cr_median - potatoes_price_pr_median ) / potatoes_price_pr_median, 
    onions_price_diff_perc = (onions_price_cr_median - onions_price_pr_median ) / onions_price_pr_median, 
    eggs_price_diff_perc = (eggs_price_cr_median - eggs_price_pr_median ) / eggs_price_pr_median, 
    cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr_median ) / cotton_cloth_price_pr_median, 
    toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr_median ) / toothbrush_adult_price_pr_median, 
    toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr_median  ) / toothpaste_price_pr_median, 
    sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr_median) / sanitary_pad_price_pr_median, 
    washing_detergent_price_diff_perc = (washing_detergent_price_cr_median - washing_detergent_price_pr_median  ) / washing_detergent_price_pr_median , 
    soap_price_diff_perc = (soap_price_cr_median - soap_price_pr_median) / soap_price_pr_median, 
    bleach_price_diff_perc = (bleach_price_cr_median - bleach_price_pr_median  ) / bleach_price_pr_median, 
    safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr_median ) / safe_water_price_pr_median, 
    firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr_median ) / firewood_price_pr_median, 
    kindling_price_diff_perc = (kindling_price_cr_median - kindling_price_pr_median ) / kindling_price_pr_median, 
    cooking_fuel_price_diff_perc = (cooking_fuel_price_cr_median - cooking_fuel_price_pr_median) / cooking_fuel_price_pr_median,
    pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr_median) / pulses_lentils_price_pr_median,
    pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr_median) / pulses_beans_price_pr_median,
    pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr_median) / pulses_split_peas_price_pr_median,
    usd_afn_buyrate_diff_perc = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median) / usd_afn_buyrate_pr_median,
    usd_afn_sellrate_diff_perc = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median) / usd_afn_sellrate_pr_median,
    usd_afn_buyrate_diff_abs = (usd_afn_buyrate_cr_median - usd_afn_buyrate_pr_median), 
    usd_afn_sellrate_diff_abs = (usd_afn_sellrate_cr_median - usd_afn_sellrate_pr_median)
  )

district_median_merged_price_diff_rounded <- district_median_merged_price_diff %>% 
  mutate_at(vars(ends_with("_diff_perc")), funs(round( . * 100, 2)))  

# prices in USD
# aggregate current round prices at District level - median
curr_data_district_median_USD <- curr_data %>%
  group_by(afg_dist) %>%
  summarise(
    wheat_local_price_cr_median = median(wheat_local_price_USD, na.rm = T),
    wheat_imported_price_cr_median = median(wheat_imported_price_USD, na.rm = T),
    local_rice_price_cr_median = median(local_rice_price_USD, na.rm = T),
    veg_oil_price_cr_median = median(veg_oil_price_USD, na.rm = T),
    pulses_merged_price_final_cr_median = median(pulses_merged_price_final_USD, na.rm = T),
    salt_price_cr_median = median(salt_price_USD, na.rm = T),
    sugar_price_cr_median = median(sugar_price_USD, na.rm = T),
    tomatoes_price_cr_median = median(tomatoes_price_USD, na.rm = T),
    potatoes_price_cr_median = median(potatoes_price_USD, na.rm = T),
    onions_price_cr_median = median(onions_price_USD, na.rm = T),
    eggs_price_cr_median = median(eggs_price_USD, na.rm = T),
    cotton_cloth_price_cr_median = median(cotton_cloth_price_USD, na.rm = T),
    toothbrush_adult_price_cr_median = median(toothbrush_adult_price_USD, na.rm = T),
    toothpaste_price_cr_median = median(toothpaste_price_USD, na.rm = T),
    sanitary_pad_price_cr_median = median(sanitary_pad_price_USD, na.rm = T),
    washing_detergent_price_cr_median = median(washing_detergent_price_USD, na.rm = T),
    soap_price_cr_median = median(soap_price_USD, na.rm = T),
    bleach_price_cr_median = median(bleach_price_USD, na.rm = T),
    safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
    safe_water_price_20L_cr_median = median(safe_water_price_20L_USD, na.rm = T),
    firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
    kindling_price_cr_median = median(kindling_price_USD, na.rm = T),
    cooking_fuel_price_cr_median = median(cooking_fuel_price_USD, na.rm = T),
  )

### MEB Median Calculation ### 

#fixed price items - AFN
#sanitary_pad <- 40
underwear <- 80
Healthcare <- 667
shelter <- 5850
#cotton_cloth <- 110 # National median round 4, due less datapoints


### National ###

meb_median_national <- data.frame(row.names = 1)
meb_median_national$level <- "National"

#AFN
meb_median_national$food_basket_AFN <- (curr_data_national_median$wheat_imported_price_cr_median * 60 ) +
                                  (curr_data_national_median$local_rice_price_cr_median * 29) +
                                  (curr_data_national_median$veg_oil_price_cr_median * 6) +
                                  (curr_data_national_median$pulses_merged_price_final_cr_median * 14) +
                                  (curr_data_national_median$salt_price_cr_median * 1) +
                                  (curr_data_national_median$sugar_price_cr_median * 6)

meb_median_national$nfi_basket_AFN <- (curr_data_national_median$soap_price_cr_median * 18) +
                                      (curr_data_national_median$cotton_cloth_price_cr_median * 2) +
                                      (curr_data_national_median$toothbrush_adult_price_cr_median * 3) +
                                      (curr_data_national_median$toothpaste_price_cr_median * 2) + curr_data_national_median$sanitary_pad_price_cr_median + underwear

meb_median_national$Healthcare_AFN <- Healthcare # Fixed (price not monitored)
meb_median_national$shelter_AFN <- shelter # Fixed (price not monitored)
meb_median_national$ten_percent_unmet_AFN <- (meb_median_national$food_basket_AFN + 
                                              meb_median_national$nfi_basket_AFN + 
                                              meb_median_national$Healthcare_AFN + 
                                              meb_median_national$shelter_AFN ) * 0.1

meb_median_national$meb_afn <- (meb_median_national$food_basket_AFN + meb_median_national$nfi_basket_AFN + 
                                    meb_median_national$Healthcare_AFN + meb_median_national$shelter_AFN) * 1.1
#USD
meb_median_national$food_basket_USD <- meb_median_national$food_basket_AFN / usd_rate
meb_median_national$nfi_basket_USD <- meb_median_national$nfi_basket_AFN / usd_rate
meb_median_national$Healthcare_USD <- meb_median_national$Healthcare_AFN / usd_rate
meb_median_national$shelter_USD <- meb_median_national$shelter_AFN / usd_rate

meb_median_national$ten_percent_unmet_USD <- (meb_median_national$food_basket_USD + 
                                              meb_median_national$nfi_basket_USD +
                                              meb_median_national$Healthcare_USD +
                                              meb_median_national$shelter_USD) * 0.1
meb_median_national$meb_usd <- meb_median_national$meb_afn / usd_rate


### Regional ###

meb_median_regional <- curr_data_region_median %>% 
  mutate(
    food_basket_AFN = (wheat_imported_price_cr_median * 60 ) +
                      (local_rice_price_cr_median * 29) +
                      (veg_oil_price_cr_median * 6) +
                      (pulses_merged_price_final_cr_median * 14) +
                      (salt_price_cr_median * 1) +
                      (sugar_price_cr_median * 6),
    nfi_basket_AFN =  (soap_price_cr_median * 18) +
                      (cotton_cloth_price_cr_median * 2) +
                      (toothbrush_adult_price_cr_median * 3) +
                      (toothpaste_price_cr_median * 2) + sanitary_pad_price_cr_median + underwear,
    Healthcare_AFN =  Healthcare,
    shelter_AFN    =  shelter,
    ten_percent_unmet_AFN = (food_basket_AFN + 
                             nfi_basket_AFN + 
                             Healthcare_AFN + 
                             shelter_AFN ) * 0.1,
    
    meb_afn = (food_basket_AFN + nfi_basket_AFN + 
                             Healthcare_AFN + shelter_AFN) * 1.1,
    food_basket_USD = food_basket_AFN / usd_rate,
    nfi_basket_USD  = nfi_basket_AFN / usd_rate,
    Healthcare_USD  = Healthcare_AFN / usd_rate,
    shelter_USD     = shelter_AFN / usd_rate,
    ten_percent_unmet_USD =    (food_basket_USD + 
                                nfi_basket_USD +
                                Healthcare_USD +
                                shelter_USD) * 0.1,
    meb_usd  = meb_afn / usd_rate
  ) %>% 
  select(afg_region, food_basket_AFN, nfi_basket_AFN,
         Healthcare_AFN, shelter_AFN, ten_percent_unmet_AFN,
         meb_afn,food_basket_USD, nfi_basket_USD, 
         Healthcare_USD, shelter_USD,ten_percent_unmet_USD, meb_usd )
  
  
### Provincial ###

meb_median_provincial <- curr_data_provice_median %>% 
  mutate(
    food_basket_AFN = (wheat_imported_price_cr_median * 60 ) +
      (local_rice_price_cr_median * 29) +
      (veg_oil_price_cr_median * 6) +
      (pulses_merged_price_final_cr_median * 14) +
      (salt_price_cr_median * 1) +
      (sugar_price_cr_median * 6),
    nfi_basket_AFN =  (soap_price_cr_median * 18) +
      (cotton_cloth_price_cr_median * 2) +
      (toothbrush_adult_price_cr_median * 3) +
      (toothpaste_price_cr_median * 2) + sanitary_pad_price_cr_median + underwear,
    Healthcare_AFN =  Healthcare,
    shelter_AFN    =  shelter,
    ten_percent_unmet_AFN = (food_basket_AFN + 
                               nfi_basket_AFN + 
                               Healthcare_AFN + 
                               shelter_AFN ) * 0.1,
    
    meb_afn = (food_basket_AFN + nfi_basket_AFN + 
                 Healthcare_AFN + shelter_AFN) * 1.1,
    food_basket_USD = food_basket_AFN / usd_rate,
    nfi_basket_USD  = nfi_basket_AFN / usd_rate,
    Healthcare_USD  = Healthcare_AFN / usd_rate,
    shelter_USD     = shelter_AFN / usd_rate,
    ten_percent_unmet_USD =    (food_basket_USD + 
                                  nfi_basket_USD +
                                  Healthcare_USD +
                                  shelter_USD) * 0.1,
    meb_usd  = meb_afn / usd_rate
  ) %>% 
  select(afg_prov, food_basket_AFN, nfi_basket_AFN,
         Healthcare_AFN, shelter_AFN, ten_percent_unmet_AFN,
         meb_afn,food_basket_USD, nfi_basket_USD, 
         Healthcare_USD, shelter_USD,ten_percent_unmet_USD, meb_usd )


#### Export Data ####

#### Median ####

# National
national_median <- list(
  Median_prices_AFN = curr_data_national_median,
  Median_price_diff = national_median_merged_price_diff_rounded,
  Median_prices_USD = curr_data_national_median_USD,
  Median_MEB = meb_median_national
)
write.xlsx(national_median, "results/median/National_Median.xlsx")

# Regional 
regional_median <- list(
  Median_prices_AFN = curr_data_region_median,
  Median_price_diff = region_median_merged_price_diff_rounded,
  Median_prices_USD = curr_data_region_median_USD,
  Median_MEB = meb_median_regional
)
write.xlsx(regional_median, "results/median/Regional_Median.xlsx")

# Provice 
province_median <- list(
  Median_prices_AFN = curr_data_provice_median,
  Median_price_diff = province_median_merged_price_diff_rounded,
  Median_prices_USD = curr_data_province_median_USD,
  Median_MEB = meb_median_provincial
)

write.xlsx(province_median, "results/median/Province_Median.xlsx") 

# District
district_median <- list(
  Median_prices_AFN = curr_data_district_median,
  Median_price_diff = district_median_merged_price_diff_rounded,
  Median_prices_USD = curr_data_district_median_USD
)
write.xlsx(district_median, "results/median/District_Median.xlsx") 

# Export recoded dataset for aggregating gategorical data
write.csv(curr_data, "input/data/recoded/AFG2002_JMMI_September_2020_recoded.csv", row.names = F)


















