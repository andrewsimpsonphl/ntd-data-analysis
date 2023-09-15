#### NTD RIDERSHIP ANALYSIS CODE ####

library(XML)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(directlabels)
library(readxl)
library(rphl)
library(scales)
library(grid)


#### DOWNLOAD NTD MOST RECENT FILES -------------------------------------------------------------------####

# Use to update all NTD data (no need to do daily but good to check for updates)
update_all_data <- function() {
  
  # pull lookup table of NTD ID to Agency Name
  agency_info_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/2021-05/2019%20Agency%20Info.xlsx"
  agency_info_file <- download.file(agency_info_url, "./inputs/agency_info_file", mode="wb")
  
  # Download Monthly Unlinked Passenger Trips 
  url_monthly_data <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/2023-09/July%202023%20Raw%20Monthly%20Ridership%20%28no%20adjustments%20or%20estimates%29.xlsx"
  ntd_monthly <- download.file(url_monthly_data, "./inputs/ntd_monthly", mode="wb")
  
  ##Download "TS2.1 - Service Data and Operating Expenses Time-Series by Mode" from FTA website
  url_TS2_1 <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/2023-06/TS2.1%20Service%20Data%20and%20Operating%20Expenses%20Time%20Series%20by%20Mode.xlsx"
  yearly_TS2_1_file <- download.file(url_TS2_1, "./inputs/ntd_yearly_file.xlsx", mode="wb") 
  
  # Note - 9/15/202 - needed to add mode="wb" to prevent downloaded excel file being corrupted
  
  #Download NTD Metrics data - NEED TO FIX - NOT USEFUL FOR SPEED...
  dowload_metric_data <- function(path = "./inputs/ntd_metric_data") {
    metrics_2021_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/2022-10/2021%20Metrics_static.xlsx"
    #metrics_2020_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/2020-Metrics.zip"
    metrics_2019_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics.xlsm"
    metrics_2018_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics_2.xlsm"
    metrics_2017_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics_1.xlsm"
    metrics_2016_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics_0.xlsm"
    metrics_2015_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics.xlsm"
    
    ntd_metric_2021_file <- download.file(metrics_2021_url, paste0(path, "/", 2021, ".xlsx"), mode="wb")
    #ntd_metric_2020_file <- download.file(metrics_2020_url, paste0(path, "/", 2020, ".xlsx"))
    ntd_metric_2019_file <- download.file(metrics_2019_url, paste0(path, "/", 2019, ".xlsx"), mode="wb")
    ntd_metric_2018_file <- download.file(metrics_2018_url, paste0(path, "/", 2018, ".xlsx"), mode="wb")
    ntd_metric_2017_file <- download.file(metrics_2017_url, paste0(path, "/", 2017, ".xlsx"), mode="wb")
    ntd_metric_2016_file <- download.file(metrics_2016_url, paste0(path, "/", 2016, ".xlsx"), mode="wb")
    ntd_metric_2015_file <- download.file(metrics_2015_url, paste0(path, "/", 2015, ".xlsx"), mode="wb")
  }
  
  dowload_metric_data() # not being use for any actual analysis now
}

load_metric_data <- function(path = "./inputs/ntd_metric_data", agency_info_data) {
  file_list <- list.files(path,
                          pattern = "*.xlsx", full.names = T)
  
  agency_info_data <- agency_info_data %>% select(c(`NTD ID`, `Agency Name`))
  
  
  df <- sapply(file_list, read_excel, c(sheet = 3), simplify=FALSE) %>% 
    bind_rows(.id = "years") %>% 
    select(1:40) %>% 
    mutate(year = regmatches(years, gregexpr("[[:digit:]]+", years))) %>% 
    left_join(agency_info_data) %>% 
    select(c(`NTD ID`, `Agency Name`, `City`, year, `Mode`, `Reporter Type`, `TOS`, `Fare Revenues per Unlinked Passenger Trip`:`Any data questionable?`))
  
  clean_metric_data <- function(metric_data) {
    output <- metric_data %>%
      filter(`Reporter Type` == "Full Reporter") %>%
      filter(`Any data questionable?` == "No") %>%
      mutate("vrm_per_vrh" = `Vehicle Revenue Miles` / `Vehicle Revenue Hours`) %>%
      mutate(Mode = recode(Mode, 
                           "MB" = "Bus", 
                           "CR" = "Commuter Rail", 
                           "HR" = "Heavy Rail", 
                           "LR" = "Trolley", 
                           "SR" = "Trolley", 
                           "CB" = "Commuter Bus", 
                           "DR" = "Demand Response", 
                           "TB" = "Bus")) %>%
      #mutate(Name = as.factor(`Name`)) %>%
      mutate(year = unlist(year)) %>% 
      group_by(`NTD ID`, `Agency Name`, City, Mode, year) %>% # uses group/summarise to combined directly operated and purchased transit
      summarise(vrm = sum(`Vehicle Revenue Miles`, na.rm = TRUE),
                "Vehicle Miles per Revenue Hour" = weighted.mean(vrm_per_vrh, `Vehicle Revenue Miles`), 
                "Unlinked Passenger Trips" = sum(`Unlinked Passenger Trips`)) %>%
      mutate(highlight_flag = ifelse(City == 'Philadelphia', T, F))
    
    return(output)
  }
  clean_ntd_metric_dat <- clean_metric_data(df)
  
  return(clean_ntd_metric_dat)
}

# Function to bring in VRM, VRH, UPT, Opex, and Fare data for every agency
load_yearly_data <- function(file_path = "./inputs/ntd_yearly_file.xlsx", Year1 = 2002, Year2 = 2021) {
  ntd_yearly_vrm_data <- read_excel(file_path, sheet = "VRM")
  ntd_yearly_vrh_data <- read_excel(file_path, sheet = "VRH")
  ntd_yearly_upt_data <- read_excel(file_path, sheet = "UPT")
  ntd_yearly_pmt_data <- read_excel(file_path, sheet = "PMT")
  ntd_yearly_opex_data <- read_excel(file_path, sheet = "OpExp Total")
  ntd_yearly_fares_data <- read_excel(file_path, sheet = "FARES")
  
  yearly_vrm <- ntd_yearly_vrm_data %>%
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `Mode Status`, `UZA Name`, Service, `1991`:`2021`)) %>% # need to fix hard coded year
    group_by(Mode) %>%
    gather(key = "Year", value = "vrm", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(vrm = sum(vrm, na.rm = TRUE))
  
  #gather a dataframe to get yearly fare revenue for each mode of all agencies
  yearly_vrh <- ntd_yearly_vrh_data %>% 
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2021`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "vrh", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(vrh = sum(vrh, na.rm = TRUE))
  
  #gather a dataframe to get yearly ridership for each mode of all agencies
  yearly_upt <- ntd_yearly_upt_data %>%
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2021`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "upt", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(upt = sum(upt, na.rm = TRUE))
  
  yearly_pmt <- ntd_yearly_upt_data %>%
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2021`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "pmt", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(pmt = sum(pmt, na.rm = TRUE))
  
  yearly_opex <- ntd_yearly_opex_data %>%
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `Mode Status`, `UZA Name`, Service, `1991`:`2021`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "operating_expense", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(operating_expense = sum(operating_expense, na.rm = TRUE))
  
  #gather a dataframe to get yearly fare revenue for each mode of all agencies
  yearly_fare_revenue <- ntd_yearly_fares_data %>% 
    mutate(Mode=recode(Mode, 
                       "MB" = "Bus", 
                       "CR" = "Commuter Rail", 
                       "HR" = "Heavy Rail", 
                       "LR" = "Trolley", 
                       "SR" = "Trolley",
                       "CB" = "Commuter Bus", 
                       "DR" = "Demand Response", 
                       "TB" = "Bus", 
                       "RB" = "Bus Rapid Transit", 
                       "IP" = "Incline Plane", 
                       "DT" = "Demand Response Taxi", 
                       "VP" = "Vanpool", 
                       "FB" = "Ferry Bus")) %>%
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2021`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "fare_revenue", c(`1991`:`2021`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(fare_revenue = sum(fare_revenue, na.rm = TRUE))
  
  output <- yearly_vrm %>%
    left_join(yearly_vrh) %>%
    left_join(yearly_upt) %>%
    left_join(yearly_pmt) %>%
    left_join(yearly_opex) %>%
    left_join(yearly_fare_revenue) %>%
    mutate(vrm_by_vrh = vrm / vrh ) %>% 
    mutate(recovery_ratio = fare_revenue / operating_expense ) %>% 
    filter(is.na(`Agency Name`) == FALSE) %>% 
    filter(!is.na(vrm) & vrm > 0) %>% 
    filter(!is.na(operating_expense) & operating_expense > 0) %>% 
    filter(Year >= Year1 & Year <= Year2) %>% 
    mutate(highlight_flag = ifelse(`UZA Name` == 'Philadelphia, PA-NJ-DE-MD', T, F))
  
  return(output)
}

#Function to bring in monthly UPT data for every agency
load_monthly_upt <- function(path= "./inputs/ntd_monthly_upt_file.xlsx") {
  data_file <- read_excel(path, sheet = 3) #datasheet with monthly UPT for each agency and mode from Jan 2002 to Sept 2019
  
  hold <- data_file %>%
    mutate(Modes=recode(Modes, "MB" = "Bus", "CR" = "Commuter Rail", "HR" = "Heavy Rail", 
                        "LR" = "Trolley", "SR" = "Trolley", "CB" = "Commuter Bus", 
                        "DR" = "Demand Response", "TB" = "Bus", "RB" = "Bus Rapid Transit", 
                        "IP" = "Incline Plane", "DT" = "Demand Response Taxi", "VP" = "Vanpool", 
                        "FB" = "Ferry Bus")) %>%
    group_by(Modes) %>%
    gather(key = "month", value = "ridership", c(-Agency, -'5 digit NTD ID',-'4 digit NTD ID', -Modes, -Active, -`Reporter Type`, -UZA, -`UZA Name`, -TOS), convert = TRUE)
  
  hold$month <- parse_date_time(hold$month, orders = "my")
  hold <- separate(hold, "month", c("Year", "Month", "Day"))
  
  output <- hold %>%
    filter(is.na(Agency) == FALSE) 
}

#### READ DATA INTO ENVIRONMENT-----------------------------------------------------------------------####

# Call when you want to redownload all the NTD data into the file path:
# update_all_data()

# Key assumptions: 
# Bus = Motorbus and Trolley Bus operations
# Trolley = Streetcar Rail and Light rail operations (becasue SEPTA switched their class from Light Rail to Streetcar in mid 00's)
ntd_yearly <- load_yearly_data(Year1 = 2002, Year2 = 2021)
agency_info_data <- read_excel("./inputs/agency_info_file", sheet = 1)
metric_data <- load_metric_data(agency_info_data = agency_info_data) #note 2020 data is not included
upt_monthly <- load_monthly_upt()

#### YEARLY UPT ANALYSIS ########

#agency bus ridership
plot_agency_mode_yearly <- function(ntd_yearly, agency_name = "Southeastern Pennsylvania Transportation Authority(SEPTA)", 
                                    mode = "Bus", Year1 = 2003, Year2 = 2018) {
  d <- ntd_yearly %>%
    group_by(`Agency Name`, `Mode`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter(Year >= Year1 & Year <= Year2 & `Agency Name` == agency_name & `Mode` == mode)
  
  p <- ggplot(d, aes(x = Year, y = upt, fill = Mode)) + geom_col() + 
    geom_text(aes(label = paste(format(round(upt / 1e6, 1), trim = TRUE), "M")), position = position_stack(vjust = 0.5), cex = (3)) + 
    scale_y_continuous("Annual Ridership", labels = scales::comma) + scale_fill_brewer(palette = "Paired") + 
    labs(title = paste(agency_name, "Yearly Ridership on", mode)) + theme(legend.position = "none") +
    theme_phl()
  
  return (p)
}

septa_name <- "Southeastern Pennsylvania Transportation Authority (SEPTA)"
septa_bus_yearly <- plot_agency_mode_yearly(ntd_yearly, septa_name, "Bus", 2002, 2019)
septa_subel_yearly <- plot_agency_mode_yearly(ntd_yearly, septa_name, "Heavy Rail", 2002, 2019)
septa_regional_yearly <- plot_agency_mode_yearly(ntd_yearly, septa_name, "Commuter Rail", 2002, 2019)
septa_trolley_yearly <- plot_agency_mode_yearly(ntd_yearly, septa_name, "Trolley", 2002, 2019)

plot_agency_stackedmodes_yearly <- function(ntd_yearly, agency_name, Year1, Year2) {
  d <- ntd_yearly %>%
    group_by(`Agency Name`, `Mode`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter(Year >= Year1 & Year <= Year2 & `Agency Name` == agency_name)
  
  p <- ggplot(d, aes(x = Year, y = upt, fill = Mode)) + geom_col() + 
    geom_text(aes(label = paste(format(round(upt / 1e6, 1), trim = TRUE), "M")), position = position_stack(vjust = 0.5), cex = (3)) + 
    scale_y_continuous("Annual Ridership", labels = scales::comma) + scale_fill_brewer(palette = "Paired") + 
    labs(title = paste(agency_name, "Yearly Ridership by Mode")) + theme(legend.position = "none") +
    theme_phl()
  return (p)
}

septa_modal_yearly_ridership <- plot_agency_stackedmodes_yearly(ntd_yearly, septa_name, Year1 = 2002, Year2 = 2019)

plot_agency_stackedmodes_yearly_pct <- function(ntd_yearly, agency_name, Year1, Year2) {
  d <- ntd_yearly %>%
    group_by(`Agency Name`, `Mode`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter(Year >= Year1 & Year <= Year2 & `Agency Name` == agency_name)
  
  p <- ggplot(d, aes(x = Year, y = upt, fill = Mode)) + geom_col(position = 'fill', stat = 'identity') + 
    scale_y_continuous("Annual Ridership", labels = scales::percent) + theme_phl() + scale_fill_brewer(palette = "Paired") +
    labs(title = paste(agency_name, " Yearly Ridership by Mode"))
  return (p)
}
septa_yearly_stacked <- plot_agency_stackedmodes_yearly_pct(ntd_yearly, septa_name, 2002, 2019)

#### FIND TOP X CITIES/AGENCIES HELPER FUNCTIONS 
find_top_x_cities <- function(data, number_cities, data_year) {
  dat <- data %>% 
    group_by(`UZA Name`, Year) %>% 
    summarise(upt = sum(`upt`)) %>% 
    filter(`Year` == data_year) %>% 
    arrange(-`upt`) %>% 
    head(number_cities) %>% 
    ungroup() %>% 
    select(`UZA Name`) %>% 
    as.list()
  
  list <- dat$`UZA Name`
  
  return(list)
}
find_top_x_cities_bymode <- function(data, number_cities, mode, data_year) {
  dat <- data %>% 
    group_by(`UZA Name`, Mode, Year) %>% 
    summarise(upt = sum(`upt`)) %>% 
    filter(Mode == mode & `Year` == data_year) %>% 
    arrange(-`upt`) %>% 
    head(number_cities) %>% 
    ungroup() %>% 
    select(`UZA Name`) %>% 
    as.list()
  
  list <- dat$`UZA Name`
  
  return(list)
}
find_top_x_agencies_bymode <- function(data, number_agencies, mode, data_year) {
  dat <- data %>% 
    group_by(`NTD ID`, `Agency Name`, Mode, Year) %>% 
    summarise(upt = sum(`upt`)) %>% 
    filter(Mode == mode & `Year` == data_year) %>% 
    arrange(-`upt`) %>% 
    head(number_agencies) %>% 
    ungroup() %>% 
    select(`Agency Name`) %>% 
    as.list()
  
  list <- dat$`Agency Name`
  
  return(list)
}
top_15_transit_cities_2017 <- find_top_x_cities(ntd_yearly, 15, 2017)
top_15_bus_cities_2017 <- find_top_x_cities_bymode(ntd_yearly, 15, "Bus", 2017)
top_15_bus_agencies_2017 <- find_top_x_agencies_bymode(ntd_yearly, 15, "Bus", 2017)


# plot line graph of yearly average, taking in a list of agencies (peer uzas), begging year, end year
plot_agencies_yearly_ridership <- function(ntd_yearly, agency_list, Year1, Year2, title_on = TRUE) {
  x <- ntd_yearly %>%
    group_by(`Agency Name`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter(`Agency Name` %in% agency_list) %>% # only map to agency list
    filter(Year >= Year1 & Year <= Year2) #select rows between years

  p <- ggplot(x, aes(x = Year, y = upt, colour =  `Agency Name`, group = `Agency Name`, labels = `Agency Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership (All Modes)", labels = scales::comma) + 
    scale_x_discrete(expand = expand_scale(mult = c(0, 0.4))) +
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    geom_dl(aes(label = `Agency Name`), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    scale_color_discrete(guide = 'none') + 
    theme_phl()
    
  
  if(title_on == TRUE) {
    return( p + labs(title = paste("Yearly Ridership by Similar Agencies")))
  }
  else { return(p)}
}
yearly_ridership_similar_agencies <- plot_agencies_yearly_ridership(ntd_yearly, top_15_bus_agencies_2017[2:7], 2010, 2017) # Drop NYC - too much ridership!


plot_uza_yearly_ridership <- function(ntd_yearly, uza_list, Year1, Year2, title_on = TRUE) {
  x <- ntd_yearly %>%
    group_by(`UZA Name`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter((`UZA Name` %in% uza_list & !is.na(`UZA Name`))) %>% # only map top 20 UZAs
    filter(Year >= Year1 & Year <= Year2) #select rows between years
  
  p <- ggplot(x, aes(x = Year, y = upt, colour =  `UZA Name`, group = `UZA Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership (All Modes)", labels = scales::comma) + 
    scale_x_discrete(expand = expand_scale(mult = c(0, 0.4))) +
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    #theme(plot.margin = margin(20, 50, 0, 0, 'pt')) +
    geom_dl(aes(label = `UZA Name`), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    scale_color_discrete(guide = 'none') +
    geom_hline(aes(yintercept = 1)) + 
    theme(plot.margin = unit(c(1,3,1,1), "lines")) +
    theme_phl()
  
  if(title_on == TRUE) { 
    return( p + labs(title = paste("Yearly Ridership by Metro Area (all regional transit providers)")))
    }
  else { return(p) }
}
top_7_transit_uzas_2019 <- find_top_x_cities(ntd_yearly, 7, 2019)
yearly_ridership_similar_UZAS <- plot_uza_yearly_ridership(ntd_yearly, top_7_transit_uzas_2019[2:7], 2008, 2019, title_on = TRUE)

plot_uza_yearly_ridership_indexed <- function(ntd_yearly, uza_list, Year1, Year2, title_on = TRUE) {
  x <- ntd_yearly %>%
    group_by(`UZA Name`, `Year`) %>% 
    summarise(upt = sum(upt)) %>% 
    filter(Year >= Year1 & Year <= Year2) %>% #select rows between years
    #find_yearly_ridership_by_UZA() %>% #drill down to necessary values
    filter(upt > 0) %>%
    filter((`UZA Name` %in% uza_list & !is.na(`UZA Name`))) %>% # only map top 20 UZAs
    mutate(delta = upt / first(upt))
  
  p <- ggplot(x, aes(x = Year, y = delta, colour =  `UZA Name`, group = `UZA Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership (All Modes), Indexed", 
                       labels = scales::percent_format(),
                       limits = c(min(x$delta) - 0.05, max(x$delta + 0.05))) + 
    scale_x_discrete(expand = expand_scale(mult = c(0, 0.4))) +
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    geom_dl(aes(label = `UZA Name`), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    scale_color_discrete(guide = 'none') +
    geom_hline(aes(yintercept = 1)) + 
    theme(plot.margin = unit(c(1,3,1,1), "lines"))  +
    theme_phl()
  
  p$layout$clip[p$layout$name == "panel"] <- "off"
  
  if(title_on == TRUE) { 
    return( p + labs(title = paste("Yearly Ridership by Metro Area (All Providers)")))
  }
  else { return(p) }
  
  return(p)
}
similar_uza_indexed_ridership <- plot_uza_yearly_ridership_indexed(ntd_yearly, top_15_transit_cities_2017, 2008, 2019)


#### SPEED ANALYSIS ####

# need to fix to use years correctly
plot_uza_speeds <- function(ntd_yearly, uza_list = find_top_x_cities_bymode(ntd_yearly, 15, "Bus", 2019), 
                             mode, minimum_UPT = 0, maximum_UPT = Inf, data_year = 2019, title_on = TRUE) {
  
  dat <- ntd_yearly %>%
    filter(`UZA Name` %in% uza_list & !is.na(`UZA Name`)) %>% 
    filter(`Mode` == mode & `Year` == data_year) %>% 
    filter(`upt` > minimum_UPT & `upt` < maximum_UPT) %>% 
    group_by(`UZA Name`, highlight_flag) %>% 
    summarise(upt = sum(upt, na.rm = TRUE),
              `Vehicle Miles per Revenue Hour` = weighted.mean(vrm_by_vrh, vrm)) # weight the speed by the number of miles traveled when combining multiple operators
  
  plot <- ggplot(dat, aes(x = reorder(`UZA Name`, -`Vehicle Miles per Revenue Hour`), 
                          y = `Vehicle Miles per Revenue Hour`)) + 
    geom_col(aes(fill = highlight_flag)) +
    xlab("Uranized Area") + 
    ylab(paste0("Vehicle Miles per Revenue Hour ", data_year)) + 
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")
  
  if(title_on == TRUE) { 
    return(plot + ggtitle(paste0("Philadelphia Among Slowest Peer ", mode, " Systems (", data_year,  " Data)")))
  }
  else{ return(plot) }
  
  return(plot)
}

plot_agency_speeds <- function(ntd_yearly, agency_list = find_top_x_agencies_bymode(ntd_yearly, 15, "Bus", 2017), 
                               mode, minimum_UPT = 0, maximum_UPT = Inf, data_year = 2019, title_on = TRUE) {
  dat <- ntd_yearly %>%
    filter(`Agency Name` %in% agency_list) %>% 
    filter(`Mode` == mode & `Year` == data_year) %>% 
    filter(`upt` > minimum_UPT & `upt` < maximum_UPT) %>% 
    group_by(`Agency Name`, highlight_flag) %>% 
    summarise(upt = sum(`upt`, na.rm = TRUE),
              `Vehicle Miles per Revenue Hour` = weighted.mean(`vrm_by_vrh`, `vrm`)) # weight the speed by the number of miles traveled when combining multiple operators
  
  plot <- ggplot(dat, aes(x = reorder(`Agency Name`, -`Vehicle Miles per Revenue Hour`), 
                          y = `Vehicle Miles per Revenue Hour`)) + 
    geom_col(aes(fill = highlight_flag)) +
    xlab("Agency") + 
    ylab(paste0("Vehicle Miles per Revenue Hour (", data_year, ")")) +
    theme_linedraw() + scale_fill_brewer(palette = "Paired") +
    rphl::theme_phl()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")
  
  if(title_on == TRUE) { 
      return(plot + ggtitle(paste0("SEPTA Among Slowest Peer ", mode, " Systems (", data_year,  " Data)")))
    }
  else{ return(plot) }
}

plot_uza_speeds(ntd_yearly, 
                  uza_list = find_top_x_cities_bymode(ntd_yearly, 15, "Bus", 2017), 
                  mode = "Bus", 
                  minimum_UPT = 0, 
                  data_year = 2017,
                  title_on = FALSE)

plot_agency_speeds(ntd_yearly, 
                  agency_list = find_top_x_agencies_bymode(ntd_yearly, 15, "Bus", 2017), 
                  mode = "Bus", 
                  minimum_UPT = 0, 
                  data_year = 2017, 
                  title_on = TRUE)

plot_speed_top_x_mode_cities <- function(ntd_yearly, mode, number_cities, minimum_UPT, maximum_upt, data_year) {
  uza_list <- find_top_x_cities_bymode(ntd_yearly, number_cities, mode, data_year)
  
  plot <- plot_uza_speeds(ntd_yearly, 
                           uza_list = uza_list, 
                    mode = mode, 
                    data_year = data_year) +
    labs(title = paste0(number_cities, " Largest ", mode, " Regions, Ordered by Effective Speed (", data_year, ")"))
  
  return(plot)
}
plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 15, data_year = 2017)

plot_speed_top_x_mode_agencies <- function(ntd_yearly, mode, number_agencies, minimum_UPT, maximum_upt, data_year) {
  agency_list <- find_top_x_agencies_bymode(ntd_yearly, number_agencies, mode, data_year)
  
  plot <- plot_agency_speeds(ntd_yearly, 
                           agency_list = agency_list, 
                           mode = mode, 
                           data_year = data_year) + 
    labs(title = paste0(number_agencies, " Largest ", mode, " Agencies, Ordered by Effective Speed (", data_year, ")"))
  
  return(plot)
}
plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2017)
plot_speed_top_x_mode_agencies(ntd_yearly, "Bus", 15, data_year = 2017)
plot_speed_top_x_mode_agencies(ntd_yearly, "Bus", 15, data_year = 2021)

largest_15agency_bus_speeds_2019 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Bus", 15, data_year = 2019)
largest_15agency_bus_speeds_2019

rail_2015 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2015)
rail_2016 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2016)
rail_2017 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2017)
rail_2018 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2018)
rail_2019 <- plot_speed_top_x_mode_agencies(ntd_yearly, "Commuter Rail", 6, data_year = 2019)

rail_2015
rail_2016
rail_2017
rail_2018
rail_2019

largest_15cities_bus_speeds_2019 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 15, data_year = 2019)

bus_2015 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 15, data_year = 2015)
bus_2016 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 20, data_year = 2016)
bus_2017 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 20, data_year = 2017)
bus_2018 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 20, data_year = 2018)
bus_2019 <- plot_speed_top_x_mode_cities(ntd_yearly, "Bus", 20, data_year = 2019)

grid.newpage()
grid.draw(rbind(ggplotGrob(bus_2015), ggplotGrob(bus_2016), ggplotGrob(bus_2017), ggplotGrob(bus_2018),ggplotGrob(bus_2019),size = "last"))


plot_yearly_agency_mode_speed <- function(ntd_yearly, ntd_id = 30019, mode = "Bus") {
  dat <- ntd_yearly %>% filter(`NTD ID` == ntd_id & `Mode` == mode)
  
  plot <- ggplot(dat, aes(x = Year, y = `vrm_by_vrh`)) +
            geom_line(group = 1) +
            scale_y_continuous(name = "Vehicle Revenue Miles per Revenue Hour", limits = c(min(dat$vrm_by_vrh) - 0.1 * min(dat$vrm_by_vrh),
                                          max(dat$vrm_by_vrh) + 0.1 * max(dat$vrm_by_vrh))) +
            scale_x_discrete(name = "Year")
        
  return(plot)
}
septa_bus_speeds_plot <- plot_yearly_agency_mode_speed(ntd_yearly, 30019, "Bus")
septa_CR_speeds_plot <- plot_yearly_agency_mode_speed(ntd_yearly, 30019, mode = "Commuter Rail")

septa_bus_speeds_plot
septa_CR_speeds_plot

# Datasets for Andy
septa_bus_speeds <- septa_bus_speeds_plot$data
peer_city_bus_speeds <- largest_15cities_bus_speeds_2019$data
peer_agency_bus_speeds <- largest_15agency_bus_speeds_2019$data
septa_modal_yearly_ridership_data <- septa_modal_yearly_ridership$data

write_csv(septa_bus_speeds, "./outputs/data/septa_bus_speeds_yearly.csv")
write_csv(peer_city_bus_speeds, "./outputs/data/peer_city_bus_speeds.csv")
write_csv(peer_agency_bus_speeds, "./outputs/data/peer_agency_bus_speeds.csv")
write_csv(septa_modal_yearly_ridership_data, "./outputs/data/septa_yearly_bymode.csv")

#### PLOT RECOVERY RATIO FOR SEPTA REGIONAL RAIL (LINE CHART) ####

# FUNCTION: clean_ntd_yearly_data
# Paramaters: 
# OpExp_file = ntd operating expenses by year for each agency and mode
# Fares_files = ntd fare revenue by year for each agency and mode
# Year1/Year2 = (inclusive) filter of years to return
# returns a clean dataframe of yearly NTD data - currenlty just operating expensis and fare revenue

#clean_ntd_yearly <- clean_ntd_yearly_data(ntd_yearly_OpExp_data, ntd_yearly_Fares_data, ntd_yearly_ridership_data, Year1 = 2002, Year2 = 2018)

plot_yearly_recovery_bymode <- function(ntd_yearly, NTD_ID = 30019, mode = "Bus") {
  recovery_dat <- ntd_yearly %>% filter(`NTD ID` == NTD_ID) %>% filter(Mode == mode) %>% 
    group_by(Year)
  
  agency_name <- recovery_dat$`Agency Name` %>% unique()
  
  plot <- ggplot(recovery_dat , aes(x = `Year`, y = `recovery_ratio`, group = 1)) +
    geom_line(colour = "azure4") +
    geom_point(size=2, colour = "blue4") +
    scale_y_continuous(labels = scales::label_percent(scale = 100), 
                       name = paste(mode, "Recovery Ratio (%)", sep = " "),
                       limits = c(min(recovery_dat$recovery_ratio) - .05, 
                                max(recovery_dat$recovery_ratio) + .05)) +
    labs(title = paste(agency_name, mode, "Fare Recovery Ratio", sep = " ")) +
    theme_phl()
  #+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) #Enable this line if you want to get rid of the first axis lables when combining two graphs 
  
  return(plot)
}
rail_recovery_yearly <- plot_yearly_recovery_bymode(ntd_yearly, mode = "Commuter Rail")
rail_recovery_yearly

# PLOT YEARLY RIDERSHIP FOR SEPTA REGIONAL RAIL (LINE CHART)

plot_yearly_ridership_bymode <- function(ntd_yearly, NTD_ID = 30019, mode = "Bus") {
  recovery_dat <- ntd_yearly %>% filter(`NTD ID` == NTD_ID) %>% filter(Mode == mode) %>% 
    group_by(Year)
  
  agency_name <- recovery_dat$`Agency Name` %>% unique()
  
  plot <- ggplot(recovery_dat , aes(x = `Year`, y = `upt`, group = 1)) +
    geom_line(colour = "azure4") +
    geom_point(size=2, colour = "blue4") +
    ylab("Commuter Rail Yearly Ridership") +
    labs(title = paste(agency_name, mode, "Yearly Ridership", sep = " ")) +
    scale_y_continuous(labels = comma,
                       limits = c(min(recovery_dat$upt) - .05 * min(recovery_dat$upt), 
                                  max(recovery_dat$upt) + .05 * max(recovery_dat$upt))) + #adding commas to the numbers on y axis
    theme_phl()
  #+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) #Enable this line if you want to get rid of the first axis lables when combining two graphs 
  
  return(plot)
}

rail_ridership_yearly <- plot_yearly_ridership_bymode(ntd_yearly, mode = "Commuter Rail")
rail_ridership_yearly

#library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(rail_recovery_yearly), ggplotGrob(rail_ridership_yearly), size = "last"))



