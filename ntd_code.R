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

peer_codes <- c(90154, 50066, 30030, 30019, 10003, 30034, 1) #WMATA, MBTA, SEPTA, Batltimore, Seattle, CTA
peer_uzas <- c(1:10) #10 largest UZAs excluding NYC
septa_name <- "Southeastern Pennsylvania Transportation Authority"
nyc_name <- "MTA New York City Transit"


#### DOWNLOAD NTD MOST RECENT FILES -------------------------------------------------------------------####
#TO DO: MAKE THIS A SINGLE FUNCTION

# Download Monthly Unlinked Passenger Trips 
url_monthly_upt <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/September%202019%20Adjusted%20Database.xlsx"
#ntd_monthly_upt <- download.file(url_monthly_upt, "./inputs/ntd_monthly_upt_file")

##Download "TS2.1 - Service Data and Operating Expenses Time-Series by Mode" from FTA website
url_TS2_1 <- "https://cms7.fta.dot.gov/sites/fta.dot.gov/files/TS2.1TimeSeriesOpExpSvcModeTOS_2.xlsx"
#yearly_upt_file <- download.file(url_TS2_1, "./inputs/ntd_yearly_upt_file")

#Download NTD Metrics data
metrics_2018_url <- "https://cms7.fta.dot.gov/sites/fta.dot.gov/files/Metrics_2.xlsm"
ntd_metric_2018_file <- download.file(metrics_2018_url, "./inputs/ntd_metric_2018_file")
metrics_2017_url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/Metrics_1.xlsm"
ntd_metric_2017_file <- download.file(metrics_2017_url, "./inputs/ntd_metric_2017_file")
metrics_2016_url <- "https://cms7.fta.dot.gov/sites/fta.dot.gov/files/Metrics_0.xlsm"
ntd_metric_2016_file <- download.file(metrics_2016_url, "./inputs/ntd_metric_2016_file")
metrics_2015_url <- "https://cms7.fta.dot.gov/sites/fta.dot.gov/files/Metrics.xlsm"
ntd_metric_2015_file <- download.file(metrics_2015_url, "./inputs/ntd_metric_2015_file")


#### READ DATA INTO ENVIRONMENT-----------------------------------------------------------------------####
ntd_monthly_data <- read_excel("./inputs/ntd_monthly_upt_file", sheet = 3) #datasheet with monthly UPT for each agency and mode from Jan 2002 to Sept 2019
ntd_metric_2018_data <- read_excel("./inputs/ntd_metric_2018_file", sheet = 3)
ntd_metric_2017_data <- read_excel("./inputs/ntd_metric_2017_file", sheet = 3)
ntd_metric_2016_data <- read_excel("./inputs/ntd_metric_2016_file", sheet = 3)
ntd_metric_2015_data <- read_excel("./inputs/ntd_metric_2015_file", sheet = 3)#datasheet with metrics data by agencies - for national speed anlysis
ntd_yearly_OpExp_data <- read_excel("./inputs/ntd_yearly_upt_file", sheet = 3) #datasheet with yearly Operating Expenses for each agency and mode from 1991 to 2018
ntd_yearly_Fares_data <- read_excel("./inputs/ntd_yearly_upt_file", sheet = 8) #datasheet with yearly Fare Revenues for each agency and mode from  2002 to 2018
ntd_yearly_ridership_data <- read_excel("./inputs/ntd_yearly_upt_file", sheet = 13)

#### UPT ANALYSIS ########
# TIDY UP NTD DATA
clean_ntd_monthly_data <- function(data_file) {
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

clean_ntd_monthly <- clean_ntd_monthly_data(ntd_monthly_data)

  
septa <- clean_ntd_monthly %>%
  filter(Agency == "Southeastern Pennsylvania Transportation Authority")

# FUNCTIONAL CODES

find_yearly_ridership_by_AgencyMode <- function(data) {
  output <- data %>%
    group_by(Agency, `UZA Name`, Modes, Year) %>%
    summarise(ridership = sum(ridership, na.rm = TRUE))
  
  return(output)
}
test <- find_yearly_ridership_by_AgencyMode(clean_ntd_monthly)

find_yearly_ridership_by_Mode <- function(data) {
  output <- data %>%
    group_by(Modes,  `UZA Name`, Year) %>%
    summarise(ridership = sum(ridership, na.rm = TRUE))
  
  return(output)
}
test <- find_yearly_ridership_by_Mode(clean_ntd_monthly)

find_yearly_ridership_by_Agency <- function(data) {
  output <- data %>%
    group_by(Agency,  `UZA Name`, Year) %>%
    summarise(n = n(), ridership = sum(ridership, na.rm = TRUE))
  return(output)
}
test <- find_yearly_ridership_by_Agency(clean_ntd_monthly)

find_yearly_ridership_by_UZA <- function(data) {
  output <- data %>%
    group_by(UZA, `UZA Name`, Year) %>%
    summarise(n = n(), ridership = sum(ridership, na.rm = TRUE))
  return(output)
}
test <- find_yearly_ridership_by_UZA(clean_ntd_monthly)

find_yearly_ridership_by_specific_Agency <- function(data, agency_name) {
  output <- data %>%
    filter(Agency == agency_name) %>%
    find_yearly_ridership_by_Agency()
  
  return(output)
}
test <- find_yearly_ridership_by_specific_Agency(clean_ntd_monthly, "Southeastern Pennsylvania Transportation Authority")

find_yearly_modal_ridership_by_specific_Agency <- function(data, agency_name) {
  output <- data %>%
    filter(Agency == agency_name) %>%
    find_yearly_ridership_by_AgencyMode()
  
  return(output)
}

find_yearly_ridership_by_specific_Agency_Mode <- function(data, agency_name, mode) {
  output <- data %>%
    filter(Agency == agency_name & Modes == mode) %>%
    find_yearly_ridership_by_AgencyMode()
  
  return(output)
}
test <- find_yearly_ridership_by_specific_Agency_Mode(clean_ntd_monthly, "Southeastern Pennsylvania Transportation Authority", "Bus")

#### CALENDAR YEAR ANNUAL RIDERSHIP PLOTS ####
#agency bus ridership
plot_agency_mode_yearly <- function(data, agency, mode, year1, year2) {
  d <- data %>%
    filter(Year >= year1 & Year <= year2) %>%
    find_yearly_ridership_by_specific_Agency_Mode(agency, mode)
  p <- ggplot(d, aes(x = Year, y = ridership, fill = Modes)) + geom_col() + 
    geom_text(aes(label = paste(format(round(ridership / 1e6, 1), trim = TRUE), "M")), 
              position = position_stack(vjust = 0.5), cex = (3)) + 
    scale_y_continuous("Annual Ridership", labels = scales::comma) + scale_fill_brewer(palette = "Paired") + 
    labs(title = paste(agency, "Yearly Ridership on", mode)) + theme(legend.position = "none")
  return (p)
}
plot_agency_mode_yearly(clean_ntd_monthly, septa_name, "Bus", 2002, 2018)
plot_agency_mode_yearly(clean_ntd_monthly, septa_name, "Heavy Rail", 2002, 2018)
plot_agency_mode_yearly(clean_ntd_monthly, septa_name, "Commuter Rail", 2002, 2019)
plot_agency_mode_yearly(clean_ntd_monthly, septa_name, "Streetcar Rail", 2012, 2018)

plot_agency_stackedmodes_yearly <- function(data, agency, year1, year2) {
  d <-  data %>%
    filter(Year >= year1 & Year <= year2) %>%
    filter(Modes != "Demand Response") %>%
    find_yearly_modal_ridership_by_specific_Agency(agency)
  p <- ggplot(d, aes(x = Year, y = ridership, fill = Modes)) + geom_col() + 
    geom_text(aes(label = paste(format(round(ridership / 1e6, 1), trim = TRUE), "M")), 
              position = position_stack(vjust = 0.5), cex = (3)) + 
    scale_y_continuous("Annual Ridership", labels = scales::comma) + theme_linedraw() + scale_fill_brewer(palette = "Paired") +
    labs(title = paste(agency, "Yearly Ridership by Mode"))
  return (p)
}
plot_agency_stackedmodes_yearly(clean_ntd_monthly, septa_name, 2002, 2018)

plot_agency_stackedmodes_yearly_pct <- function(data, agency, year1, year2) {
  d <- data %>%
    filter(Year >= year1 & Year <= year2) %>%
    filter(Modes != "Demand Response") %>%
    find_yearly_modal_ridership_by_specific_Agency(agency)
  p <- ggplot(d, aes(x = Year, y = ridership, fill = Modes)) + geom_col(position = 'fill', stat = 'identity') + 
    scale_y_continuous("Annual Ridership", labels = scales::percent) + theme_linedraw() + scale_fill_brewer(palette = "Paired") +
    labs(title = paste(agency, " Yearly Ridership by Mode"))
  return (p)
}
plot_agency_stackedmodes_yearly_pct(clean_ntd_monthly, septa_name, 2002, 2018)


# plot line graph of yearly average, taking in a list of agencies (peer uzas), begging year, end year
plot_agencies_yearly_ridership <- function(data, list, year1, year2) {
  x <- data %>%
    filter(`5 digit NTD ID` == list) %>% # only map to agency list
    filter(Year >= year1 & Year <= year2) %>% #select rows between years
    find_yearly_ridership_by_Agency() #drill down to necessary values
  
  p <- ggplot(x, aes(x = Year, y = ridership, colour =  `UZA Name`, group = `UZA Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership", labels = scales::comma) + 
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    geom_dl(aes(label = `UZA Name`), method = list(dl.combine("first.qp"), cex = 1)) + 
    scale_color_discrete(guide = 'none') +
    labs(title = paste("Yearly Ridership by Peer Providers"))
  
  return(p)
}
plot_agencies_yearly_ridership(clean_ntd_monthly, peer_codes, 2010, 2017)


plot_uza_yearly_ridership <- function(data, list, year1, year2) {
  x <- data %>%
    filter(Year >= year1 & Year <= year2) %>% #select rows between years
    find_yearly_ridership_by_UZA() %>% #drill down to necessary values
    filter(str_detect(`UZA Name`, list)) %>% # only map top 20 UZAs
    mutate(delta = ridership / first(ridership))
  
  p <- ggplot(x, aes(x = Year, y = ridership, colour =  `UZA Name`, group = `UZA Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership", labels = scales::comma, limits = c(-1, 750000000)) + 
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    #theme(plot.margin = margin(20, 50, 0, 0, 'pt')) +
    geom_dl(aes(label = `UZA Name`), method = list(dl.combine("last.qp"), list(dl.trans(x=x-3.5)), list(dl.trans(y=y+0.4)), cex = 0.75)) + 
    scale_color_discrete(guide = 'none') +
    labs(title = paste("Yearly Ridership by Metro Area (all regional transit providers)"))
  return(p)
}

list_uzas <- "Chicago, IL-IN|Philadelphia, PA-NJ-DE-MD|Washington, DC-VA-MD|Boston, MA-NH-RI|Seattle, WA|Los Angeles-Long Beach-Anaheim, CA"
plot_uza_yearly_ridership(clean_ntd_monthly, list_uzas, 2004, 2018)

plot_uza_yearly_ridership_indexed <- function(data, list, year1, year2) {
  x <- data %>%
    filter(Year >= year1 & Year <= year2) %>% #select rows between years
    find_yearly_ridership_by_UZA() %>% #drill down to necessary values
    filter(ridership > 0) %>%
    filter(str_detect(`UZA Name`, list)) %>% # only map top 20 UZAs
    mutate(delta = ridership / first(ridership))
  
  p <- ggplot(x, aes(x = Year, y = delta, colour =  `UZA Name`, group = `UZA Name`)) + geom_line() + 
    scale_y_continuous("Annual Ridership Change, Indexed") + 
    scale_x_discrete(expand = expand_scale(mult = c(0, 0.4))) +
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    geom_dl(aes(label = `UZA Name`), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) + 
    scale_color_discrete(guide = 'none') +
    labs(title = paste("Yearly Ridership by Metro Area (all regional transit providers)")) + 
    geom_hline(aes(yintercept = 1)) + 
    theme(plot.margin = unit(c(1,3,1,1), "lines")) 
  
  p$layout$clip[p$layout$name == "panel"] <- "off"
  
  return(p)
}

list_uzas <- "Chicago, IL-IN|Philadelphia, PA-NJ-DE-MD|Washington, DC-VA-MD|Boston, MA-NH-RI|Seattle, WA|Los Angeles-Long Beach-Anaheim, CA|New York-Newark, NY-NJ-CT|Portland, OR"
plot_uza_yearly_ridership_indexed(clean_ntd_monthly, list_uzas, 2008, 2018)


#### NATIONAL SPEED ANALYSIS ####

clean_metric_data <- function(metric_data) {
  output <- metric_data %>%
    filter(`Reporter Type` == "Full Reporter") %>%
    select(-c(40:45)) %>%
    filter(`Any data questionable?` == "No") %>%
    mutate("Vehicle Miles per Revenue Hour" = `Vehicle Revenue Miles` / `Vehicle Revenue Hours`) %>%
    mutate(Mode = recode(Mode, "MB" = "Bus", "CR" = "Commuter Rail", "HR" = "Heavy Rail", 
                        "LR" = "Trolley", "SR" = "Trolley", "CB" = "Commuter Bus", 
                        "DR" = "Demand Response", "TB" = "Bus")) %>%
    mutate(Name = as.factor(`Name`)) %>%
    group_by(City, Mode) %>%
    summarise("Vehicle Miles per Revenue Hour" = mean(`Vehicle Miles per Revenue Hour`), 
              "Unlinked Passenger Trips" = sum(`Unlinked Passenger Trips`)) %>%
    mutate(highlight_flag = ifelse(City == 'Philadelphia', T, F))
    
  return(output)
}

clean_ntd_metric_dat <- clean_metric_data(ntd_metric_data)


plot_agency_speed <- function(clean_ntd_metric_dat, city, mode, minimum_UPT, maximum_UPT) {
  if(missing(maximum_UPT)) {
    dat <- clean_ntd_metric_dat %>%
      filter(`City` == city)
      filter(`Mode` == mode) %>% 
      filter(`Unlinked Passenger Trips` > minimum_UPT)
  }
  else {
    dat <- clean_ntd_metric_dat %>%
      filter(`Mode` == mode) %>% 
      filter(`Unlinked Passenger Trips` > minimum_UPT & `Unlinked Passenger Trips` < maximum_UPT)
  }
  
  plot <- ggplot(dat, aes(x = reorder(City, -`Vehicle Miles per Revenue Hour`), 
                          y = `Vehicle Miles per Revenue Hour`)) + 
    geom_col(aes(fill = highlight_flag)) +
    xlab("City") + 
    ylab("Vehicle Miles per Revenue Hour") + 
    labs(title = paste("Philadelphia Among Slowest Peer", mode, "Systems (2017 Data)")) + 
    theme_linedraw() + scale_fill_brewer(palette = "Paired") + 
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = "none")
  
  return(dat)
}

plot_agency_speed(clean_ntd_metric_dat, "Bus", 50000000)

#### PLOT RECOVERY RATIO FOR SEPTA REGIONAL RAIL (LINE CHART) ####

# FUNCTION: clean_ntd_yearly_data
# Paramaters: 
# OpExp_file = ntd operating expenses by year for each agency and mode
# Fares_files = ntd fare revenue by year for each agency and mode
# Year1/Year2 = (inclusive) filter of years to return
# returns a clean dataframe of yearly NTD data - currenlty just operating expensis and fare revenue

clean_ntd_yearly_data <- function(OpExp_file, Fares_file, Ridership_file, Year1 = 1991, Year2 = 2018) {
  #gather a dataframe to get yearly operating expense for each mode of all agencies
  yearly_operating_expense <- OpExp_file %>%
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
    select(c(`NTD ID`,`Agency Name`, Mode, `Mode Status`, `UZA Name`, Service, `1991`:`2018`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "operating_expense", c(`1991`:`2018`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(operating_expense = sum(operating_expense, na.rm = TRUE))
  
  #gather a dataframe to get yearly fare revenue for each mode of all agencies
  yearly_fare_revenue <- Fares_file %>% 
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
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2018`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "fare_revenue", c(`1991`:`2018`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(fare_revenue = sum(fare_revenue, na.rm = TRUE))
    
  #gather a dataframe to get yearly ridership for each mode of all agencies
  yearly_ridership <- Ridership_file %>%
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
    select(c(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Service, `1991`:`2018`)) %>%
    group_by(Mode) %>%
    gather(key = "Year", value = "yearly_ridership", c(`1991`:`2018`), convert = TRUE) %>% 
    mutate(Year = parse_date_time(Year, orders = "y")) %>% 
    separate("Year", c("Year")) %>% 
    group_by(`NTD ID`,`Agency Name`, Mode, `UZA Name`, Year) %>% 
    summarise(yearly_ridership = sum(yearly_ridership, na.rm = TRUE))
  
  
  output <- yearly_operating_expense %>%
    left_join(yearly_fare_revenue) %>%
    left_join(yearly_ridership) %>%
    mutate(recovery_ratio = fare_revenue / operating_expense ) %>% 
    filter(is.na(`Agency Name`) == FALSE) %>% 
    filter(!is.na(operating_expense) & operating_expense > 0) %>% 
    filter(Year >= Year1 & Year <= Year2)

  return(output)
}

##create a dataframe
clean_ntd_yearly <- clean_ntd_yearly_data(ntd_yearly_OpExp_data, ntd_yearly_Fares_data, ntd_yearly_ridership_data, Year1 = 2002, Year2 = 2018)

plot_yearly_recovery_bymode <- function(clean_ntd_yearly, NTD_ID = 30019, mode = "Bus") {
  recovery_dat <- clean_ntd_yearly %>% filter(`NTD ID` == NTD_ID) %>% filter(Mode == mode) %>% 
    select(-c(operating_expense, fare_revenue, yearly_ridership)) %>%
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
rail_recovery_yearly <- plot_yearly_recovery_bymode(clean_ntd_yearly, mode = "Commuter Rail")
rail_recovery_yearly

# PLOT YEARLY RIDERSHIP FOR SEPTA REGIONAL RAIL (LINE CHART)

plot_yearly_ridership_bymode <- function(clean_ntd_yearly, NTD_ID = 30019, mode = "Bus") {
  recovery_dat <- clean_ntd_yearly %>% filter(`NTD ID` == NTD_ID) %>% filter(Mode == mode) %>% 
    select(-c(operating_expense, recovery_ratio, fare_revenue)) %>%
    group_by(Year)
  
  agency_name <- recovery_dat$`Agency Name` %>% unique()
  
  plot <- ggplot(recovery_dat , aes(x = `Year`, y = `yearly_ridership`, group = 1)) +
    geom_line(colour = "azure4") +
    geom_point(size=2, colour = "blue4") +
    ylab("Commuter Rail Yearly Ridership") +
    labs(title = paste(agency_name, mode, "Yearly Ridership", sep = " ")) +
    scale_y_continuous(labels = comma,
                       limits = c(min(recovery_dat$yearly_ridership) - .05 * min(recovery_dat$yearly_ridership), 
                                  max(recovery_dat$yearly_ridership) + .05 * max(recovery_dat$yearly_ridership))) + #adding commas to the numbers on y axis
    theme_phl()
  #+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) #Enable this line if you want to get rid of the first axis lables when combining two graphs 
  
  return(plot)
}

rail_ridership_yearly <- plot_yearly_ridership_bymode(clean_ntd_yearly, mode = "Commuter Rail")
rail_ridership_yearly

#library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(rail_recovery_yearly), ggplotGrob(rail_ridership_yearly), size = "last"))



