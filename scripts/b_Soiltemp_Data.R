#### 1 - LOAD PACKAGES  ##### 
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)

#### 2 - Cairngorms HOBO loggers ####
read_hobo <- function(file) {
  
  # Extract serial number from filename
  serial <- file 
  print(file)
  
  # Read the data file
  data <- read_delim(file, delim = ",",
                     col_names = F, 
                     locale=locale(decimal_mark = ",")) 
  
  
  # Check file has contents. Empty files due to bad data download only have "File is empty" as text. 
  if (ncol(data) > 1) {
    # Create vector of column names
    vars <- c("Index", "Datetime_UTC", "Temperature", "Lux", "Button Down","Button Up",
              "Host Connected","Low Power Warning",	"Safe Shut down", "Low Battery",	"Stopped",	"End of File") 
    
    # Format data for output
    names(data) <- vars
    
    data_with_ID <- data  %>% 
      mutate(SerialID = serial) %>% 
      select(SerialID, everything()) %>% 
      mutate(Datetime_UTC = lubridate::parse_date_time(Datetime_UTC,orders = c("%m/%d/%Y %H:%M:%S")))
    
  } else {
    print("empty file")
    data_with_ID <- NULL
  }
  
  
  return(data_with_ID)
}


# read in data files
hobo <- "data/cairngorms_HOBO_pendants"
hobo_files <- list.files(path = hobo,full.names = T)
cairn_hobo <- map_dfr(hobo_files, read_hobo)

# data cleaning
cairn_hobo = cairn_hobo[-1,]
cairn_hobo$Temperature <- as.numeric(cairn_hobo$Temperature)

clean_cairn <- cairn_hobo %>% 
  filter(Datetime_UTC < lubridate::mdy_hms("12/01/2022 12:00:00")) 

hobo_summary <- ggplot(clean_cairn, aes(x = Datetime_UTC, y = Temperature)) +
  geom_line(aes(color=SerialID)) + 
  theme_classic()

hobo_summary + theme(legend.position = "none")

# histogram of variables
hist_hobo <- ggplot(clean_cairn) +
  aes(x = Temperature, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() 

hist_hobo + theme(legend.position = "none")

# write as csv
write.csv(clean_cairn, file = "data/Cairn_FullHOBO_2022.csv", row.names = FALSE)

#### 3 - Coastal BC Mountain TOMST ####
read_tms4 <- function(file) {
  
  # Extract serial number from filename
  serial <- file 
  print(file)
  
  # Read the data file
  data <- read_delim(file, delim = ";",
                     col_names = F, 
                     locale=locale(decimal_mark = ",")) 
  
  
  # Check file has contents. Empty files due to bad data download only have "File is empty" as text. 
  if (ncol(data) > 1) {
    # Create vector of column names
    vars <- c("Index", "Datetime_UTC", "TimeZone", "T1: Soil sensor", "T2: Surface sensor", "T3: Top sensor", "SoilMoistureCount", "shake",
              "errFlag", "empty")
    
    # Format data for output
    names(data) <- vars
    
    data_with_ID <- data  %>% 
      mutate(SerialID = serial) %>% 
      select(SerialID, everything()) %>% 
      mutate(Datetime_UTC = lubridate::parse_date_time(Datetime_UTC,orders = c("%Y.%m.%d %H:%M")))
    
  } else {
    print("empty file")
    data_with_ID <- NULL
  }
  
  
  return(data_with_ID)
}

# Read-in data files
tomst <- "data/coastal_mtn_TOMST"
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
bc_data <- map_dfr(files, read_tms4)

# change date (GMT to BC time) - 6 hours time difference 
bc_data$Datetime_UTC <- bc_data$Datetime_UTC - hours(7)
str(bc_data)


tomst_bc <-  bc_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2022-06-27 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

tomst_summary <- ggplot(tomst_bc, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic()

tomst_summary + theme(legend.position = "none")

# historgram of variables
hist_tomst <- ggplot(tomst_bc) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free")

hist_tomst + theme(legend.position = "none")

#### 4 - Merge data frames ####
# write as csv
write.csv(tomst_bc, file = "data/BC_FullTOMST_2022.csv", row.names = FALSE)



#### 4 - Kluane TOMST ####
tomst <- "data/new_kluane_TOMST"
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
kp_data <- map_dfr(files, read_tms4)

# change date (GMT to BC time) - 6 hours time difference 
kp_data$Datetime_UTC <- kp_data$Datetime_UTC - hours(6)
str(kp_data)


tomst_kp <-  kp_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2022-04-27 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

tomst_summary <- ggplot(tomst_kp, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic()

tomst_summary + theme(legend.position = "none")

# historgram of variables
hist_tomst <- ggplot(tomst_kp) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free")

hist_tomst + theme(legend.position = "none")

#### 4 - Merge data frames ####
# write as csv
write.csv(tomst_kp, file = "data/KP_FullTOMST_2022.csv", row.names = FALSE)



#### 5 - Toolik TOMST ####
tomst <- "data/toolik_TOMST"
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
tl_data <- map_dfr(files, read_tms4)

# change date (GMT to BC time) - 6 hours time difference 
tl_data$Datetime_UTC <- tl_data$Datetime_UTC - hours(7)
str(tl_data)


tomst_tl <-  tl_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2021-05-27 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

tomst_summary <- ggplot(tomst_tl, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic()

tomst_summary + theme(legend.position = "none")

# historgram of variables
hist_tomst <- ggplot(tomst_tl) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free")

hist_tomst + theme(legend.position = "none")

#### 4 - Merge data frames ####
# write as csv
write.csv(tomst_tl, file = "data/TL_FullTOMST_2022.csv", row.names = FALSE)


#### 6 - Niwot TOMST ####
# read in raw data files
NIW_08 <- read_csv("data/niwot_TOMST/sn_08_tenminute.jm.data.csv")
NIW_11 <- read_csv("data/niwot_TOMST/sn_11_tenminute.jm.data.csv")
NIW_12 <- read_csv("data/niwot_TOMST/sn_12_tenminute.jm.data.csv")
NIW_13 <- read_csv("data/niwot_TOMST/sn_13_tenminute.jm.data.csv")

# merge into one dataset
niwot_all <- rbind(NIW_08, NIW_11, NIW_12, NIW_13)


#### 7 - July means ####
# adapted from Erica Zaja's Common Garden code 2023
# get date column for all dfs

# cairngorms
cg_data <- clean_cairn %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
cg_data$Date <- as.character(cg_data$Date)
range(cg_data$Date) #"2020-11-20" "2022-12-01"

# kluane
kp_data <- tomst_kp %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
kp_data$Date <- as.character(kp_data$Date)
range(kp_data$Date) # "2022-04-27" "2022-08-15"

# coastal bc mountains
bc_data <- tomst_bc %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
bc_data$Date <- as.character(bc_data$Date)
range(bc_data$Date) # "2022-05-01" "2022-09-19"

# toolik lake
tl_data <- tomst_tl %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
tl_data$Date <- as.character(tl_data$Date)
range(tl_data$Date) # "2021-05-27" "2022-07-18"

# niwot ridge
nw_data <- niwot_all %>% 
  mutate(Date = lubridate::date(date))
nw_data$Date <- as.character(nw_data$Date)
nw_data$Date <- lubridate::ymd(nw_data$Date)
range(nw_data$Date) #"2017-08-01" "2022-12-31"

# cairngorms
cairn_july_surface_temp <- cg_data %>%
  group_by(Date, SerialID) %>%
  summarise(mean_temp = mean(Temperature)) %>%
  ungroup() %>%
  subset((SerialID == 'data/cairngorms_HOBO_pendants/20907917-2023-04-26-15_33_56-BST-Data-BST.csv' & Date >= "2021-07-30" & Date <= "2021-07-31") |
      (SerialID != 'data/cairngorms_HOBO_pendants/20907917-2023-04-26-15_33_56-BST-Data-BST.csv' & Date >= "2021-07-30" & Date <= "2022-07-31"))

mean(cairn_july_surface_temp$mean_temp, na.rm = TRUE) # 7.156657


kluane_july_soil_temp <- kp_data %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31") %>% 
  filter(Variable %in% "T1: Soil sensor") %>% 
  summarise(mean_temp = mean(Value)) 

mean(kluane_july_soil_temp$mean_temp) # 3.925377


bc_july_soil_temp <- bc_data %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31") %>% 
  filter(Variable %in% "T1: Soil sensor") %>% 
  summarise(mean_temp = mean(Value)) 

mean(bc_july_soil_temp$mean_temp) # -23.57685

tl_july_soil_temp <- tl_data %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31") %>% 
  filter(Variable %in% "T1: Soil sensor") %>% 
  summarise(mean_temp = mean(Value)) 

mean(tl_july_soil_temp$mean_temp) # 15.75355


niwot_july_soil_temp <- nw_data %>%
  group_by(Date) %>% 
  summarise(mean_temp = mean(soiltemp_5cm_avg)) %>% 
  subset(Date >= "2021-07-01" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")

mean(niwot_july_soil_temp$mean_temp, na.rm = TRUE) #0.04370183

# get daily means for each site
CG_mean_daily_temp <- cg_data  %>%
  #filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2021-04-30")) %>% 
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Temperature)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

KP_mean_daily_temp <- kp_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2021-04-30")) %>% 
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

BC_mean_daily_temp <- bc_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2021-04-30")) %>% 
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

TL_mean_daily_temp <- tl_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2021-04-30")) %>% 
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

NW_mean_daily_temp <- nw_data  %>%
  #filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2021-04-30")) %>% 
  group_by(Date, local_site) %>% 
  summarise(mean_temp = mean(soiltemp_5cm_avg)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()


# get serial numbers from serial_ID
# for hobo we want everything starting 56 to 64
#users/egallois/soilcores/data30/cairngorms_HOBO_pendants/5620907920-2023-04-26-16_33_06-BST-Data-BST.csv	
CG_mean_daily_temp$SerialID <- substr(CG_mean_daily_temp$SerialID, 56, 63)

# for KP we want everything 49 to 56
#users/egallois/soilcores/data30/kluane_TOMST/data_4994217207_2022_08_15_0.csv	
KP_mean_daily_temp$SerialID <- substr(KP_mean_daily_temp$SerialID, 49, 56)

# for BC we want everything from 54 to 61
#users/egallois/soilcores/data30/coastal_mtn_TOMST38/data_94221281_2022_09_19_0.csv	
BC_mean_daily_temp$SerialID <- substr(BC_mean_daily_temp$SerialID, 54, 61)

# for TL we want everything from 49 to 56
#users/egallois/soilcores/data30/toolik_TOMST/data_94213641_2022_07_04_0.csv	
TL_mean_daily_temp$SerialID <- substr(TL_mean_daily_temp$SerialID, 49, 56)

# for NW we want to change date to Date and site to SerialID
names(NW_mean_daily_temp)[names(NW_mean_daily_temp) == "local_site"] <- "SerialID"
names(NW_mean_daily_temp)[names(NW_mean_daily_temp) == "date"] <- "Date"


# save as csvs
write.csv(CG_mean_daily_temp, file = "users/egallois/soilcores/data/CG_mean_daily_soil.csv", row.names = FALSE)
write.csv(BC_mean_daily_temp, file = "users/egallois/soilcores/data/BC_mean_daily_soil.csv", row.names = FALSE)
write.csv(KP_mean_daily_temp, file = "users/egallois/soilcores/data/KP_mean_daily_soil.csv", row.names = FALSE)
write.csv(TL_mean_daily_temp, file = "users/egallois/soilcores/data/TL_mean_daily_soil.csv", row.names = FALSE)
write.csv(NW_mean_daily_temp, file = "users/egallois/soilcores/data/NW_mean_daily_soil.csv", row.names = FALSE)

# add in site names
CG_mean_daily_temp <- CG_mean_daily_temp %>% mutate(site = "cairngorms")
BC_mean_daily_temp <- BC_mean_daily_temp %>% mutate(site = "BC_coastal")
KP_mean_daily_temp <- KP_mean_daily_temp %>% mutate(site = "kluane")
TL_mean_daily_temp <- TL_mean_daily_temp %>% mutate(site = "toolik")
NW_mean_daily_temp <- NW_mean_daily_temp %>% mutate(site = "niwot")

# some of the date columns need to be converted to character
TL_mean_daily_temp$Date <- as.character(TL_mean_daily_temp$Date)
NW_mean_daily_temp$Date <- as.character(NW_mean_daily_temp$Date)

# merge all together
full_sites <- rbind(CG_mean_daily_temp,
                    BC_mean_daily_temp,
                    KP_mean_daily_temp,
                    TL_mean_daily_temp,
                    NW_mean_daily_temp)

# convert date column back to date format
full_sites$Date <- as.Date(full_sites$Date)

# quick plot of all data
ggplot(full_sites) +
  aes(x = Date, y = mean_temp, colour = SerialID) +
  geom_col(fill = "#112446") +
  scale_color_hue(direction = 1) +
  labs(
    y = "Mean Soil Temp (degrees Celsius)",
    title = "Full Soil Temp Data"
  ) +
  theme_classic() +
  facet_wrap(vars(site))

# save final tommie file as a master csv
write.csv(full_sites, file = "users/egallois/soilcores/data/all_sites_mean_daily_soil.csv", row.names = FALSE)



