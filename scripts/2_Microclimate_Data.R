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

# historgram of variables
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

# change date (GMT to BC time) - 7 hours time difference 
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
write.csv(tomst_bc, file = "users/egallois/soilcores/data/BC_FullTOMST_2022.csv", row.names = FALSE)

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
tomst <- "data/new_toolik_TOMST"
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
tl_data <- purrr::map_dfr(files, read_tms4)

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

# save as NW_FullTOMST_2022.csv
write.csv(niwot_all, file = "data/NW_FullTOMST_2022.csv", row.names = FALSE)

#### 7 - July means ####
# adapted from Erica Zaja's Common Garden code 2023
# get date column for all dfs

# download full data if not already imported
tomst_kp <- read_csv("data/KP_FullTOMST_2022.csv")
tomst_bc <- read_csv("data/BC_FullTOMST_2022.csv")
tomst_tl <- read_csv("data/TL_FullTOMST_2022.csv")
clean_cairn <- read_csv("data/Cairn_FullHOBO_2022.csv")
niwot_all <- read_csv("data/NW_FullTOMST_2022.csv")

# in tomst_kp convert 2022 to 2021 and 2023 to 2022 - somehow they got converted
tomst_kp$Datetime_UTC <- ifelse(tomst_kp$Datetime_UTC > lubridate::ymd_hm("2022-01-01 00:00"), tomst_kp$Datetime_UTC - years(1), tomst_kp$Datetime_UTC)

# cairngorms
cg_data <- clean_cairn %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
cg_data$Date <- as.character(cg_data$Date)
range(cg_data$Date) #"2020-11-20" "2022-12-01"

# kluane
kp_data <- tomst_kp %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
kp_data$Date <- as.character(kp_data$Date)
range(kp_data$Date) # '2021-04-27" "2022-08-22"

# coastal bc mountains
bc_data <- tomst_bc %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
bc_data$Date <- as.character(bc_data$Date)
range(bc_data$Date) # "2022-06-27" "2022-09-19"

# toolik lake
tl_data <- tomst_tl %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
tl_data$Date <- as.character(tl_data$Date)
range(tl_data$Date) # 2021-05-27" "2023-08-28"

# niwot ridge
nw_data <- niwot_all %>% 
  mutate(Date = lubridate::date(date))
nw_data$Date <- as.character(nw_data$Date)
nw_data$Date <- lubridate::ymd(nw_data$Date)
range(nw_data$Date) #"2017-08-01" "2022-12-31"


# for which date range do all of the above overlap?
# get unique dates from each df
cg_dates <- unique(cg_data$Date)
kp_dates <- unique(kp_data$Date)
bc_dates <- unique(bc_data$Date)
tl_dates <- unique(tl_data$Date)
nw_dates <- unique(nw_data$Date)

# common dates 
common_dates <- Reduce(intersect, list(cg_dates, nw_dates,kp_dates, bc_dates, tl_dates))

# are the common dates continuous
continuous_range <- range(common_dates)
 
cg_data_common <- cg_data %>% 
  filter(Date %in% common_dates) %>% 
  mutate(site = "cg")
bc_data_common <- bc_data %>% 
  filter(Date %in% common_dates) %>%
  mutate(site = "bc")
tl_data_common <- tl_data %>% 
  filter(Date %in% common_dates)%>%
  mutate(site = "tl")
kp_data_common <- kp_data %>% 
  filter(Date %in% common_dates) %>%
  mutate(site = "kp")
nw_data_common <- nw_data %>% 
  filter(Date %in% common_dates) %>%
  mutate(site = "nw")

all_sites_common <- rbind(bc_data_common, tl_data_common, kp_data_common)

# plot all time series for all sites using common dates only
ggplot(all_sites_common) +
  aes(x = Date, y = Value, colour = site) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic()+ theme(legend.position = "none")


cairn_summer_surface_temp <- cg_data %>%
  group_by(Date, SerialID) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE)) %>%
  ungroup() %>%
  subset(Date >= "2022-06-01" & Date <= "2022-08-31") 
mean(cairn_summer_surface_temp$mean_temp) #  13.79605


kluane_summer_surface_temp <- kp_data %>%
  subset(Date >= "2022-06-01" & Date <= "2022-08-31") %>% 
  filter(Variable %in% "T2: Surface sensor") %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE)) 
  
mean(kluane_summer_surface_temp$mean_temp) # 9.069342

bc_summer_surface_temp <- bc_data %>%
  subset(Date >= "2022-07-20" & Date <= "2022-08-31") %>% 
  filter(Variable %in% "T2: Surface sensor") %>% 
  summarise(mean_temp = mean(Value)) 

mean(bc_summer_surface_temp$mean_temp) # 13.84498

tl_summer_surface_temp <- tl_data %>%
  subset(Date >= "2022-06-01" & Date <= "2022-08-31") %>% 
  filter(Variable %in% "T2: Surface sensor") %>% 
  summarise(mean_temp = mean(Value)) 

mean(tl_summer_surface_temp$mean_temp) # 14.12455


niwot_summer_surface_temp <- nw_data %>%
  group_by(Date) %>% 
  summarise(mean_temp = mean(airtemp_avg)) %>% 
  subset(Date >= "2022-06-01" & Date <= "2022-08-31")

mean(niwot_summer_surface_temp$mean_temp, na.rm = TRUE) #  10.42344

# get daily means for each site
CG_mean_daily_temp <- cg_data  %>%
  #filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-06-01")) %>% 
  filter(Date < lubridate::ymd("2022-08-31")) %>% 
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Temperature, na.rm = TRUE)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

KP_mean_daily_temp <- kp_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-06-01")) %>% 
  filter(Date < lubridate::ymd("2022-08-31")) %>%
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

BC_mean_daily_temp <- bc_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-07-20")) %>% 
  filter(Date < lubridate::ymd("2022-08-31")) %>%
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

TL_mean_daily_temp <- tl_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-06-01")) %>% 
  filter(Date < lubridate::ymd("2022-08-31")) %>%
  group_by(Date, SerialID) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

NW_mean_daily_temp <- nw_data  %>%
  #filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-06-01")) %>% 
  filter(Date < lubridate::ymd("2022-08-31")) %>%
  group_by(Date, local_site) %>% 
  summarise(mean_temp = mean(airtemp_avg, na.rm = TRUE)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()


# get serial numbers from serial_ID
# for hobo we want everything after pendants
CG_mean_daily_temp$SerialID <- gsub(".*pendants/(.*)-20.*", "\\1", CG_mean_daily_temp$SerialID)
KP_mean_daily_temp$SerialID <- gsub(".*TOMST/data_(.*)_20.*", "\\1", KP_mean_daily_temp$SerialID)
BC_mean_daily_temp$SerialID <- gsub(".*TOMST/data_(.*)_20.*", "\\1", BC_mean_daily_temp$SerialID)
TL_mean_daily_temp$SerialID <- gsub(".*TOMST/data_(.*)_20.*", "\\1", TL_mean_daily_temp$SerialID)

# for NW we want to change date to Date and site to SerialID
names(NW_mean_daily_temp)[names(NW_mean_daily_temp) == "local_site"] <- "SerialID"
names(NW_mean_daily_temp)[names(NW_mean_daily_temp) == "date"] <- "Date"


# save as csvs
write.csv(CG_mean_daily_temp, file = "data/CG_mean_daily_temp.csv", row.names = FALSE)
write.csv(BC_mean_daily_temp, file = "data/BC_mean_daily_temp.csv", row.names = FALSE)
write.csv(KP_mean_daily_temp, file = "data/KP_mean_daily_temp.csv", row.names = FALSE)
write.csv(TL_mean_daily_temp, file = "data/TL_mean_daily_temp.csv", row.names = FALSE)
write.csv(NW_mean_daily_temp, file = "data/NW_mean_daily_temp.csv", row.names = FALSE)

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
  geom_line(fill = "#112446") +
  scale_color_hue(direction = 1) +
  labs(
    y = "Mean Surf Temp (degrees Celsius)",
    title = "Full Surface Temp Data"
  ) +
  theme_classic() +
  facet_wrap(vars(site)) + 
  theme(legend.position = "none")


# save final tommie file as a master csv
write.csv(full_sites, file = "data/all_sites_mean_daily_temp.csv", row.names = FALSE)



