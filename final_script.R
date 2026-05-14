#install.packages("terra")
#install.packages("sf")
#install.packages("ggExtra")

#Loading in libraries and data ----
library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(ggExtra)

# Read in integrity layers raster data
r_landscap <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/ENVS325_Final/NJPC_Integrity_Layers-njpc_landscap")

# All water site info from all monitored sites in southern NJ  
water_site_info <- read.csv("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/ENVS325_Final/SITE_INFO.csv")

# Separate by two datums: NAD27 and NAD83
Water_site_NAD27 <- water_site_info %>%
  filter(HorzDatum == "NAD27")

Water_site_NAD83 <- water_site_info %>%
  filter(HorzDatum == "NAD83")

# Convert to sf
Water_site_NAD27_sf_points <- st_as_sf(
  Water_site_NAD27,
  coords = c("DecLongVa", "DecLatVa"),
  crs = 4267 #NAD27
) 
Water_site_NAD83_sf_points <- st_as_sf(
  Water_site_NAD83,
  coords = c("DecLongVa", "DecLatVa"),
  crs = 4269  # NAD83
)

# Transform each site group to use the same Datum as the raster landscape data
transformed_27 <- sf::st_transform(Water_site_NAD27_sf_points, crs = 3424)
transformed_83 <- sf::st_transform(Water_site_NAD83_sf_points, crs = 3424)

# Combine the two groups
combined_sf <- rbind(transformed_27, transformed_83)
# Convert to spatial vector
combined_vect <- terra::vect(combined_sf)
# Extract the landscape integrity value at each location site
extracted_vals <- terra::extract(r_landscap, combined_vect)
# Add this info to the site vector
combined_vect$lanscap_val <- extracted_vals

# Separate sites within the pinelands and those outside
in_range <- combined_vect[!is.na(extracted_vals[, 2]), ]
out_of_range <- combined_vect[is.na(extracted_vals[, 2]), ]

# Overlaying map
terra::plot(r_landscap)
# Plotting in range vs out of range sites
plot(in_range, max.plot = 1, add= TRUE, pch = 16, col = "green")
plot(out_of_range, max.plot = 1, add= TRUE, pch = 16, col = "red")

# Read in water quality data
water_site_quality <- read.csv("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/ENVS325_Final/QUALITY.csv")

# Filter water quality data for sites that are in raster data range ----
relevent_sites_data <- water_site_quality %>%
  filter(SiteNo %in% unique(values(in_range)$SiteNo))

# Add Date column
relevent_sites_data$Date <- ymd(relevent_sites_data$ActivityStartDate) + hms(relevent_sites_data$ActivityStartTime)
# Set SiteId as character representation of SiteNo
relevent_sites_data$SiteId <- as.character(relevent_sites_data$SiteNo)
# Update Measurements data to numeric
relevent_sites_data$ResultMeasureValue <- as.numeric(relevent_sites_data$ResultMeasureValue)

#finding viable characteristics ----
general <- relevent_sites_data %>%
  group_by(SiteNo, CharacteristicName) %>%
  summarise(Average = mean(ResultMeasureValue, na.rm = TRUE)) 

element_df <- general %>%
  group_by(CharacteristicName) %>%
  summarise(sites = length(SiteNo))

full_characteristics <- element_df %>%
  filter(sites == 35)

# Creates list of Characteristics that could potentially be analyzed 
list <- unique(full_characteristics$CharacteristicName)

# Adding important averages to spatVector ----
# Create Spatvec copy of in_range to add water quality data to
final <- in_range

# Create list of relevant Characteristics
characteristics <- c("pH","Nitrate", "Total dissolved solids", "Oxygen", "Calcium")

# Iterate through each characteristic to add site averages
for(char in characteristics) {
  average <- relevent_sites_data %>%
    filter(CharacteristicName == char) %>%
    group_by(SiteNo) %>%
    summarise(!!(paste0(char, "_Average")) := mean(ResultMeasureValue, na.rm = TRUE)) 
  final <- merge(final, average, by = "SiteNo")
}

# Create df of final sites
final_df <- values(final)
# Add zone identifier column
final_df <- final_df %>%
  mutate(
    zone = case_when(
      lanscap_val >= 80 ~ "80 - 100",
      lanscap_val >= 60 ~ "60 - 80",
      lanscap_val >= 40 ~ "40 - 60",
      lanscap_val >= 20 ~ "20 - 40",
      lanscap_val >= 0 ~ "0 - 20"  
    )
  )

#Figure 1: ---- 
#Locate water quality site locations
terra::plot(r_landscap, legend = TRUE, 
            breaks=c(0, 20, 40, 60, 80, 100),
            plg = list(title="Land Integrity"), axes = FALSE)

plot(final["pH_Average"], add = TRUE,  pch = 19, col = "black")


#Figure 2: ----
#scatter plot of pH Average
ph_scatter_plot <- ggplot(final_df,
       aes(x = lanscap_val, y = pH_Average)) +
  geom_point(size = 3, shape = 16,
    aes(colour = pH_Average < 7 & pH_Average > 4),
             show.legend = FALSE) +
  scale_y_continuous(limits = c(3, 9)) +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs( #title = "Water Quality sites Average pH",
        y = "Average pH",
        x = "Land Integrity Value") +
  #geom_hline(yintercept = 6, linetype = "dashed") + 
  #geom_hline(yintercept = 4, linetype = "dashed") +
  geom_segment(aes(x = 25, y = 7, xend = 10, yend = 7.9),
               arrow = arrow(length = unit(0.5, "cm")),
               color = "black",
               size = 1) +
  annotate("text", x=40, y=7.5, label="Well above the average pH \n of the Pinelands") +
  theme_minimal()
ggMarginal(ph_scatter_plot, type = "histogram", 
           fill = "white",
           margins = "x",
           breaks = seq(0, 100, by = 20))


#Figure 3: ----
# Plot pH trends by integrity group

# Sorting for pH values
ph_data <- relevent_sites_data %>%
  filter(CharacteristicName == "pH")

# Adding Zone labels 
ph_data_zones <- ph_data %>%
  left_join(final_df %>% select(SiteNo, zone), by = "SiteNo")

#Removing the outlier from the group
ph_data_zones <- ph_data_zones %>%
  filter(SiteNo != 395638074432501)

# Finding mean of each zone at each date 
result <- ph_data_zones %>%
  group_by(zone, Date) %>%
  summarise(Average_pH = mean(ResultMeasureValue))

# Plot using ggplot
ggplot(result,
       aes(x = Date, y = Average_pH, col = zone)) +
  geom_smooth(method = "auto", se = FALSE) +
  labs(x = "Date", y = "Average pH", color = "Integrity Zone") +
  theme_bw()

#Figure 4: ----
# Scatter plot of Calcium Average
ggplot(final_df,
       aes(x = lanscap_val, y = Calcium_Average)) +
  geom_point(size = 3, shape = 16) +
  #scale_y_continuous(limits = c(3, 9)) +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs( #title = "Water Quality sites Average Calcium",
    y = "Average Calcium",
    x = "Land Integrity Value") +
  theme_minimal()
