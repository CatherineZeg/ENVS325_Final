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


r_wet_du <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_wet_du")
r_wetlands <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_wetlands")
r_aquatic <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_aquatic")
r_eia <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_eia")
r_landscap <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_landscap")

plot(r_wet_du)
plot(r_landscap)
plot(r_eia)


min(relevent_sites_data$LabAnalysisStartDate)

# List layers in the geodatabase
st_layers("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb")

# Read a specific vector layer
#PinelandsWideAnuranSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideAnuranSitesWithData")
#PinelandsWideVegetationSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideVegetationSitesWithData")
#PinelandsWideWaterQualitySitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideWaterQualitySitesWithData")
#PinelandsWideFishSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideFishSitesWithData")
#PinelandsWideWatershedsWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideWatershedsWithData")

water_site_info <- read.csv("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/data/SITE_INFO.csv")

#Separate by two datums: NAD27 and NAD83
Water_site_NAD27 <- water_site_info %>%
  filter(HorzDatum == "NAD27")

Water_site_NAD83 <- water_site_info %>%
  filter(HorzDatum == "NAD83")

#Convert to sf
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

transformed_27 <- sf::st_transform(Water_site_NAD27_sf_points, crs = 3424)
transformed_83 <- sf::st_transform(Water_site_NAD83_sf_points, crs = 3424)

combined_sf <- rbind(transformed_27, transformed_83)
combined_vect <- terra::vect(combined_sf)

extracted_vals <- terra::extract(r_landscap, combined_vect)
combined_vect$lanscap_val <- extracted_vals


#repeat but for eia integrity layer
extracted_vals_eia <- terra::extract(r_eia, combined_vect)
combined_vect$eia_val <- extracted_vals_eia

#separating sites within the pinelands and those outside
in_range <- combined_vect[!is.na(extracted_vals[, 2]), ]
out_of_range <- combined_vect[is.na(extracted_vals[, 2]), ]

#overlaying map
terra::plot(r_landscap)
plot(in_range, max.plot = 1, add= TRUE, pch = 16, col = "red")
plot(out_of_range, max.plot = 1, add= TRUE, pch = 16, col = "green")

#Read in water quality data
water_site_quality <- read.csv("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/data/QUALITY.csv")


#filter water quality data for sites that are in raster data range
in_range_sites <- water_site_quality %>%
  filter(SiteNo %in% unique(values(in_range)$SiteNo))

#find rows with no data ----
na_counts <- colSums(is.na(in_range_sites))
na_df <- data.frame(na_counts)

na_list <- na_df %>%
  filter(na_counts == nrow(in_range_sites))

name <- rownames(na_list)

#remove them from data set
relevent_sites_data <- relevent_sites[ , !(names(relevent_sites) %in% rownames(na_list))]

relevent_sites_data <- in_range_sites

# add Date column----
relevent_sites_data$Date <- ymd(relevent_sites_data$ActivityStartDate) + hms(relevent_sites_data$ActivityStartTime)
# set SiteId as character representation of SiteNo
relevent_sites_data$SiteId <- as.character(relevent_sites_data$SiteNo)
relevent_sites_data$ResultMeasureValue <- as.numeric(relevent_sites_data$ResultMeasureValue)

#filter for pH values----
ph_data <- relevent_sites_data %>%
  filter(CharacteristicName == "pH")

#ph_averages <- relevent_sites_data %>%
#  filter(CharacteristicName == "pH") %>%
#  group_by(SiteNo) %>%
#  summarise(Average = mean(ResultMeasureValue, na.rm = TRUE)) 

#ggplot(ph_data,
#       aes(x = Date, y = ResultMeasureValue, col = SiteId)) +
#  geom_line()

#df with all avarages for site 
#join on certain requirement?
#in_range$pH <- ph_averages$Average

#trying to make the plot different
#r_changed <- r_landscap/10000
#srPlot <- stretch(r_changed, minq=0.001,maxq=0.999)
#plotRGB(r_changed, r=3,g=2,b=1)

#finding viable characteristics ----
general <- relevent_sites_data %>%
  group_by(SiteNo, CharacteristicName) %>%
  summarise(Average = mean(ResultMeasureValue, na.rm = TRUE)) 

unique(general$CharacteristicName)

element_df <- general %>%
  group_by(CharacteristicName) %>%
  summarise(sites = length(SiteNo))

full_characteristics <- element_df %>%
  filter(sites == 35)

write.csv(full_characteristics, "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/data/characteristics.csv")
  
list <- unique(full_characteristics$CharacteristicName)

#----
#terra::plot(r_landscap)
#grad_fn <- colorRampPalette(c("green", "white"))
#plot(in_range["pH"], add = TRUE,  pch = 16, col = grad_fn(10))
#plot(in_range["lanscap_val"], add = TRUE,  pch = 16, col = grad_fn(10))

#Playing with data ----
#Total dissolved solids
tds_data <- relevent_sites_data[relevent_sites$CharacteristicName == "pH",]
#Arsenic
arsenic_data <- relevent_sites_data[relevent_sites$CharacteristicName == "Arsenic",]
#Nitrate
nitrate_data <- relevent_sites_data[relevent_sites$CharacteristicName == "Nitrate",]
#Oxygen
oxygen_data <- relevent_sites_data[relevent_sites$CharacteristicName == "Oxygen",]

oxygen_count <- oxygen_data %>%
  count(SiteNo, name = "Oxygen count")

ph_count <- ph_data %>%
  count(SiteNo, name = "pH count")

tds_count <- tds_data %>%
  count(SiteNo, name = "tds count")

counts_df <- oxygen_count %>%
  left_join(ph_count, by='SiteNo') %>%
  left_join(tds_count, by='SiteNo')

#TDS graph
ggplot(tds_data,
       aes(x = Date, y = ResultMeasureValue, col = SiteId)) +
  geom_line() +
  labs(title = "Total Dissolved Solids")

#Nitrate graph
ggplot(nitrate_data,
       aes(x = Date, y = ResultMeasureValue, col = SiteId)) +
  geom_line() +
  labs(title = "Nitrate")

#Oxygen graph
ggplot(oxygen_data,
       aes(x = Date, y = ResultMeasureValue, col = SiteId)) +
  geom_line() +
  labs(title = "Oxygen")


#Adding important averages to spatVector ----
final <- in_range

characteristics <- c("pH","Nitrate", "Total dissolved solids", "Oxygen")

for(char in characteristics) {
  average <- relevent_sites_data %>%
    filter(CharacteristicName == char) %>%
    group_by(SiteNo) %>%
    summarise(!!(paste0(char, "_Average")) := mean(ResultMeasureValue, na.rm = TRUE)) 
  final <- merge(final, average, by = "SiteNo")
}

final_df <- values(final)
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


#Multiple Linear Regression: ----


#Plotting ----
grad_fn <- colorRampPalette(c("#d6d6d6", "#024b30"))
#scale_color_paletteer_d(grad_fn)
terra::plot(r_landscap, col = grad_fn(100), legend = TRUE, axes = FALSE)


grad_fn2 <- colorRampPalette(c("#990000", "purple"))
plot(final["Oxygen_Average"], add = TRUE,  pch = 16,
     breaks=c(0, 25, 50, 75, 100),
     col = grad_fn2(4),
     plg=list(title=""))
title(main = "Average oxygen at monitoring sites")

plot(in_range["pH"], add = TRUE,  pch = 16, col = grad_fn2(10))
legend("topright", legend = c("Group 1", "Group 2", "3", "4"), col = 1:4, pch = 16)

#Scatter plot
ggplot(values(final),
       aes(x = "pH_Average", y = "landscap_val")) +
  points()


#Figure 1: ---- 
#Locate water quality site locations
terra::plot(r_landscap, legend = TRUE, 
            breaks=c(0, 20, 40, 60, 80, 100),
            plg = list(title="Land Integrity"), axes = FALSE)

plot(final["pH_Average"], add = TRUE,  pch = 19, col = "black")




#Figure 2a: ----
#scatter plot of Nitrate Average
ggplot(final_df,
       aes(x = lanscap_val, y = Nitrate_Average)) +
  geom_point()

#Figure 2b: ----

#scatter plot of pH Average
ph_scatter_plot <- ggplot(final_df,
       aes(x = lanscap_val, y = pH_Average)) +
  geom_point(size = 3, shape = 16,
    aes(colour = pH_Average < 7 & pH_Average > 4),
             show.legend = FALSE) +
  scale_y_continuous(limits = c(3, 9)) +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs( title = "Water Quality sites Average pH",
        y = "Average pH",
        x = "Land Integrity Value") +
  #geom_hline(yintercept = 6, linetype = "dashed") + 
  #geom_hline(yintercept = 4, linetype = "dashed") +
  geom_segment(aes(x = 30, y = 7, xend = 10, yend = 7.9),
               arrow = arrow(length = unit(0.5, "cm")),
               color = "black",
               size = 1) +
  annotate("text", x=40, y=7.5, label="Well above the standard pH \n of the Pine Barrens") +
  theme_minimal()
ggMarginal(ph_scatter_plot, type = "histogram", 
           margins = "x",
           breaks = seq(0, 100, by = 20))

#Figure 3: ----
#bar plot of integrity values
ggplot(final_df,
       aes(x = lanscap_val)) +
  geom_histogram(breaks = seq(0, 100, by = 20))
  


#Figure 4:
#group by integrity group, 
ph_data <- relevent_sites_data %>%
  filter(CharacteristicName == "pH")
ph_data_zones <- ph_data %>%
  left_join(final_df %>% select(SiteNo, zone), by = "SiteNo")
ph_data_zones <- ph_data_zones %>%
  filter(SiteNo = 3.937441e+14)
result <- ph_data_zones %>%
  group_by(zone, Date) %>%
  summarise(Average_pH = mean(ResultMeasureValue))
result$zone <- as.character(result$zone)

ggplot(result,
       aes(x = Date, y = Average_pH, col = zone)) +
  geom_smooth() +
  labs(title = "pH")
