install.packages("terra")
install.packages("sf")

library(terra)
library(dplyr)
library(ggplot2)
library(sf)


r_wetlands <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_wetlands")
r_aquatic <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_aquatic")
r_eia <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_eia")
r_landscap <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_landscap")
r_wet_du <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_wet_du")


r_wet_du <- rast("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJPC_Integrity_Layers/NJPC_Integrity_Layers/njpc_wet_du")

plot(r_wetlands)
plot(r_aquatic)
plot(r_eia)
plot(r_landscap)
plot(r_wet_du)

# List layers in the geodatabase
st_layers("C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb")


# Read a specific vector layer
PinelandsWideAnuranSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideAnuranSitesWithData")
PinelandsWideVegetationSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideVegetationSitesWithData")
PinelandsWideWaterQualitySitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideWaterQualitySitesWithData")
PinelandsWideFishSitesWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideFishSitesWithData")
PinelandsWideWatershedsWithData <- st_read(dsn = "C:/Users/cathe/OneDrive/Documents/ENVST325/final_project/NJ_Pinelands_Commission_LTEM_data_and_metadata/New Jersey Pinelands Commission LTEM Geospatial Data.gdb", layer = "PinelandsWideWatershedsWithData")



plot(PinelandsWideAnuranSitesWithData)
plot(PinelandsWideVegetationSitesWithData)
plot(PinelandsWideWaterQualitySitesWithData)
plot(PinelandsWideFishSitesWithData)
plot(PinelandsWideWatershedsWithData)







