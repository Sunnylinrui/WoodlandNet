# 1. Load required libraries
install.packages(c("terra", "ggplot2"))
library(terra)
library(ggplot2)

# 2. Read the three GeoTIFF raster files (replace with your actual file names/paths)
no_dam <- rast("Flood Simulated Water Depth (No Dam).tif")
man_dam <- rast("Flood Simulated Water Depth (With Man-Made Dam).tif")
beaver_dam <- rast("Flood Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Define the flood threshold (e.g. water depth > 0.05 m is considered flooded)
threshold <- 0.05

# 4. Create binary masks for flooded areas (1 = flooded, 0 = not flooded)
no_dam_mask <- no_dam > threshold
man_dam_mask <- man_dam > threshold
beaver_dam_mask <- beaver_dam > threshold

# 5. Calculate cell area in square meters (based on raster resolution)
cell_area <- res(no_dam)[1] * res(no_dam)[2]

# 6. Calculate flooded area for each scenario (sum of flooded cells × cell area)
area_no_dam <- global(no_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area
area_man_dam <- global(man_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area
area_beaver_dam <- global(beaver_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area

# 7. Create a dataframe for storing the results
area_df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dams"),
  Area_m2 = c(area_no_dam, area_man_dam, area_beaver_dam)
)

print(area_df)  # Output the flooded area table for inspection

# 8. Plot bar chart (Flooded Area in m²)
ggplot(area_df, aes(x = Scenario, y = Area_m2, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Flooded Area by Scenario",
       y = "Flooded Area (m²)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")



