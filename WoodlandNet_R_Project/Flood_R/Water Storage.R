# 1. Load required libraries
install.packages(c("terra", "dplyr"))
library(terra)
library(dplyr)

# 2. Load simulated water depth rasters under drought conditions (replace with your own file paths)
depth_no_dam <- rast("Drought Simulated Water Depth (No Dam).tif")
depth_man_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam).tif")
depth_beaver_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Calculate pixel area (in square meters)
cell_area <- res(depth_no_dam)[1] * res(depth_no_dam)[2]  # Typically x resolution * y resolution

# 4. Calculate total water storage (in cubic meters) for each scenario
volume_no_dam <- global(depth_no_dam, sum, na.rm = TRUE)[1,1] * cell_area
volume_man_dam <- global(depth_man_dam, sum, na.rm = TRUE)[1,1] * cell_area
volume_beaver_dam <- global(depth_beaver_dam, sum, na.rm = TRUE)[1,1] * cell_area

# 5. Create dataframe to display results
volume_df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dam"),
  Volume_m3 = c(volume_no_dam, volume_man_dam, volume_beaver_dam)
)

# 6. Print water storage results
print(volume_df)

# 7. Plot bar chart for comparison
library(ggplot2)
ggplot(volume_df, aes(x = Scenario, y = Volume_m3, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Simulated Water Storage under Drought Conditions",
       x = NULL,
       y = "Water Volume (m³)") +
  theme_minimal() +
  theme(legend.position = "none")