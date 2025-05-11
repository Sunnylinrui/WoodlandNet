# 1. Load required packages
install.packages(c("terra", "dplyr", "ggplot2", "patchwork"))
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

# 2. Load drought simulation rasters (replace with your file paths)
depth_no_dam <- rast("Drought Simulated Water Depth (No Dam).tif")
depth_man_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam).tif")
depth_beaver_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Calculate cell area (in m²)
cell_area <- res(depth_no_dam)[1] * res(depth_no_dam)[2]

# 4. Calculate total volume (in m³)
volume_no_dam <- global(depth_no_dam, sum, na.rm = TRUE)[1,1] * cell_area
volume_man_dam <- global(depth_man_dam, sum, na.rm = TRUE)[1,1] * cell_area
volume_beaver_dam <- global(depth_beaver_dam, sum, na.rm = TRUE)[1,1] * cell_area

# 5. Calculate surface area (only where depth > 0)
area_no_dam <- global(depth_no_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_man_dam <- global(depth_man_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_beaver_dam <- global(depth_beaver_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area

# 6. Combine results into dataframe
results_df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dam"),
  Volume_m3 = c(volume_no_dam, volume_man_dam, volume_beaver_dam),
  Area_m2 = c(area_no_dam, area_man_dam, area_beaver_dam)
)

# 7. Create volume bar chart
p1 <- ggplot(results_df, aes(x = Scenario, y = Volume_m3, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Simulated Water Storage under Drought Conditions",
    x = NULL, y = "Water Volume (m³)", fill = "Scenario"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 8. Create surface area bar chart
p2 <- ggplot(results_df, aes(x = Scenario, y = Area_m2, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Surface Area with Water under Drought Conditions",
    x = NULL, y = "Surface Area (m²)", fill = "Scenario"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 9. Combine both plots side by side
combined_plot <- p1 + p2
print(combined_plot)
