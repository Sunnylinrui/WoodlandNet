# 1. Install and load required libraries
install.packages(c("terra", "ggplot2", "dplyr", "gridExtra"))

library(terra)
library(ggplot2)
library(dplyr)
library(gridExtra)

# 2. Load simulated water depth rasters under drought conditions
depth_no_dam <- rast("Drought Simulated Water Depth (No Dam).tif")
depth_man_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam).tif")
depth_beaver_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Calculate the area of a single pixel (in square meters)
cell_area <- res(depth_no_dam)[1] * res(depth_no_dam)[2]

# 4. Calculate total water storage (m³) = sum of depth per pixel × pixel area
vol_no_dam <- global(depth_no_dam, sum, na.rm = TRUE)[1,1] * cell_area
vol_man_dam <- global(depth_man_dam, sum, na.rm = TRUE)[1,1] * cell_area
vol_beaver_dam <- global(depth_beaver_dam, sum, na.rm = TRUE)[1,1] * cell_area

# 5. Calculate number of wet pixels × pixel area = water surface area (m²)
area_no_dam <- global(depth_no_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_man_dam <- global(depth_man_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_beaver_dam <- global(depth_beaver_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area

# 6. Combine results into a data frame
results_df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dam"),
  Volume = c(vol_no_dam, vol_man_dam, vol_beaver_dam),
  Area = c(area_no_dam, area_man_dam, area_beaver_dam)
)

# 7. Add percentage change compared to the "No Dam" baseline
results_df <- results_df %>%
  mutate(
    Volume_pct = (Volume - Volume[1]) / Volume[1] * 100,
    Area_pct = (Area - Area[1]) / Area[1] * 100
  )

# 8. Define y-axis limits to avoid label clipping
volume_ylim <- c(0, max(results_df$Volume) * 1.2)
area_ylim <- c(0, max(results_df$Area) * 1.2)

# 9. Plot water storage (volume)
p1 <- ggplot(results_df, aes(x = Scenario, y = Volume, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Volume), " m³\n+", round(Volume_pct, 1), "%")),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Simulated Water Storage under Drought Conditions",
    y = "Water Volume (m³)", x = NULL
  ) +
  ylim(volume_ylim) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 10. Plot surface water area
p2 <- ggplot(results_df, aes(x = Scenario, y = Area, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Area), " m²\n+", round(Area_pct, 1), "%")),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Surface Area with Water under Drought Conditions",
    y = "Surface Area (m²)", x = NULL
  ) +
  ylim(area_ylim) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 11. Display the two plots side-by-side
grid.arrange(p1, p2, ncol = 2)