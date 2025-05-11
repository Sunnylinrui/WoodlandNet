# 1. Install and load required libraries
install.packages(c("terra", "ggplot2", "dplyr", "gridExtra", "scales"))
library(terra)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# 2. Load drought-condition water depth raster data
depth_no_dam <- rast("Drought Simulated Water Depth (No Dam).tif")
depth_man_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam).tif")
depth_beaver_dam <- rast("Drought Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Calculate cell area (m²)
cell_area <- res(depth_no_dam)[1] * res(depth_no_dam)[2]

# 4. Calculate total water volume (m³)
vol_no_dam <- global(depth_no_dam, sum, na.rm = TRUE)[1,1] * cell_area
vol_man_dam <- global(depth_man_dam, sum, na.rm = TRUE)[1,1] * cell_area
vol_beaver_dam <- global(depth_beaver_dam, sum, na.rm = TRUE)[1,1] * cell_area

# 5. Calculate water-covered surface area (m²)
area_no_dam <- global(depth_no_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_man_dam <- global(depth_man_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area
area_beaver_dam <- global(depth_beaver_dam > 0, sum, na.rm = TRUE)[1,1] * cell_area

# 6. Calculate maximum water depth (m)
max_no_dam <- global(depth_no_dam, max, na.rm = TRUE)[1,1]
max_man_dam <- global(depth_man_dam, max, na.rm = TRUE)[1,1]
max_beaver_dam <- global(depth_beaver_dam, max, na.rm = TRUE)[1,1]

# 7. Create summary dataframe
results_df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dam"),
  Volume = c(vol_no_dam, vol_man_dam, vol_beaver_dam),
  Area = c(area_no_dam, area_man_dam, area_beaver_dam),
  MaxDepth = c(max_no_dam, max_man_dam, max_beaver_dam)
)

# 8. Calculate percentage change (based on No Dam scenario)
results_df <- results_df %>%
  mutate(
    Volume_pct = (Volume - Volume[1]) / Volume[1] * 100,
    Area_pct = (Area - Area[1]) / Area[1] * 100
  )

# 9. Set scale factor to align max depth on secondary axis visually
depth_scale_factor <- 700 / 1.2  # aligns 1.2 m max depth with 700 y-axis height

# 10. Left plot: Water Volume + Max Depth Line
p1 <- ggplot(results_df, aes(x = Scenario)) +
  geom_bar(aes(y = Volume, fill = Scenario), stat = "identity", width = 0.6) +
  geom_line(aes(y = MaxDepth * depth_scale_factor, group = 1, color = "Max Water Depth"), size = 1.2) +
  geom_point(aes(y = MaxDepth * depth_scale_factor, color = "Max Water Depth"), size = 3) +
  geom_text(aes(y = Volume + 30, label = paste0(round(Volume), " m³\n+", round(Volume_pct), "%")),
            size = 3.5) +
  geom_text(aes(y = MaxDepth * depth_scale_factor + 35, label = paste0(round(MaxDepth, 2), " m")),
            size = 3, color = "purple") +
  scale_y_continuous(
    name = "Water Volume (m³)",
    sec.axis = sec_axis(~./depth_scale_factor, name = "Max Water Depth (m)", breaks = seq(0, 1.2, 0.3))
  ) +
  scale_color_manual(name = NULL, values = c("Max Water Depth" = "purple")) +
  labs(title = "Simulated Water Storage under Drought Conditions", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y.right = element_text(color = "purple"),
    axis.text.y.right = element_text(color = "purple"),
    legend.position = "right"
  )

# 11. Right plot: Surface Area with Water
p2 <- ggplot(results_df, aes(x = Scenario, y = Area, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(y = Area + 200, label = paste0(round(Area), " m²\n+", round(Area_pct), "%")),
            size = 3.5) +
  labs(title = "Surface Area with Water under Drought Conditions", y = "Surface Area (m²)", x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 12. Display side-by-side plots
grid.arrange(p1, p2, ncol = 2)

