# 1. Load Libraries
install.packages(c("terra", "ggplot2", "dplyr", "tidyr"))
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Read Raster Files (replace with your actual file paths)
vel_n02 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.02).tif")
vel_n05 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.05).tif")
vel_n10 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.1).tif")

# 3. Define velocity threshold
threshold <- 0.3  # Reasonable threshold for high flow velocity (m/s)

# 4. Calculate high-velocity area (> threshold)
cell_area <- res(vel_n02)[1] * res(vel_n02)[2]

area_n02 <- global(vel_n02 > threshold, sum, na.rm = TRUE)[1,1] * cell_area
area_n05 <- global(vel_n05 > threshold, sum, na.rm = TRUE)[1,1] * cell_area
area_n10 <- global(vel_n10 > threshold, sum, na.rm = TRUE)[1,1] * cell_area

area_df <- data.frame(
  Scenario = c("n = 0.02", "n = 0.05", "n = 0.10"),
  Area_m2 = c(area_n02, area_n05, area_n10)
)

# 5. Plot bar chart of high-velocity areas
ggplot(area_df, aes(x = Scenario, y = Area_m2, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "High-Velocity Area (>0.3 m/s) under Different Manning's n",
       y = "Area (m²)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# 6. Extract velocity profile along a transect
row_index <- 150  # Adjust as needed
cols <- 1:ncol(vel_n02)

# Use cellFromRowCol to get cell index along row
cells_n02 <- cellFromRowCol(vel_n02, row_index, cols)
cells_n05 <- cellFromRowCol(vel_n05, row_index, cols)
cells_n10 <- cellFromRowCol(vel_n10, row_index, cols)

# Extract values for each scenario
vel_profile <- data.frame(
  Distance = 1:length(cols),
  n_0.02 = values(vel_n02)[cells_n02],
  n_0.05 = values(vel_n05)[cells_n05],
  n_0.10 = values(vel_n10)[cells_n10]
)

# Remove NA if present
vel_profile <- na.omit(vel_profile)

# 7. Reshape and plot velocity profile line graph
vel_long <- pivot_longer(vel_profile, -Distance, names_to = "Scenario", values_to = "Velocity")

ggplot(vel_long, aes(x = Distance, y = Velocity, color = Scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Flow Velocity Profile Comparison for Different Manning's n",
       x = "Distance along profile (cell index)",
       y = "Velocity (m/s)") +
  theme_minimal()
