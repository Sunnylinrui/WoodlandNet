# 1. Load required libraries
install.packages(c("terra", "ggplot2", "dplyr", "tidyr"))
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Load velocity rasters
vel_n02 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.02).tif")
vel_n05 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.05).tif")
vel_n10 <- rast("Flood Simulated Water Velocity (With Man-Made Dam + Manning0.1).tif")

# 3. Convert to matrix
mat_n02 <- as.matrix(vel_n02, wide = TRUE)
mat_n05 <- as.matrix(vel_n05, wide = TRUE)
mat_n10 <- as.matrix(vel_n10, wide = TRUE)

# 4. Get max velocity per column (i.e. per longitude column)
max_n02 <- apply(mat_n02, 2, max, na.rm = TRUE)
max_n05 <- apply(mat_n05, 2, max, na.rm = TRUE)
max_n10 <- apply(mat_n10, 2, max, na.rm = TRUE)

# 5. Flip all sequences to ensure right-to-left plot
max_n02 <- rev(max_n02)
max_n05 <- rev(max_n05)
max_n10 <- rev(max_n10)

# 6. Build profile dataframe
profile_df <- data.frame(
  Pixel = 1:length(max_n02),
  n_0.02 = max_n02,
  n_0.05 = max_n05,
  n_0.10 = max_n10
)

# 7. Filter out rows that are all zero or all NA
profile_df <- profile_df %>%
  filter(rowSums(is.na(select(., starts_with("n_")))) < 3) %>%
  filter(rowSums(select(., starts_with("n_"))) > 0)

# 8. Create reversed longitude from west to east 
start_lon <- -4.9365
end_lon <- -4.9334
profile_df$Longitude <- seq(from = start_lon, to = end_lon, length.out = nrow(profile_df))

# 9. Flip Longitude and Velocity so plot goes right to left (upstream to downstream)
profile_df <- profile_df[rev(1:nrow(profile_df)), ]
profile_df$Longitude <- rev(profile_df$Longitude)

# 10. Reshape for ggplot
profile_long <- pivot_longer(profile_df, cols = starts_with("n_"),
                             names_to = "Scenario", values_to = "Velocity") %>%
  mutate(Scenario = as.character(Scenario))

# 11. Plot with correct direction and colors
ggplot(profile_long, aes(x = Longitude, y = Velocity, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Max Flow Velocity Profile from Upstream to Downstream",
    x = "Longitude (°W)",
    y = "Velocity (m/s)"
  ) +
  scale_color_manual(
    values = c("n_0.02" = "#E69F00", "n_0.05" = "#009E73", "n_0.10" = "#D55E00")
  ) +
  theme_minimal()

