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

# 4. Get max velocity per column (i.e. max per longitude pixel)
max_n02 <- apply(mat_n02, 2, max, na.rm = TRUE)
max_n05 <- apply(mat_n05, 2, max, na.rm = TRUE)
max_n10 <- apply(mat_n10, 2, max, na.rm = TRUE)

# 5. Build profile dataframe with consistent naming
profile_df <- data.frame(
  Pixel = 1:ncol(mat_n02),
  n_0.02 = rev(max_n02),
  n_0.05 = rev(max_n05),
  n_0.10 = rev(max_n10)
)

# 6. Trim rows with all zero or NA velocities
profile_df <- profile_df %>%
  filter(rowSums(is.na(select(., starts_with("n_")))) < 3) %>%
  filter(rowSums(select(., starts_with("n_"))) > 0)

# 7. Convert pixel to longitude
start_lon <- -4.9334
end_lon <- -4.9365
profile_df$Longitude <- seq(from = start_lon, to = end_lon, length.out = nrow(profile_df))

# 8. Reshape and plot
profile_long <- pivot_longer(profile_df, cols = starts_with("n_"),
                             names_to = "Scenario", values_to = "Velocity") %>%
  mutate(Scenario = as.character(Scenario))

ggplot(profile_long, aes(x = Longitude, y = Velocity, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Max Flow Velocity",
    x = "Longitude (°W)",
    y = "Velocity (m/s)",
    color = "Manning Coefficient"
  ) +
  scale_color_manual(
    values = c("n_0.02" = "red", "n_0.05" = "green", "n_0.10" = "yellow"),
    labels = c("n_0.02" = "n = 0.02", "n_0.05" = "n = 0.05", "n_0.10" = "n = 0.1")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))  
