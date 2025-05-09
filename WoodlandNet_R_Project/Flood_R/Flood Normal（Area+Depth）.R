# 1. Load libraries
install.packages(c("terra", "ggplot2", "dplyr", "scales"))
library(terra)
library(ggplot2)
library(dplyr)
library(scales)

# 2. Read water depth rasters 
no_dam <- rast("Flood Simulated Water Depth (No Dam).tif")
man_dam <- rast("Flood Simulated Water Depth (With Man-Made Dam).tif")
beaver_dam <- rast("Flood Simulated Water Depth (With Man-Made Dam + Beavers Dam).tif")

# 3. Flood threshold (m)
threshold <- 0.05

# 4. Mask flooded areas
no_dam_mask <- no_dam > threshold
man_dam_mask <- man_dam > threshold
beaver_dam_mask <- beaver_dam > threshold

# 5. Cell area (m²)
cell_area <- res(no_dam)[1] * res(no_dam)[2]

# 6. Calculate flooded area
area_no_dam <- global(no_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area
area_man_dam <- global(man_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area
area_beaver_dam <- global(beaver_dam_mask, sum, na.rm = TRUE)[1,1] * cell_area

# 7. Calculate max water depth
depth_no_dam <- global(no_dam, max, na.rm = TRUE)[1,1]
depth_man_dam <- global(man_dam, max, na.rm = TRUE)[1,1]
depth_beaver_dam <- global(beaver_dam, max, na.rm = TRUE)[1,1]

# 8. Combine into one data frame
df <- data.frame(
  Scenario = c("No Dam", "Man-Made Dam", "Man-Made + Beaver Dams"),
  Flooded_Area_m2 = c(area_no_dam, area_man_dam, area_beaver_dam),
  Max_Depth_m = c(depth_no_dam, depth_man_dam, depth_beaver_dam)
)

# 9. Add % change relative to No Dam
df <- df %>%
  mutate(
    Area_Change_Percent = round((Flooded_Area_m2 - area_no_dam) / area_no_dam * 100, 1),
    Area_Label = paste0(format(round(Flooded_Area_m2), big.mark = ","), " m²"),
    Depth_Label = paste0(round(Max_Depth_m, 2), " m")
  )
# 10. Plot: Flooded Area (bar) + Max Depth (line) + Value Labels
ggplot(df, aes(x = Scenario)) +
  # Bar chart: flooded area
  geom_bar(aes(y = Flooded_Area_m2, fill = Scenario), stat = "identity", width = 0.6) +
  
  # Area value below bar top
  geom_text(aes(y = Flooded_Area_m2 - 1000, label = Area_Label),
            size = 4.2, vjust = 1.2) +
  
  # Percent change below bar top
  geom_text(aes(y = Flooded_Area_m2 - 3000,
                label = paste0(ifelse(Area_Change_Percent > 0, "+", ""),
                               Area_Change_Percent, "%")),
            size = 4.2, vjust = 1.2, fontface = "italic") +
  
  # Line plot: max water depth
  geom_line(aes(y = Max_Depth_m * 10000, group = 1), color = "darkred", size = 1.2) +
  geom_point(aes(y = Max_Depth_m * 10000), color = "darkred", size = 3) +
  
  # Depth label above line
  geom_text(aes(y = Max_Depth_m * 10000 + 1000, label = Depth_Label),
            color = "darkred", size = 4) +
  
  # Manual color
  scale_fill_manual(values = c("Man-Made + Beaver Dams" = "green",
                               "Man-Made Dam" = "gold",
                               "No Dam" = "red")) +
  
  # Axes
  scale_y_continuous(
    name = "Flooded Area (m²)",
    sec.axis = sec_axis(~./10000, name = "Max Water Depth (m)")
  ) +
  
  labs(title = "Flooded Area and Maximum Depth in Normal Condition", x = NULL) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
