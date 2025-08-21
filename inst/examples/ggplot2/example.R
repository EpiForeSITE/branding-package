# Example script demonstrating ggplot2/thematic integration
# Run this after installing the rbranding package

library(rbranding)
library(ggplot2)

# Step 1: Initialize and get brand (run once)
message("Step 1: Setting up brand configuration...")
# brand_init()  # Uncomment for first-time setup
# get_brand()   # Uncomment to get latest brand file

# Step 2: Set ggplot2 theme from brand
message("Step 2: Applying brand theme to ggplot2...")
brand_set_ggplot()

# Step 3: Create example plots
message("Step 3: Creating branded plots...")

# Basic scatter plot
p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point(aes(color = factor(cyl)), size = 3) +
  labs(
    title = "Car Weight vs. Miles per Gallon",
    subtitle = "Branded theme applied automatically",
    x = "Miles per Gallon", 
    y = "Weight (1000 lbs)",
    color = "Cylinders"
  )

print(p1)
ggsave("example_plot1.png", p1, width = 8, height = 6, dpi = 150)

# Bar plot with logo
p2 <- ggplot(mtcars, aes(x = factor(gear), fill = factor(cyl))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Car Distribution by Gear and Cylinders",
    x = "Number of Gears",
    y = "Count", 
    fill = "Cylinders"
  ) +
  brand_add_logo(x = 0.9, y = 0.9, size = 0.08)

print(p2)
ggsave("example_plot2.png", p2, width = 8, height = 6, dpi = 150)

# Step 4: Interactive plot with plotly (optional)
if (requireNamespace("plotly", quietly = TRUE)) {
  message("Step 4: Creating interactive plot with plotly...")
  p1_interactive <- plotly::ggplotly(p1)
  print(p1_interactive)
  
  # Save as HTML
  htmlwidgets::saveWidget(p1_interactive, "example_interactive.html")
}

# Step 5: Reset theme
message("Step 5: Resetting to default theme...")
brand_reset_ggplot()

# Create comparison plot with default theme
p3 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point(aes(color = factor(cyl)), size = 3) +
  labs(
    title = "Same Data with Default Theme",
    x = "Miles per Gallon",
    y = "Weight (1000 lbs)", 
    color = "Cylinders"
  )

print(p3)
ggsave("example_plot3_default.png", p3, width = 8, height = 6, dpi = 150)

message("Example complete! Check the generated PNG files to see the difference.")