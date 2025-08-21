# ggplot2 Branding Integration Example

This directory contains examples of how to integrate ggplot2 visualizations with brand themes using the `rbranding` package.

## Files

- `example.R` - Main example script demonstrating the complete workflow
- `README.md` - This file

## Quick Start

1. Install and load the package:
```r
# Install from GitHub
remotes::install_github("EpiForeSITE/branding-package")
library(rbranding)
```

2. Set up branding (run once):
```r
brand_init()
get_brand()
```

3. Apply brand theme and create plots:
```r
library(ggplot2)
brand_set_ggplot()

# Your plots will now use brand colors and fonts
ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  labs(title = "Branded Plot")
```

4. Optionally add logo:
```r
# Add brand logo to plot
ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  brand_add_logo()
```

5. Reset when done:
```r
brand_reset_ggplot()
```

## Requirements

- R packages: `ggplot2`, `yaml`
- Optional: `sysfonts`, `showtext` (for custom fonts)
- Optional: `plotly` (for interactive plots)
- Optional: `png`, `grid` (for logo functionality)

## Troubleshooting

If you encounter font loading issues, disable custom fonts:
```r
brand_set_ggplot(use_fonts = FALSE)
```

For more details, see the package vignette:
```r
vignette("ggplot2-integration", package = "rbranding")
```