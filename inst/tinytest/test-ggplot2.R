library(tinytest)

# Create a temporary brand file for testing
test_brand_content <- "
meta:
  name:
    full: Test Organization
    short: TO

color:
  palette:
    primary: '#1c8478'
    secondary: '#4e2d53'
  foreground: black
  background: white
  primary: primary
  secondary: secondary

typography:
  fonts:
    - family: Arial
      source: system

logo:
  images:
    icon:
      path: test_logo.png
      alt: Test logo
"

# Write test brand file
test_brand_file <- tempfile(fileext = ".yml")
writeLines(test_brand_content, test_brand_file)

# Create a dummy PNG for logo testing
test_logo_file <- "test_logo.png"
if (requireNamespace("png", quietly = TRUE)) {
  # Create a simple test image (1x1 pixel)
  img <- array(c(1, 0, 0, 1), dim = c(1, 1, 4))  # Red pixel with alpha
  png::writePNG(img, test_logo_file)
}

# Test brand_set_ggplot function
expect_silent(brand_set_ggplot(test_brand_file, use_fonts = FALSE))

# Test that theme was actually set
current_theme <- ggplot2::theme_get()
expect_true(inherits(current_theme, "theme"))

# Test brand_reset_ggplot function
expect_true(brand_reset_ggplot())

# Test error cases
expect_error(brand_set_ggplot("nonexistent_file.yml"), pattern = "Brand file not found")

# Test brand_add_logo (requires brand to be set first)
brand_set_ggplot(test_brand_file, use_fonts = FALSE)

if (requireNamespace("png", quietly = TRUE) && file.exists(test_logo_file)) {
  expect_silent({
    logo_layer <- brand_add_logo(x = 0.5, y = 0.5, size = 0.1)
  })
  expect_true(inherits(logo_layer, "Layer"))
} else {
  # Test error when logo file doesn't exist
  expect_error(brand_add_logo(), pattern = "Logo file not found")
}

# Test error when brand not loaded
brand_reset_ggplot()
# Clear the brand environment
if (exists(".brand_env", envir = parent.frame())) {
  rm(list = ls(envir = .brand_env), envir = .brand_env)
}
expect_error(brand_add_logo(), pattern = "Brand configuration not loaded")

# Clean up
file.remove(test_brand_file)
if (file.exists(test_logo_file)) {
  file.remove(test_logo_file)
}