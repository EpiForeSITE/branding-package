# Internal environment to store branding state
.brand_env <- new.env(parent = emptyenv())

#' Set ggplot2 Theme from Brand Configuration
#' 
#' Sets the ggplot2 theme based on colors and typography defined in a _brand.yml file.
#' This function reads the brand configuration and applies it as the default ggplot2 theme.
#' 
#' @param brand_file Path to the _brand.yml file. If NULL, looks for _brand.yml in the current directory.
#' @param use_fonts Logical. Whether to attempt to load and use custom fonts from the brand file. Default is TRUE.
#' 
#' @return Invisibly returns the previous ggplot2 theme (for potential restoration).
#' 
#' @details 
#' This function reads a brand.yml file and extracts color and typography information to 
#' create a custom ggplot2 theme. The function:
#' \itemize{
#'   \item Maps brand colors to ggplot2 theme elements
#'   \item Attempts to load Google Fonts specified in the brand file
#'   \item Stores the previous theme for later restoration
#'   \item Sets the new theme as the default for all subsequent ggplot2 plots
#' }
#' 
#' The brand.yml file should follow the schema defined at:
#' https://github.com/posit-dev/brand-yml/
#' 
#' @examples
#' \dontrun{
#' # Set theme from default _brand.yml file
#' brand_set_ggplot()
#' 
#' # Create a plot - will use the brand theme
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   labs(title = "Example Plot with Brand Theme")
#' 
#' # Reset to original theme
#' brand_reset_ggplot()
#' }
#' 
#' @export
brand_set_ggplot <- function(brand_file = NULL, use_fonts = TRUE) {
  
  # Default to _brand.yml in current directory
  if (is.null(brand_file)) {
    brand_file <- "_brand.yml"
  }
  
  # Check if file exists
  if (!file.exists(brand_file)) {
    stop("Brand file not found: ", brand_file, 
         "\nPlease run brand_init() and get_brand() first, or provide a valid path.")
  }
  
  # Read the YAML file
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for brand_set_ggplot(). Please install it.")
  }
  
  doc <- yaml::yaml.load_file(brand_file)
  
  # Process colors - replace color names with actual hex values from palette
  doc_color <- doc$color
  if (!is.null(doc_color)) {
    # Get color names to modify (all except 'palette')
    to_modify <- setdiff(names(doc_color), "palette")
    
    for (color_name in to_modify) {
      color_value <- doc_color[[color_name]]
      # If the color value exists in the palette, replace it
      if (!is.null(doc_color$palette) && color_value %in% names(doc_color$palette)) {
        doc_color[[color_name]] <- doc_color$palette[[color_value]]
      }
    }
  }
  
  # Process fonts if requested
  use_custom_font <- FALSE
  font_family <- ""
  
  if (use_fonts && !is.null(doc$typography$fonts)) {
    # Extract font families
    families <- sapply(doc$typography$fonts, function(x) x$family)
    
    # Look for common Google Fonts that work well
    google_fonts <- c("Source Sans Pro", "Open Sans", "Roboto", "Lato")
    available_font <- intersect(families, google_fonts)[1]
    
    if (!is.na(available_font)) {
      # Try to load the font
      if (requireNamespace("sysfonts", quietly = TRUE) && 
          requireNamespace("showtext", quietly = TRUE)) {
        
        tryCatch({
          # Convert font name to a valid R font family name
          font_family <- gsub("[^A-Za-z0-9]", "_", tolower(available_font))
          sysfonts::font_add_google(available_font, font_family)
          showtext::showtext_auto()
          use_custom_font <- TRUE
        }, error = function(e) {
          warning("Could not load font '", available_font, "': ", e$message)
        })
      }
    }
  }
  
  # Store information in internal environment
  .brand_env$doc <- doc
  .brand_env$color <- doc_color
  .brand_env$font <- doc$typography
  
  # Create the new theme
  new_theme <- ggplot2::theme_minimal()
  
  # Apply colors if available
  if (!is.null(doc_color)) {
    theme_elements <- list()
    
    # Background colors
    if (!is.null(doc_color$background)) {
      theme_elements$plot.background <- ggplot2::element_rect(fill = doc_color$background, color = NA)
      theme_elements$panel.background <- ggplot2::element_rect(fill = doc_color$background, color = NA)
    }
    
    # Grid colors
    if (!is.null(doc_color$primary)) {
      theme_elements$panel.grid.major <- ggplot2::element_line(color = doc_color$primary, linewidth = 0.2)
    }
    if (!is.null(doc_color$secondary)) {
      theme_elements$panel.grid.minor <- ggplot2::element_line(color = doc_color$secondary, linewidth = 0.1)
    }
    
    # Text colors
    if (!is.null(doc_color$foreground)) {
      theme_elements$text <- ggplot2::element_text(color = doc_color$foreground)
      theme_elements$axis.text <- ggplot2::element_text(color = doc_color$foreground)
      theme_elements$plot.title <- ggplot2::element_text(color = doc_color$foreground)
      theme_elements$axis.title <- ggplot2::element_text(color = doc_color$foreground)
    }
    
    # Apply font if loaded successfully
    if (use_custom_font && font_family != "") {
      if (!is.null(theme_elements$text)) {
        theme_elements$text <- ggplot2::element_text(color = doc_color$foreground, family = font_family)
      } else {
        theme_elements$text <- ggplot2::element_text(family = font_family)
      }
      theme_elements$plot.title <- ggplot2::element_text(family = font_family, 
                                                        color = doc_color$foreground)
      theme_elements$axis.title <- ggplot2::element_text(family = font_family,
                                                        color = doc_color$foreground)
    }
    
    # Apply all theme elements
    new_theme <- new_theme + do.call(ggplot2::theme, theme_elements)
  }
  
  # Save the current theme before setting new one
  .brand_env$prev_theme <- ggplot2::theme_get()
  
  # Set the new theme
  ggplot2::theme_set(new_theme)
  
  message("Brand theme applied successfully!")
  if (use_custom_font && font_family != "") {
    message("Custom font loaded: ", font_family)
  }
  
  invisible(.brand_env$prev_theme)
}

#' Reset ggplot2 Theme to Previous State
#' 
#' Resets the ggplot2 theme to the state it was in before brand_set_ggplot() was called.
#' 
#' @return Invisibly returns TRUE if reset was successful, FALSE if no previous theme was stored.
#' 
#' @examples
#' \dontrun{
#' # Set brand theme
#' brand_set_ggplot()
#' 
#' # Create some plots with brand theme...
#' 
#' # Reset to original theme
#' brand_reset_ggplot()
#' }
#' 
#' @export
brand_reset_ggplot <- function() {
  if (exists("prev_theme", envir = .brand_env)) {
    ggplot2::theme_set(.brand_env$prev_theme)
    message("ggplot2 theme reset to previous state.")
    invisible(TRUE)
  } else {
    warning("No previous theme found. brand_set_ggplot() must be called first.")
    invisible(FALSE)
  }
}

#' Add Brand Logo to ggplot2 Plot
#' 
#' Adds a logo from the brand configuration as an annotation to a ggplot2 plot.
#' 
#' @param x Numeric. Horizontal position of the logo (0-1 scale). Default is 0.9.
#' @param y Numeric. Vertical position of the logo (0-1 scale). Default is 0.1.
#' @param size Numeric. Size of the logo as a fraction of the plot (0-1 scale). Default is 0.05.
#' @param logo_type Character. Which logo to use: "icon" (default) or "full". 
#' 
#' @return A ggplot2 annotation_custom layer that can be added to a plot with `+`.
#' 
#' @details 
#' This function reads the logo path from the stored brand configuration and creates
#' a ggplot2 annotation layer. The brand configuration must be loaded first using
#' brand_set_ggplot().
#' 
#' The function supports PNG images and requires the 'png' and 'grid' packages.
#' 
#' @examples
#' \dontrun{
#' # First set the brand theme to load configuration
#' brand_set_ggplot()
#' 
#' # Create a plot and add logo
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   labs(title = "Example Plot") +
#'   brand_add_logo()
#' 
#' # Customize logo position and size
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   labs(title = "Example Plot") +
#'   brand_add_logo(x = 0.1, y = 0.9, size = 0.08)
#' }
#' 
#' @export
brand_add_logo <- function(x = 0.9, y = 0.1, size = 0.05, logo_type = "icon") {
  
  # Check if brand configuration is loaded
  if (!exists("doc", envir = .brand_env)) {
    stop("Brand configuration not loaded. Please call brand_set_ggplot() first.")
  }
  
  # Get logo information
  doc <- .brand_env$doc
  if (is.null(doc$logo) || is.null(doc$logo$images)) {
    stop("No logo information found in brand configuration.")
  }
  
  # Determine which logo to use
  logo_path <- NULL
  if (logo_type == "icon" && !is.null(doc$logo$images$icon$path)) {
    logo_path <- doc$logo$images$icon$path
  } else if (logo_type == "full" && !is.null(doc$logo$images$full$path)) {
    logo_path <- doc$logo$images$full$path
  } else {
    # Fall back to any available logo
    if (!is.null(doc$logo$images$icon$path)) {
      logo_path <- doc$logo$images$icon$path
    } else if (!is.null(doc$logo$images$full$path)) {
      logo_path <- doc$logo$images$full$path
    }
  }
  
  if (is.null(logo_path)) {
    stop("No suitable logo found in brand configuration.")
  }
  
  # Check if logo file exists
  if (!file.exists(logo_path)) {
    stop("Logo file not found: ", logo_path)
  }
  
  # Check required packages
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required for brand_add_logo(). Please install it.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required for brand_add_logo(). Please install it.")
  }
  
  # Read and create logo annotation
  tryCatch({
    logo_image <- png::readPNG(logo_path)
    
    ggplot2::annotation_custom(
      grob = grid::rasterGrob(
        logo_image,
        x = x,
        y = y,
        width = grid::unit(size, "npc"),
        height = grid::unit(size, "npc")
      ),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )
  }, error = function(e) {
    stop("Failed to load logo image: ", e$message)
  })
}