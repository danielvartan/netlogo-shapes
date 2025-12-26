# Generated with Posit Databot and Claude Opus 4.5

# --------------------------------------------------

# Convert Inkscape SVG to NetLogo Turtle Shape XML
#
# This script reads an Inkscape SVG file and converts it to NetLogo
# turtle shape XML format.

library(xml2)

# --- NetLogo Color Mapping (reverse lookup) ---
netlogo_base_colors <- c(
  "black (0)" = "#000000",
  "white (10)" = "#FFFFFF",
  "gray (5)" = "#8D8D8D",
  "red (15)" = "#D73229",
  "orange (25)" = "#F16A15",
  "brown (35)" = "#9D6E48",
  "yellow (45)" = "#ECEC29",
  "green (55)" = "#59B03C",
  "lime (65)" = "#2CD13B",
  "turquoise (75)" = "#1D9F78",
  "cyan (85)" = "#54C4C4",
  "sky (95)" = "#2D8DBE",
  "blue (105)" = "#345DA9",
  "violet (115)" = "#7C50A4",
  "magenta (125)" = "#A71B6A",
  "pink (135)" = "#D9637F"
)

netlogo_xml_colors <- c(
  "black (0)" = "255",
  "white (10)" = "-1",
  "gray (5)" = "-1920102913",
  "red (15)" = "-684578305",
  "orange (25)" = "-244705793",
  "brown (35)" = "-1653716737",
  "yellow (45)" = "-303222273",
  "green (55)" = "1504722175",
  "lime (65)" = "751909887",
  "turquoise (75)" = "496990463",
  "cyan (85)" = "1422181631",
  "sky (95)" = "764264191",
  "blue (105)" = "878553599",
  "violet (115)" = "2085659903",
  "magenta (125)" = "-1491375361",
  "pink (135)" = "-528509185"
)

# Create reverse lookup: hex -> XML color code
hex_to_xml <- setNames(netlogo_xml_colors, toupper(netlogo_base_colors))

#' Convert hex color to NetLogo XML color code
#' @param hex_color The hex color string (e.g., "#8D8D8D")
#' @return NetLogo XML color code as string
hex_to_netlogo_color <- function(hex_color) {
  hex_upper <- toupper(hex_color)
  if (hex_upper %in% names(hex_to_xml)) {
    return(hex_to_xml[[hex_upper]])
  }
  # Default to gray for unknown colors
  return("-1920102913")
}

#' Parse SVG path d attribute to absolute coordinates
#' @param d The path d attribute string
#' @return Matrix of x,y coordinates
parse_svg_path <- function(d) {
  # Remove extra spaces and normalize
  d <- gsub("\\s+", " ", trimws(d))
  d <- gsub("([a-zA-Z])", " \\1 ", d)
  d <- gsub(",", " ", d)
  d <- gsub("\\s+", " ", trimws(d))

  tokens <- strsplit(d, " ")[[1]]
  tokens <- tokens[tokens != ""]

  coords <- matrix(ncol = 2, nrow = 0)
  current_x <- 0
  current_y <- 0
  start_x <- 0
  start_y <- 0
  i <- 1

  while (i <= length(tokens)) {
    cmd <- tokens[i]

    if (cmd == "m" || cmd == "M") {
      # Move to (relative or absolute)
      i <- i + 1
      x <- as.numeric(tokens[i])
      i <- i + 1
      y <- as.numeric(tokens[i])

      if (cmd == "m") {
        current_x <- current_x + x
        current_y <- current_y + y
      } else {
        current_x <- x
        current_y <- y
      }
      start_x <- current_x
      start_y <- current_y
      coords <- rbind(coords, c(current_x, current_y))

      # Subsequent coordinate pairs are implicit lineto
      i <- i + 1
      while (i <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
        x <- as.numeric(tokens[i])
        i <- i + 1
        y <- as.numeric(tokens[i])

        if (cmd == "m") {
          current_x <- current_x + x
          current_y <- current_y + y
        } else {
          current_x <- x
          current_y <- y
        }
        coords <- rbind(coords, c(current_x, current_y))
        i <- i + 1
      }
    } else if (cmd == "l" || cmd == "L") {
      # Line to
      i <- i + 1
      while (i <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
        x <- as.numeric(tokens[i])
        i <- i + 1
        y <- as.numeric(tokens[i])

        if (cmd == "l") {
          current_x <- current_x + x
          current_y <- current_y + y
        } else {
          current_x <- x
          current_y <- y
        }
        coords <- rbind(coords, c(current_x, current_y))
        i <- i + 1
      }
    } else if (cmd == "h" || cmd == "H") {
      # Horizontal line
      i <- i + 1
      while (i <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
        x <- as.numeric(tokens[i])
        if (cmd == "h") {
          current_x <- current_x + x
        } else {
          current_x <- x
        }
        coords <- rbind(coords, c(current_x, current_y))
        i <- i + 1
      }
    } else if (cmd == "v" || cmd == "V") {
      # Vertical line
      i <- i + 1
      while (i <= length(tokens) && !grepl("^[a-zA-Z]$", tokens[i])) {
        y <- as.numeric(tokens[i])
        if (cmd == "v") {
          current_y <- current_y + y
        } else {
          current_y <- y
        }
        coords <- rbind(coords, c(current_x, current_y))
        i <- i + 1
      }
    } else if (cmd == "z" || cmd == "Z") {
      # Close path - don't add point, just move back to start
      current_x <- start_x
      current_y <- start_y
      i <- i + 1
    } else {
      # Unknown command or number (implicit lineto after move)
      if (grepl("^-?[0-9]", cmd)) {
        x <- as.numeric(cmd)
        i <- i + 1
        y <- as.numeric(tokens[i])
        current_x <- current_x + x
        current_y <- current_y + y
        coords <- rbind(coords, c(current_x, current_y))
      }
      i <- i + 1
    }
  }

  coords
}

#' Extract color from SVG style attribute
#' @param style The style attribute string
#' @param attr_name The attribute to extract ("fill" or "stroke")
#' @return Hex color string or NULL
extract_color_from_style <- function(style, attr_name = "fill") {
  if (is.na(style)) {
    return(NULL)
  }

  # Parse style attribute
  pattern <- paste0(attr_name, "\\s*:\\s*([^;]+)")
  match <- regmatches(style, regexpr(pattern, style, ignore.case = TRUE))

  if (length(match) == 0 || match == "") {
    return(NULL)
  }

  color <- gsub(paste0(attr_name, "\\s*:\\s*"), "", match, ignore.case = TRUE)
  color <- trimws(color)

  if (color == "none") {
    return(NULL)
  }

  toupper(color)
}

#' Check if element is filled based on style
#' @param style The style attribute string
#' @return Logical indicating if element is filled
is_filled <- function(style) {
  if (is.na(style)) {
    return(TRUE)
  }

  fill <- extract_color_from_style(style, "fill")
  !is.null(fill) && fill != "NONE"
}

#' Convert SVG element to NetLogo XML
#' @param elem An xml_node representing an SVG element
#' @return String containing NetLogo XML for the element
svg_element_to_netlogo <- function(elem) {
  elem_name <- xml_name(elem)
  style <- xml_attr(elem, "style")

  if (elem_name == "path") {
    # Convert path to polygon
    d <- xml_attr(elem, "d")
    coords <- parse_svg_path(d)

    if (nrow(coords) < 3) {
      return("")
    }

    filled <- is_filled(style)
    if (filled) {
      color <- extract_color_from_style(style, "fill")
    } else {
      color <- extract_color_from_style(style, "stroke")
    }
    if (is.null(color)) {
      color <- "#8D8D8D"
    }

    xml_color <- hex_to_netlogo_color(color)
    filled_str <- if (filled) "true" else "false"

    # Build polygon XML
    lines <- sprintf(
      '      <polygon color="%s" filled="%s" marked="true">',
      xml_color,
      filled_str
    )
    for (i in 1:nrow(coords)) {
      lines <- c(
        lines,
        sprintf(
          '        <point x="%d" y="%d"></point>',
          round(coords[i, 1]),
          round(coords[i, 2])
        )
      )
    }
    lines <- c(lines, '      </polygon>')

    return(paste(lines, collapse = "\n"))
  } else if (elem_name == "circle") {
    cx <- as.numeric(xml_attr(elem, "cx"))
    cy <- as.numeric(xml_attr(elem, "cy"))
    r <- as.numeric(xml_attr(elem, "r"))

    # Convert center+radius to x,y,diameter
    x <- round(cx - r)
    y <- round(cy - r)
    diameter <- round(r * 2)

    filled <- is_filled(style)
    if (filled) {
      color <- extract_color_from_style(style, "fill")
    } else {
      color <- extract_color_from_style(style, "stroke")
    }
    if (is.null(color)) {
      color <- "#8D8D8D"
    }

    xml_color <- hex_to_netlogo_color(color)
    filled_str <- if (filled) "true" else "false"

    return(sprintf(
      '      <circle x="%d" y="%d" marked="false" color="%s" diameter="%d" filled="%s"></circle>',
      x,
      y,
      xml_color,
      diameter,
      filled_str
    ))
  } else if (elem_name == "line") {
    x1 <- xml_attr(elem, "x1")
    y1 <- xml_attr(elem, "y1")
    x2 <- xml_attr(elem, "x2")
    y2 <- xml_attr(elem, "y2")

    color <- extract_color_from_style(style, "stroke")
    if (is.null(color)) {
      color <- "#8D8D8D"
    }
    xml_color <- hex_to_netlogo_color(color)

    return(sprintf(
      '      <line endX="%s" startY="%s" marked="false" color="%s" endY="%s" startX="%s"></line>',
      x2,
      y1,
      xml_color,
      y2,
      x1
    ))
  } else if (elem_name == "rect") {
    x <- as.numeric(xml_attr(elem, "x"))
    y <- as.numeric(xml_attr(elem, "y"))
    width <- as.numeric(xml_attr(elem, "width"))
    height <- as.numeric(xml_attr(elem, "height"))

    startX <- round(x)
    startY <- round(y)
    endX <- round(x + width)
    endY <- round(y + height)

    filled <- is_filled(style)
    if (filled) {
      color <- extract_color_from_style(style, "fill")
    } else {
      color <- extract_color_from_style(style, "stroke")
    }
    if (is.null(color)) {
      color <- "#8D8D8D"
    }

    xml_color <- hex_to_netlogo_color(color)
    filled_str <- if (filled) "true" else "false"

    return(sprintf(
      '      <rectangle endX="%d" startY="%d" marked="true" color="%s" endY="%d" startX="%d" filled="%s"></rectangle>',
      endX,
      startY,
      xml_color,
      endY,
      startX,
      filled_str
    ))
  }

  ""
}

#' Convert Inkscape SVG file to NetLogo shape XML
#' @param svg_file Path to the SVG file
#' @param shape_name Name for the shape (defaults to filename without extension)
#' @param rotatable Whether the shape is rotatable (default TRUE)
#' @return String containing NetLogo shape XML
convert_svg_to_netlogo <- function(
  svg_file,
  shape_name = NULL,
  rotatable = TRUE
) {
  # Read SVG file
  svg <- read_xml(svg_file)

  # Get shape name from filename if not provided
  if (is.null(shape_name)) {
    shape_name <- gsub("_", " ", tools::file_path_sans_ext(basename(svg_file)))
  }

  rotatable_str <- if (rotatable) "true" else "false"

  # Start shape XML
  xml_lines <- sprintf(
    '    <shape name="%s" rotatable="%s" editableColorIndex="0">',
    shape_name,
    rotatable_str
  )

  # Find all SVG elements using local-name() to handle namespace
  elements <- xml_find_all(
    svg,
    "//*[local-name()='path' or local-name()='circle' or local-name()='line' or local-name()='rect']"
  )

  for (elem in elements) {
    elem_xml <- svg_element_to_netlogo(elem)
    if (elem_xml != "") {
      xml_lines <- c(xml_lines, elem_xml)
    }
  }

  xml_lines <- c(xml_lines, '    </shape>')

  paste(xml_lines, collapse = "\n")
}

#' Convert multiple SVG files to a complete NetLogo shapes XML file
#' @param svg_files Vector of paths to SVG files
#' @param output_file Path to output XML file (optional)
#' @return String containing complete NetLogo turtleShapes XML
convert_multiple_svg_to_netlogo <- function(svg_files, output_file = NULL) {
  xml_lines <- c('<turtleShapes>')

  for (svg_file in svg_files) {
    cat("Processing:", svg_file, "\n")
    shape_xml <- convert_svg_to_netlogo(svg_file)
    xml_lines <- c(xml_lines, shape_xml)
  }

  xml_lines <- c(xml_lines, '</turtleShapes>')

  result <- paste(xml_lines, collapse = "\n")

  if (!is.null(output_file)) {
    writeLines(result, output_file)
    cat("Saved to:", output_file, "\n")
  }

  invisible(result)
}

# --- Main execution ---
if (interactive()) {
  cat("To convert an SVG to NetLogo XML, run:\n")
  cat('  netlogo_xml <- convert_svg_to_netlogo("my_shape.svg")\n')
  cat('  cat(netlogo_xml)\n\n')
  cat("To convert multiple SVGs:\n")
  cat(
    '  svg_files <- list.files("svg_shapes", pattern = "\\\\.svg$", full.names = TRUE)\n'
  )
  cat('  convert_multiple_svg_to_netlogo(svg_files, "output_shapes.xml")\n')
}
