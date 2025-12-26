# Generated with Posit Databot and Claude Opus 4.5

# --------------------------------------------------

# Convert NetLogo Turtle Shapes XML to Inkscape-compatible SVG files
#
# This script reads a turtle_shapes.xml file and converts each shape
# to an Inkscape-compatible SVG file with proper path commands and colors.

library(xml2)

# --- NetLogo Color Mapping ---
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

# Create reverse lookup: XML color code -> hex
xml_to_hex <- setNames(netlogo_base_colors, netlogo_xml_colors)

#' Convert NetLogo XML color code to hex color
#' @param color_int The integer color code from the XML
#' @return Hex color string (e.g., "#8D8D8D")
netlogo_color_to_hex <- function(color_int) {
  color_key <- as.character(as.integer(color_int))
  if (color_key %in% names(xml_to_hex)) {
    return(xml_to_hex[[color_key]])
  }
  # Default to gray for unknown colors
  return("#8D8D8D")
}

#' Convert polygon points to Inkscape-style relative path
#' @param points_str String of points in "x1,y1 x2,y2 ..." format
#' @return SVG path data string with relative commands
points_to_inkscape_path <- function(points_str) {
  pairs <- strsplit(points_str, " ")[[1]]
  coords <- do.call(
    rbind,
    lapply(pairs, function(p) {
      as.numeric(strsplit(p, ",")[[1]])
    })
  )

  if (nrow(coords) < 2) {
    return("")
  }

  path_parts <- sprintf("m %g,%g", coords[1, 1], coords[1, 2])
  last_cmd <- "m"

  for (i in 2:nrow(coords)) {
    dx <- coords[i, 1] - coords[i - 1, 1]
    dy <- coords[i, 2] - coords[i - 1, 2]

    if (dy == 0 && dx != 0) {
      path_parts <- c(path_parts, sprintf("h %g", dx))
      last_cmd <- "h"
    } else if (dx == 0 && dy != 0) {
      path_parts <- c(path_parts, sprintf("v %g", dy))
      last_cmd <- "v"
    } else {
      # For diagonal: include 'l' only if previous wasn't a diagonal
      if (last_cmd %in% c("l", "m")) {
        path_parts <- c(path_parts, sprintf("%g,%g", dx, dy))
      } else {
        path_parts <- c(path_parts, sprintf("l %g,%g", dx, dy))
      }
      last_cmd <- "l"
    }
  }

  path_parts <- c(path_parts, "Z")
  paste(path_parts, collapse = " ")
}

#' Create Inkscape-compatible SVG from a shape node
#' @param shape_node An xml_node representing a NetLogo shape
#' @param canvas_size The size of the SVG canvas (default 300)
#' @return String containing the complete SVG content
create_inkscape_svg <- function(shape_node, canvas_size = 300) {
  shape_name <- xml_attr(shape_node, "name")

  svg_lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">',
      canvas_size,
      canvas_size,
      canvas_size,
      canvas_size
    )
  )

  elements <- xml_children(shape_node)
  elem_id <- 1

  for (elem in elements) {
    elem_name <- xml_name(elem)

    if (elem_name == "polygon") {
      color <- netlogo_color_to_hex(as.numeric(xml_attr(elem, "color")))
      filled <- tolower(xml_attr(elem, "filled")) == "true"

      points <- xml_find_all(elem, "point")
      point_str <- paste(
        sapply(points, function(p) {
          sprintf("%s,%s", xml_attr(p, "x"), xml_attr(p, "y"))
        }),
        collapse = " "
      )

      path_d <- points_to_inkscape_path(point_str)

      if (filled) {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <path id="polygon%d" style="fill:%s" d="%s" />',
            elem_id,
            color,
            path_d
          )
        )
      } else {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <path id="polygon%d" style="fill:none;stroke:%s;stroke-width:1" d="%s" />',
            elem_id,
            color,
            path_d
          )
        )
      }
      elem_id <- elem_id + 1
    } else if (elem_name == "circle") {
      x <- as.numeric(xml_attr(elem, "x"))
      y <- as.numeric(xml_attr(elem, "y"))
      diameter <- as.numeric(xml_attr(elem, "diameter"))
      color <- netlogo_color_to_hex(as.numeric(xml_attr(elem, "color")))
      filled <- tolower(xml_attr(elem, "filled")) == "true"

      cx <- x + diameter / 2
      cy <- y + diameter / 2
      r <- diameter / 2

      if (filled) {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <circle id="circle%d" style="fill:%s" cx="%.1f" cy="%.1f" r="%.1f" />',
            elem_id,
            color,
            cx,
            cy,
            r
          )
        )
      } else {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <circle id="circle%d" style="fill:none;stroke:%s;stroke-width:1" cx="%.1f" cy="%.1f" r="%.1f" />',
            elem_id,
            color,
            cx,
            cy,
            r
          )
        )
      }
      elem_id <- elem_id + 1
    } else if (elem_name == "line") {
      startX <- xml_attr(elem, "startX")
      startY <- xml_attr(elem, "startY")
      endX <- xml_attr(elem, "endX")
      endY <- xml_attr(elem, "endY")
      color <- netlogo_color_to_hex(as.numeric(xml_attr(elem, "color")))

      svg_lines <- c(
        svg_lines,
        sprintf(
          '  <line id="line%d" style="stroke:%s;stroke-width:1" x1="%s" y1="%s" x2="%s" y2="%s" />',
          elem_id,
          color,
          startX,
          startY,
          endX,
          endY
        )
      )
      elem_id <- elem_id + 1
    } else if (elem_name == "rectangle") {
      startX <- as.numeric(xml_attr(elem, "startX"))
      startY <- as.numeric(xml_attr(elem, "startY"))
      endX <- as.numeric(xml_attr(elem, "endX"))
      endY <- as.numeric(xml_attr(elem, "endY"))
      color <- netlogo_color_to_hex(as.numeric(xml_attr(elem, "color")))
      filled <- tolower(xml_attr(elem, "filled")) == "true"

      x <- min(startX, endX)
      y <- min(startY, endY)
      width <- abs(endX - startX)
      height <- abs(endY - startY)

      if (filled) {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <rect id="rect%d" style="fill:%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f" />',
            elem_id,
            color,
            x,
            y,
            width,
            height
          )
        )
      } else {
        svg_lines <- c(
          svg_lines,
          sprintf(
            '  <rect id="rect%d" style="fill:none;stroke:%s;stroke-width:1" x="%.1f" y="%.1f" width="%.1f" height="%.1f" />',
            elem_id,
            color,
            x,
            y,
            width,
            height
          )
        )
      }
      elem_id <- elem_id + 1
    }
  }

  svg_lines <- c(svg_lines, '</svg>')
  paste(svg_lines, collapse = "\n")
}

#' Convert all shapes from a turtle_shapes.xml file to SVG files
#' @param xml_file Path to the turtle_shapes.xml file
#' @param output_dir Directory to save SVG files (created if doesn't exist)
#' @return Data frame with shape names and success status
convert_turtle_shapes_to_svg <- function(xml_file, output_dir = "svg_shapes") {
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)

  # Read XML file
  xml_content <- read_xml(xml_file)

  # Get all shapes
  shapes <- xml_find_all(xml_content, "//shape")
  shape_names <- xml_attr(shapes, "name")

  cat("Processing", length(shapes), "shapes...\n")

  results <- data.frame(
    name = character(),
    filename = character(),
    success = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(shapes)) {
    shape_node <- shapes[[i]]
    name <- shape_names[i]
    filename <- paste0(output_dir, "/", gsub(" ", "_", name), ".svg")

    success <- tryCatch(
      {
        svg_content <- create_inkscape_svg(shape_node)
        writeLines(svg_content, filename)
        TRUE
      },
      error = function(e) {
        cat("Error with shape:", name, "-", e$message, "\n")
        FALSE
      }
    )

    results <- rbind(
      results,
      data.frame(
        name = name,
        filename = filename,
        success = success,
        stringsAsFactors = FALSE
      )
    )
  }

  cat("\nCompleted!\n")
  cat("Successfully created:", sum(results$success), "SVG files\n")
  if (any(!results$success)) {
    cat("Errors:", sum(!results$success), "\n")
  }

  invisible(results)
}

# --- Main execution ---

if (interactive()) {
  cat("To convert turtle shapes to SVG, run:\n")
  cat(
    '  results <- convert_turtle_shapes_to_svg("turtle_shapes.xml", "svg_shapes")\n'
  )
} else {
  # When sourced or run from command line
  if (file.exists("turtle_shapes.xml")) {
    results <- convert_turtle_shapes_to_svg("turtle_shapes.xml", "svg_shapes")
  }
}
