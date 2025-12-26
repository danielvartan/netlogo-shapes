# Load Packages -----

library(beepr)
library(fs)
library(groomr) # github.com/danielvartan/groomr
library(here)
library(readr)
library(stringr)

# Remove Empty Lines from `README.md` -----

here("README.md") |> remove_blank_line_dups()

# Fix Image Links in `README.md` -----

readme_files_dir <- here("README_files")
readme_images_dir <- path(readme_files_dir, "figure-commonmark")
images_dir <- here("images")

if (dir.exists(readme_files_dir)) {
  if (dir.exists(readme_images_dir)) {
    dir_map(
      path = readme_images_dir,
      \(x) file_move(x, path(images_dir, basename(x)))
    )
  }

  dir_delete(readme_files_dir)
}

file <- here("README.md")

file |>
  read_lines() |>
  str_replace_all(
    pattern = "README_files/figure-commonmark/",
    replacement = "images/"
  ) |>
  write_lines(file)

# Check If the Script Ran Successfully -----

beep(1)

Sys.sleep(3)
