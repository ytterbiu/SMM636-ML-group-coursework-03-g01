render_clean <- function(file_path, ...) {
  # 1. Get the base name of the file (e.g., "cm2-assessment01-group08")
  base_name <- tools::file_path_sans_ext(file_path)

  # 2. List the common LaTeX auxiliary extensions that cause corruption
  bad_exts <- c(
    "aux",
    "toc",
    "out",
    "log",
    "fls",
    "fdb_latexmk",
    "lof",
    "lot",
    "tex"
  )

  # 3. Create the full file paths to delete
  files_to_delete <- paste0(base_name, ".", bad_exts)

  # 4. Delete them (unlink fails silently if the file doesn't exist, which is perfect)
  unlink(files_to_delete)
  message("Cleaned up old auxiliary files.")

  # 5. Render the document as normal
  rmarkdown::render(file_path, ...)
}

# How to use it:
# render_clean("01SMM363-assessment-03.rmd", output_format = "all")
