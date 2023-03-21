#' Transform Google Doc to Quarto book
#'
#' @param gdoc_id ID of the Google Document.
#' @param path A path, where to create the Quarto book. It will be created if needed.
#' @param render Logical. Whether to render the resulting Quarto book.
#'
#' @return Nothing.
#' @export
#'
#' @examples id <- googledrive::drive_find(
#'     q = "name contains 'rOpenSci Champions Program Mentor Guidelines v1.0'"
#'   )$id
#'  quarto_dir <- withr::local_tempdir()
#'  quartificate::quartificate(id, quarto_dir, render = TRUE)
quartificate <- function(gdoc_id, path, render = FALSE) {
  # download files from Google Drive ----
  from_gdoc <- withr::local_tempdir()
  fs::dir_create(from_gdoc)

  googledrive::drive_download(
    googledrive::as_id(gdoc_id),
    type = "docx",
    path = file.path(from_gdoc, "gdoc.docx")
  )

  temp_dir <- withr::local_tempdir()

  withr::with_dir(temp_dir, {
    googledrive::drive_download(
      googledrive::as_id(gdoc_id),
      type = "application/zip"
    )
  })
  zip_file <- fs::dir_ls(temp_dir, glob = "*.zip")
  utils::unzip(zip_file, exdir = temp_dir)
  fs::dir_copy(file.path(temp_dir, "images"), file.path(from_gdoc, "images"))

  # Convert docx ----

  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
  }

  pandoc::pandoc_convert(
    file = file.path(from_gdoc, "gdoc.docx"),
    from = "docx",
    to = "gfm-raw_html",
    output = file.path(path, "raw.md")
  )

  fs::dir_copy(
    file.path(from_gdoc, "images"),
    file.path(path, "media")
  )

  # TODO Fix quotes in list ----
  # TODO Fix "smart" quotes (punctuation signs not block like above) ----
  # convert to HTML with sections ----
  out_temp_file <- withr::local_tempfile(fileext = ".html")
  withr::with_dir(path, {
    pandoc::pandoc_run(
      c(
        "-t", "html", # output format
        "--wrap=preserve", # preserve soft linebreaks
        "--no-highlight",
        "-f", "gfm-autolink_bare_uris", # input format, do not transform bare URIs into links
        "-o", out_temp_file, # output temp file
        "raw.md",
        "--section-divs" # wrap sections into divs (for parsing)
      )
    )
  })
  html <- xml2::read_html(out_temp_file)
  sections <- xml2::xml_find_all(html, ".//section[@class='level1']")
  ids <- purrr::map_chr(sections, treat_section, path = path)

  # use first part as index.qmd
  fs::file_move(
    file.path(path, sprintf("%s.qmd", ids[1])),
    file.path(path, "index.qmd")
  )

  chapters <- sprintf("    - %s.qmd", c("index", ids[-1])) |> paste(collapse="\n")

  # prepare Quarto config ----

  meta <- googledrive::drive_get(googledrive::as_id(gdoc_id))
  title <- meta[["name"]]
  author <- meta[["drive_resource"]][[1]][["owners"]][[1]][["displayName"]]
  date <- as.character(Sys.Date())
  config_template <- system.file("quarto-config-template.yaml", package = "quartificate")
  whisker::whisker.render(
    brio::read_lines(config_template) |> paste(collapse = "\n")
  ) |>
    brio::write_lines(file.path(path, "_quarto.yml"))
  # TODO put preamble (anything before first highest level header) under a preamble header

  if (render) {
    withr::with_dir(path, {
      quarto::quarto_render()
    })
  }
}

treat_section <- function(section, path) {
  id <- xml2::xml_attr(section, "id")
  lists <- xml2::xml_find_all(section, ".//li")
  fix_list <- function(list) {
    blockquotes <- xml2::xml_find_all(list, ".//blockquote")
    fix_blockquote <- function(blockquote) {
      parent <- xml2::xml_parent(blockquote)
      contents <- xml2::xml_contents(blockquote)
      purrr::walk(contents, ~xml2::xml_add_child(parent, .x))
      xml2::xml_remove(blockquote)
    }
    if (length(blockquotes) > 0) {
      purrr::walk(blockquotes, fix_blockquote)
    }
  }
  purrr::walk(lists, fix_list)
  temp_file <- withr::local_tempfile()
  xml2::write_html(section, temp_file)
  md_temp_file <- withr::local_tempfile()
  pandoc::pandoc_convert(
    file = temp_file,
    from = "html",
    to = "gfm-raw_html",
    output = md_temp_file,
  )
  fs::file_copy(md_temp_file, file.path(path, sprintf("%s.qmd", id)))
  return(id)
}
