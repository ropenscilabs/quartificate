#' Transform Google Doc to Quarto book
#'
#' @param gdoc_id ID of the Google Document.
#' @param path A path, where to create the Quarto book. It will be created if needed.
#' @param render Logical. Whether to render the resulting Quarto book.
#' @param fix_lists Logical. Whether to try and fix lists, see Details.
#'
#' @details If your Google Document contains lists whose items span several lines,
#' you might get better results with the `fix_lists` parameter set to `TRUE`.
#' The problem is that in Google Docs lists, from the second line lines in items
#' have a small indentation. Pandoc tends to interpret this as a blockquote.
#' We try to fix that by merging blockquotes in their previous sibling,
#' when that previous sibling is a list item.
#'
#' @return Nothing.
#' @export
#'
#' @examples id <- googledrive::drive_find(
#'     q = "name contains 'rOpenSci Champions Program Mentor Guidelines v1.0'"
#'   )$id
#'  quarto_dir <- withr::local_tempdir()
#'  quartificate::quartificate(id, quarto_dir, render = TRUE)
quartificate <- function(gdoc_id, path, render = FALSE, fix_lists = FALSE) {
  # download files from Google Drive ----
  from_gdoc <- withr::local_tempdir()
  fs::dir_create(from_gdoc)

  googledrive::drive_download(
    googledrive::as_id(gdoc_id),
    type = "docx",
    path = file.path(from_gdoc, "gdoc.docx")
  )

  # Convert docx ----

  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
  }

  withr::with_dir(path, {
    pandoc::pandoc_convert(
      file = file.path(from_gdoc, "gdoc.docx"),
      from = "docx",
      to = "gfm-raw_html",
      args = sprintf("--wrap=none  --extract-media %s", "images"),
      output = "raw.md"
    )
  })

  # meta ----
  meta <- googledrive::drive_get(googledrive::as_id(gdoc_id))
  title <- meta[["name"]]

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
  fs::file_delete(file.path(path, "raw.md"))

  html <- xml2::read_html(out_temp_file, encoding = "utf-8")

  sections <- xml2::xml_find_all(html, ".//section[@class='level1']")
  ids <- purrr::map_chr(sections, treat_section, path = path, fix_lists)

  # use first part as index.qmd if no preamble
  kiddos <- xml2::xml_children(xml2::xml_child(html))
  first_section <- min(which(xml2::xml_name(kiddos) == "section" & xml2::xml_attr(kiddos, "class") == "level1"))
  no_preamble <- (first_section == 1)
  if (no_preamble) {
    fs::file_move(
      file.path(path, sprintf("%s.qmd", ids[1])),
      file.path(path, "index.qmd")
    )
    chapters <- sprintf("    - %s.qmd", c("index", ids[-1])) |> paste(collapse="\n")
  } else {
    preamble <- kiddos[seq_len(first_section - 1)]
    temp_html <- withr::local_tempfile()
    temp_md <- withr::local_tempfile()
    brio::write_lines(as.character(preamble), temp_html)
    pandoc::pandoc_convert(
      file = temp_html,
      from = "html",
      to = "gfm",
      output = temp_md
    )
    md_lines <- brio::read_lines(temp_md)
    md_lines <- c(sprintf("# %s {.unnumbered}", title), "", md_lines)
    brio::write_lines(md_lines, temp_md)
    fs::file_move(
      temp_md,
      file.path(path, "index.qmd")
    )
    chapters <- sprintf("    - %s.qmd", c("index", ids)) |> paste(collapse="\n")
  }



  # prepare Quarto config ----


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

treat_section <- function(section, path, fix_lists) {
  id <- xml2::xml_attr(section, "id")
  blockquotes <- xml2::xml_find_all(section, ".//blockquote")
  if (fix_lists && length(blockquotes) > 0) {
    purrr::walk(blockquotes, fix_blockquote)
  }

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

fix_blockquote <- function(blockquote) {

  previous_siblings <- xml2::xml_find_all(blockquote, "preceding-sibling::*")
  if (length(previous_siblings) == 0) {
    return()
  }
  closest_sibling <- previous_siblings[length(previous_siblings)]

  if (!xml2::xml_name(closest_sibling) %in% c("ul", "ol", "li")) {
    return()
  }

  last_li <- if (xml2::xml_name(closest_sibling) %in% c("ul", "ol")) {
    xml2::xml_children(closest_sibling)[length(xml2::xml_children(closest_sibling))]
  } else {
    closest_sibling
  }
  purrr::walk(
    xml2::xml_contents(blockquote),
    ~xml2::xml_add_child(last_li, .x, .where = "after")
  )
  xml2::xml_remove(blockquote)

}
