
# quartificate

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of quartificate is to transform Google Docs into Quarto books.

## Installation

You can install the development version of quartificate from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropenscilabs/quartificate")
```

## Example

Say you have created a Google Document with several headers (level 1), 
and are now ready to maintain it as a Quarto book instead.
I created [My Example Document](https://docs.google.com/document/u/1/d/1WCHXxoc99rYTxElvAtapBotG7jd1WWEwR2ZUjTDgOYI/edit), maybe you can duplicate it to give it a try?

For authentication refer to [googledrive documentation](https://googledrive.tidyverse.org/index.html).

```r
id <- googledrive::drive_find(
    q = "name contains 'My Example Document'"
  )$id
quartificate::quartificate(id, path = "../example-doc", render = TRUE)
servr::httw(file.path("../example-doc", "docs"))
```

I get this [Quarto book](https://sprightly-conkies-115109.netlify.app/).

## Details

The package downloads a docx export of your Google Document.
It converts it to Markdown using Pandoc, extracting images at the same time.
Then it converts the Markdown document to an HTML divided into sections: each part started with a header level 1 is a section and will become a book chapter.
Content that's before the first level 1 header is put into `index.qmd`.
If there's no content before the first level 1 header, the first chapter is used as `index.qmd`.
