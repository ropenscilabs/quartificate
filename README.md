
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
