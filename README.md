
<!-- README.md is generated from README.Rmd. Please edit that file -->

# json2tools

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/jsontools)](https://CRAN.R-project.org/package=jsontools)
<!-- badges: end -->

## Overview

With the increasing support of JSON in databases and since 2016 even
support in the SQL standard, JSON data in vectors become more common.
With jsontools one can easily work with JSON vectors in R. The three
main parts are: 1. parsing from JSON and converting to JSON 2. direct
manipulation of JSON without parsing and conversion over R 3. extract
data from JSON without parsing everything

## Installation

jsontools is not yet release on CRAN. You can install it from GitHub
with

``` r
# install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(jsontools)

gh_orgs[1]
#> {"login":"ggobi","id":423638,"node_id":"MDEyOk9yZ2FuaXphdGlvbjQyMzYzOA==","url":"https://api.github.com/orgs/ggobi","repos_url":"https://api.github.com/orgs/ggobi/repos","events_url":"https://api.github.com/orgs/ggobi/events","hooks_url":"https://api.github.com/orgs/ggobi/hooks","issues_url":"https://api.github.com/orgs/ggobi/issues","members_url":"https://api.github.com/orgs/ggobi/members{/member}","public_members_url":"https://api.github.com/orgs/ggobi/public_members{/member}","avatar_url":"https://avatars1.githubusercontent.com/u/423638?v=4","description":""}

gh_orgs <- prettify(gh_orgs)
gh_orgs[1]
#> {
#>     "login": "ggobi",
#>     "id": 423638,
#>     "node_id": "MDEyOk9yZ2FuaXphdGlvbjQyMzYzOA==",
#>     "url": "https://api.github.com/orgs/ggobi",
#>     "repos_url": "https://api.github.com/orgs/ggobi/repos",
#>     "events_url": "https://api.github.com/orgs/ggobi/events",
#>     "hooks_url": "https://api.github.com/orgs/ggobi/hooks",
#>     "issues_url": "https://api.github.com/orgs/ggobi/issues",
#>     "members_url": "https://api.github.com/orgs/ggobi/members{/member}",
#>     "public_members_url": "https://api.github.com/orgs/ggobi/public_members{/member}",
#>     "avatar_url": "https://avatars1.githubusercontent.com/u/423638?v=4",
#>     "description": ""
#> }

json_glimpse(gh_orgs[1])
#> # A JSON object with 12 elements
#> $ login              <string>[5] ggobi
#> $ id                 <number>[423638] 423638
#> $ node_id            <string>[32] MDEyOk9yZ2FuaXphdGlvbjQyMzYzOA==
#> $ url                <string>[33] https://api.github.com/orgs/ggobi
#> $ repos_url          <string>[39] https://api.github.com/orgs/ggobi/repos
#> $ events_url         <string>[40] https://api.github.com/orgs/ggobi/events
#> $ hooks_url          <string>[39] https://api.github.com/orgs/ggobi/hooks
#> $ issues_url         <string>[40] https://api.github.com/orgs/ggobi/issues
#> $ members_url        <string>[50] https://api.github.com/orgs/ggobi/members{/…
#> $ public_members_url <string>[57] https://api.github.com/orgs/ggobi/public_me…
#> $ avatar_url         <string>[51] https://avatars1.githubusercontent.com/u/42…
#> $ description        <string>[0]

gh_orgs_small <- gh_orgs %>% 
  json_build_object(login, id, description)

parse_json_vector(gh_orgs_small, simplifyDataFrame = TRUE)
#> [[1]]
#> [[1]]$login
#> [1] "ggobi"
#> 
#> [[1]]$id
#> [1] 423638
#> 
#> [[1]]$description
#> [1] ""
#> 
#> 
#> [[2]]
#> [[2]]$login
#> [1] "rstudio"
#> 
#> [[2]]$id
#> [1] 513560
#> 
#> [[2]]$description
#> NULL
#> 
#> 
#> [[3]]
#> [[3]]$login
#> [1] "rstats"
#> 
#> [[3]]$id
#> [1] 722735
#> 
#> [[3]]$description
#> NULL
#> 
#> 
#> [[4]]
#> [[4]]$login
#> [1] "ropensci"
#> 
#> [[4]]$id
#> [1] 1200269
#> 
#> [[4]]$description
#> [1] ""
#> 
#> 
#> [[5]]
#> [[5]]$login
#> [1] "rjournal"
#> 
#> [[5]]$id
#> [1] 3330561
#> 
#> [[5]]$description
#> NULL
#> 
#> 
#> [[6]]
#> [[6]]$login
#> [1] "r-dbi"
#> 
#> [[6]]$id
#> [1] 5695665
#> 
#> [[6]]$description
#> [1] "R + databases"
#> 
#> 
#> [[7]]
#> [[7]]$login
#> [1] "RConsortium"
#> 
#> [[7]]$id
#> [1] 15366137
#> 
#> [[7]]$description
#> [1] "The R Consortium, Inc was established to provide support to the R Foundation and R Community, using maintaining and distributing R software."
#> 
#> 
#> [[8]]
#> [[8]]$login
#> [1] "tidyverse"
#> 
#> [[8]]$id
#> [1] 22032646
#> 
#> [[8]]$description
#> [1] "The tidyverse is a collection of R packages that share common principles and are designed to work together seamlessly"
#> 
#> 
#> [[9]]
#> [[9]]$login
#> [1] "r-lib"
#> 
#> [[9]]$id
#> [1] 22618716
#> 
#> [[9]]$description
#> [1] ""
#> 
#> 
#> [[10]]
#> [[10]]$login
#> [1] "rstudio-education"
#> 
#> [[10]]$id
#> [1] 34165516
#> 
#> [[10]]$description
#> NULL

(logins <- jsonr_extract_chr(gh_orgs_small, ".login"))
#>  [1] "ggobi"             "rstudio"           "rstats"           
#>  [4] "ropensci"          "rjournal"          "r-dbi"            
#>  [7] "RConsortium"       "tidyverse"         "r-lib"            
#> [10] "rstudio-education"

json_mutate(gh_orgs, .login = paste0("new-", logins))[1:2] %>% 
  prettify()
#> {
#>     "login": "new-ggobi",
#>     "id": 423638,
#>     "node_id": "MDEyOk9yZ2FuaXphdGlvbjQyMzYzOA==",
#>     "url": "https://api.github.com/orgs/ggobi",
#>     "repos_url": "https://api.github.com/orgs/ggobi/repos",
#>     "events_url": "https://api.github.com/orgs/ggobi/events",
#>     "hooks_url": "https://api.github.com/orgs/ggobi/hooks",
#>     "issues_url": "https://api.github.com/orgs/ggobi/issues",
#>     "members_url": "https://api.github.com/orgs/ggobi/members{/member}",
#>     "public_members_url": "https://api.github.com/orgs/ggobi/public_members{/member}",
#>     "avatar_url": "https://avatars1.githubusercontent.com/u/423638?v=4",
#>     "description": ""
#> }
#> 
#> {
#>     "login": "new-rstudio",
#>     "id": 513560,
#>     "node_id": "MDEyOk9yZ2FuaXphdGlvbjUxMzU2MA==",
#>     "url": "https://api.github.com/orgs/rstudio",
#>     "repos_url": "https://api.github.com/orgs/rstudio/repos",
#>     "events_url": "https://api.github.com/orgs/rstudio/events",
#>     "hooks_url": "https://api.github.com/orgs/rstudio/hooks",
#>     "issues_url": "https://api.github.com/orgs/rstudio/issues",
#>     "members_url": "https://api.github.com/orgs/rstudio/members{/member}",
#>     "public_members_url": "https://api.github.com/orgs/rstudio/public_members{/member}",
#>     "avatar_url": "https://avatars1.githubusercontent.com/u/513560?v=4",
#>     "description": null
#> }
```
