library(tidyverse)

gh_orgs <- read_file("https://api.github.com/users/hadley/orgs")
gh_orgs <- gh_orgs %>%
  json_unnest()

usethis::use_data(gh_orgs, overwrite = TRUE)


gh_commits_vec <- gh_commits %>%
  jq_do2(".[0:4]") %>%
  json_unnest() %>%
  json_build_object(sha, commit)

gh_commits_vec <- c(NA, gh_commits_vec)

usethis::use_data(gh_commits_vec, overwrite = TRUE)
