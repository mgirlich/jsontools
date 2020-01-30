test_that("translations work", {
  skip("no proper tests yet")
  con <- dbplyr::simulate_dbi("PqConnection")

  dbplyr::translate_sql(ymd(x), con = con)
  dbplyr::translate_sql(jsonr_extract_int(x, ".a.b"), con = con)

  dbplyr::tbl_lazy(
    dplyr::tibble(extra_information = character(), date_id = character()),
    con = con
  ) %>%
    dplyr::filter(
      jsonr_has_key(extra_information, "spa_campaign_id"),
      ymd(date_id)
    ) %>%
    dplyr::mutate(
      spa_camp_id = jsonr_extract_int(extra_information, ".spa_campaign_id")
    ) %>%
    dplyr::show_query()
})
