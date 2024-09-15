library("assertthat")
library("dplyr")
library("sf")
library("ggplot2")

ca = sf::read_sf("data/scotland_ca_2019.shp")
ca = rmapshaper::ms_simplify(ca, keep = 0.05)

oa = 
    sf::read_sf("data/scotland_oa_2022.shp") |>
    select(geoid, council) |>
    st_drop_geometry()

ethnic = 
    readr::read_csv("data/UV201 - Ethnic group.csv", skip = 4, na = "-", show_col_types = FALSE) |>
    rename(
        oa_code = code,
        total = `All People`,
        irish = `White: White Irish`
    ) |>
    select(oa_code, irish, total) |>
    mutate(irish = if_else(is.na(irish), 0, irish))

ethnic =
    left_join(oa, ethnic, by = c("geoid" = "oa_code")) |>
    group_by(council) |>
    summarise(irish = sum(irish), total = sum(total)) |>
    ungroup()

assertthat::assert_that(assertthat::are_equal(
    nrow(anti_join(ca, ethnic, by = c("geoid" = "council"))),
    0L
))

assert_that(are_equal(56766L, sum(ethnic$irish)))  # manual count from spreadsheet

ca = left_join(ca, ethnic, by = c("geoid" = "council"))

ca =
    ca |>
    mutate(`% White Irish` = (irish / total) * 100)

map = ggplot(data = ca) +
    geom_sf(aes(fill = `% White Irish`))
ggsave("figures/scot-ca-irish.pdf", map, width = 210, height = 297, units = "mm", dpi = 300)
