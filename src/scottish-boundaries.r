library("dplyr")
library("sf")
library("ggplot2")

ca = sf::read_sf("data/scotland_ca_2019.shp")
oa = sf::read_sf("data/scotland_oa_2022.shp")

ethnic = 
    readr::read_csv("data/UV201 - Ethnic group.csv", skip = 4, na = "-", show_col_types = FALSE) |>
    rename(
        total = `All People`,
        irish = `White: White Irish`
    ) |>
    select(code, irish, total) |>
    mutate(irish = if_else(is.na(irish), 0, irish)) |>
    mutate(p_irish = irish / total)

oa = left_join(oa, ethnic, by = c("geoid" = "code"))

print(oa)
saveRDS(oa, file = "oa.rds", compress = FALSE)

map = ggplot(data = ca) +
    geom_sf()
ggsave("figures/scot-ca.pdf", map, width = 210, height = 297, units = "mm", dpi = 300)

# map = ggplot(data = oa) +
#     geom_sf(aes(fill = p_irish))
# ggsave("figures/scot-oa-irish.pdf", map, width = 210, height = 297, units = "mm", dpi = 300)
