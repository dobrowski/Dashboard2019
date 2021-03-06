library(htmlwidgets)
library(ggthemes)
library(DT)

#### Changes ----

changes.df <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           rtype == "D"
    ) %>% 
    select(cds, districtname, studentgroup, ind, color, year) %>%
    na.omit() %>%
    pivot_wider(names_prefix = "color." , names_from = year, values_from = color) %>%
    filter(color.2018 != 0 & color.2019 != 0) %>%
    mutate(color.change = color.2019 - color.2018) %>%
    filter(color.change != 0)

changes.dt <- changes.df %>% datatable( options = list(pageLength = 20))

saveWidget(widget = changes.dt, file =  "Changes in Color from Last Year.html", selfcontained = TRUE )


ggplot(changes.df) +
    geom_bar(aes(x = color.change)) +
    theme_hc() +
    labs(title = "There were an equivalent number of indicators that increased color as declined.",
         x = "Change in Color",
         y = "Number of indicators")


ggsave(here("figs","Color Change.png"), width = 10, height = 6)
