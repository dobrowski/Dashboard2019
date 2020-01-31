



graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"),
           str_detect(districtname, "Salinas U"), 
           studentgroup %in% c("HOM", "EL", "SWD"),
   #        ind == "math",
           #     year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) 


ggplot(graphthis, aes(year, y = fct_rev(ind))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~studentgroup, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Salinas UHSD Student Groups over Three Years")
        #, caption = capt
         )



ggsave(here("figs","SUHSD Subgroups 3 years Colors.png"), width = 10, height = 6)
