
#  Chart for Denise about math colors over time


graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           ind == "math",
      #     year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) 


ggplot(graphthis, aes(year, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Math Colors on Dashboard over Three Years"),
         caption = capt)



ggsave(here("figs","Math Dashbaord Colors.png"), width = 16, height = 16)
