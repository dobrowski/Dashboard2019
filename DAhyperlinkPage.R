library(kableExtra)

inches <- 3

DAlist <- mry.DA %>% 
    ungroup() %>% 
    select(districtname) %>% 
    distinct() %>%
    simplify()

DAlist <- DAlist[2:6]

dis <- DAlist[1]

for (i in DAlist) {
 
    dis <- i

all %>%
    filter(str_detect(countyname, "Monterey"), 
           str_detect(districtname, dis), 
           color.factor %in% c(1,2),
           year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    ggplot( aes(ind, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y=""#,
        # title = paste0(dis," Red and Orange Indicators for ", yrs)
         )

ggsave(here("figs", "DA", paste0(dis," Red and Orange.png") ), width = inches, height = inches)

all %>%
    filter(str_detect(countyname, "Monterey"), 
           str_detect(districtname, dis), 
           color.factor %in% c(1),
           year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    ggplot( aes(ind, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y=""#,
         #title = paste0(dis," Red Indicators for ", yrs)
         )

ggsave(here("figs", "DA", paste0(dis," Red.png") ), width = inches, height = inches)




all %>%
    filter(str_detect(countyname, "Monterey"), 
           str_detect(districtname, dis), 
           color.factor %in% c(4,5),
           year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    ggplot( aes(ind, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y=""#,
      #   title = paste0(dis," Blue and Green Indicators for ", yrs)
      )

ggsave(here("figs", "DA", paste0(dis," Blue and Green.png") ), width = inches, height = inches)




quals <- mry.DA %>% 
    ungroup() %>%
    filter(str_detect(districtname, dis)) %>%
    select(districtname, studentgroup, starts_with("Prior")) %>% 
    knitr::kable(booktabs = T,
                 escape = F,
                 col.names = linebreak(c("District",
                                             "Student Group",
                                         "ELA, Math, ELPI",
                                             "Graduation, Absenteeism",
                                             "Suspension",
                                             "College/Career"
                                             ))) %>%
    add_header_above(c(" " = 2, "Pupil Achievement" = 1, "Pupil Engagement" = 1,"School Climate" = 1,"Broad Course of Study" = 1)) %>%
    add_header_above(c(" " = 2, "Priority 4" = 1, "Priority 5" = 1,"Priority 6" = 1,"Priority 8" = 1)) %>%
    kable_styling(latex_options = c("striped", "scale_down")) 

save_kable(quals ,  paste0(dis," Qualifying Student Groups.png") )
}
