
Dist <- "Salinas City"

DAlist <- mry.DA %>% 
    ungroup() %>% 
    select(districtname) %>% 
    distinct() %>%
    simplify()



DAlist <- c("Gonzales", "San Antonio", "Greenfield", "Salinas City")



for (Dist in DAlist) {
    


ind_ord <- c("chronic","susp","grad","cci","elpi", "ela","math")
sg_ord <- c(   "MR", "WH","PI",  "HI", "FI","AS", "AI","AA",   "SWD","SED", "HOM","FOS", "EL", "ALL") # "ELO", "EO",



graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           str_detect(districtname, Dist),
           studentgroup %notin% c("ELO", "EO"),
           year == yrs,
           #            charter_flag == "Y"
           rtype == "D"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate( combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    mutate(ind = factor(ind, levels = ind_ord),
           studentgroup = factor(studentgroup, levels = sg_ord))


ggplot(graphthis, aes(x = ind, y = studentgroup)) + 
    geom_tile(aes(fill = color.factor)) +
    geom_text(aes(label = round(change, 1))) +
  #  lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    scale_x_discrete(position = "top") +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Student Groups with Change Amounts \nfor ", Dist)
        # caption = capt
         )

ggsave(here("figs", "DAstudentgroupchange", paste0(Dist," Student Group Change.png")), width = 8, height = 5)

}




###  For all schools in a district ---------


Dist <- "Salinas City"
sch <- "Salinas High"


SUlist <- all %>%
  filter(str_detect(countyname, "Monterey")   , 
         str_detect(districtname, "Salinas U")) %>% 
  ungroup() %>% 
  select(schoolname) %>% 
  distinct() %>%
  simplify()


for (sch in SUlist) {
  
  
  
  ind_ord <- c("chronic","susp","grad","cci","elpi", "ela","math")
  sg_ord <- c(   "MR", "WH","PI",  "HI", "FI","AS", "AI","AA",   "SWD","SED", "HOM","FOS", "EL", "ALL") # "ELO", "EO",
  
  
  
  graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           str_detect(schoolname, paste0("^",sch)),
           studentgroup %notin% c("ELO", "EO"),
           year == yrs,
           #            charter_flag == "Y"
           rtype == "S"
           #           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    mutate( combo.name = paste0(districtname," - ",schoolname),
            bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    mutate(ind = factor(ind, levels = ind_ord),
           studentgroup = factor(studentgroup, levels = sg_ord))
  
  
  ggplot(graphthis, aes(x = ind, y = studentgroup)) + 
    geom_tile(aes(fill = color.factor)) +
    geom_text(aes(label = round(change, 1))) +
    #  lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    scale_x_discrete(position = "top") +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Student Groups with Change Amounts \nfor ", sch)
         # caption = capt
    )
  
  ggsave(here("figs", "SUstudentgroupchange", paste0(sch," Student Group Change.png")), width = 5, height = 5)
  
}