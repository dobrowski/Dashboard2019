

# https://www.cde.ca.gov/ta/ac/cm/

### Load libraries ----

library(ggthemes)

# ### Define default variables -----

 yr <- "2019"
 
 county <- "Monterey"

pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue",
         "Graduation Rate Below 67%" = alpha("light grey", alpha = 0.3),
         "Lowest 5%" = alpha( "white", alpha = 0.3))


all.mc.schools <- all %>% 
    filter(color >= 1,
           year == yr) %>%
    filter( str_detect( countyname, county) ) %>% 
    filter(studentgroup == "ALL") %>%
    filter(rtype == "S") %>%
    select(districtname, schoolname, ind, studentgroup, color, color.factor, currstatus, priorstatus) %>% 
    group_by(districtname, schoolname) %>% 
    arrange(districtname, schoolname)




CSI.list2 <- all.mc.schools %>% 
    group_by(districtname, schoolname) %>% 
    arrange(districtname, schoolname) %>% 
    mutate(reds = sum(color.factor == "1"),
           oranges = sum(color.factor == "2"),
           total = max( row_number() ) ,
           CSI.all.reds = if_else(reds == total, 1, 0),
           CSI.all.red.but.one = if_else(total >=2 & total - reds == 1, 1, 0),
           CSI.all.red.or.orange = if_else( reds >=1  & reds + oranges == total, 1, 0),
           CSI.5.more.majority = if_else( (total >= 5) & ((reds/total) >.5) , 1, 0 ) ,
           two.year = (currstatus + priorstatus)/2 ,
           CSI.grad = if_else( ind == "grad" & two.year < 68, 1, 0    ),
           CSI.grad = sum(CSI.grad)) %>% 
    mutate(CSI = if_else( sum(CSI.all.reds, CSI.all.red.but.one, CSI.all.red.or.orange, CSI.5.more.majority, CSI.grad) > 0, "Y", "N") ) %>% 
    select(districtname, schoolname, reds, oranges, total, starts_with("CSI")) %>%
    distinct() %>%
    filter(CSI == "Y") %>%
    arrange(desc(CSI), districtname, schoolname) # %>%
#    filter(!str_detect(schoolname, "Special"))





# write_csv(CSI.list2, here("data", "CSIlist.csv" ) )



CSI.list3 <- CSI.list2 %>%
    select(districtname, schoolname, CSI.grad) %>%
    left_join(all.mc.schools) %>%
    mutate(Reason = if_else(CSI.grad >= 1, "Graduation Rate Below 68%", "Lowest 5%"),
           School = str_c(schoolname, districtname, sep = ",\n ") ,
           ind = recode(ind, "chronic" = "abs")
    )



ggplot(CSI.list3, aes(ind   , fct_rev(School)     ,  fill = color.factor )) + 
    geom_rect(data = CSI.list3, aes(fill =  Reason , alpha = 0.3) ,xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf) +
    geom_tile(colour = "white") +
    geom_text(aes(label=ind)) +
    facet_wrap( fct_rev(Reason) ~.) +
    scale_fill_manual(values = pal) +
    theme_hc() +
    theme(
        legend.position = "none",
        # axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = 'white'),
        # panel.grid.major.y =   element_line(colour = "black",size=0.75),
        # panel.ontop = TRUE
        # panel.background = element_rect(fill = "lightblue")
    ) +
    labs(x="Dashboard Indicators",
         y="",
         title = "Dashboard Indicators for CSI Schools", 
         subtitle="", 
         fill="")


ggsave( here("figs", "CSI Dashboard Indicators with labels2.png"), width = 8.5, height = 6)
