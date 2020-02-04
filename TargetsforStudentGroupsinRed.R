
library(googlesheets4)


dist <- "South Monterey"




DAlist <- c("Soledad", "South Monterey", "Salinas U", "Monterey Peninsula", "Santa Rita")



for (dist in DAlist) {
    

SoMoCo <- all %>% 
    filter(str_detect(districtname, dist),
           rtype == "D",
           year == "2019",
           color == 1) %>%
    select(cds, 
           rtype, 
           districtname, 
           schoolname, 
           charter_flag, 
           dass_flag, 
           studentgroup, 
           ind , 
           color, 
           color.factor, 
           currnumer, 
           curr_prep,
           currdenom, 
           currstatus, 
           change, 
           statuslevel, 
           changelevel  ,  
           cutoff, 
           cutoffdiff, 
           improve) %>%
    mutate(target = case_when(ind == "chronic" ~  floor(currnumer + if_else(cutoffdiff > improve, cutoffdiff , improve )/100 * currdenom),
                              ind == "cci" ~  ceiling(curr_prep + if_else(cutoffdiff < improve, cutoffdiff , improve )/100 * currdenom),
                              ind == "grad" ~  ceiling(currnumer + cutoffdiff/100 * currdenom),
                              ind == "susp" ~  floor(currnumer + if_else(cutoffdiff > improve, cutoffdiff , improve )/100 * currdenom),
                              ind %in% c("ela","math")  ~  currstatus + improve,
                              TRUE ~ NA_real_ ),
           target.type = case_when(ind == "chronic" ~  "students",
                              ind == "cci" ~  "students",
                              ind == "grad" ~  "students",
                              ind == "susp" ~  "students",
                              ind %in% c("ela","math")  ~  "DFS",
                              TRUE ~ NA_character_),
           )


# ss <- sheets_create("DA Targets", sheets = SoMoCo)

sheets_write(data = SoMoCo, ss = ss, sheet = dist)

}



###  All Indicators for the Student Groups that are at risk for three years eligibility (SoMoCo)



SoMoCo <- all %>% 
    filter(str_detect(districtname, dist),
           rtype == "D",
           year == "2019",
       #    color == 1
       studentgroup %in% c("HOM", "EL", "SWD")
           ) %>%
    select(cds, 
           rtype, 
           districtname, 
           schoolname, 
           charter_flag, 
           dass_flag, 
           studentgroup, 
           ind , 
           color, 
           color.factor, 
           currnumer, 
           curr_prep,
           currdenom, 
           currstatus, 
           change, 
           statuslevel, 
           changelevel  ,  
           cutoff, 
           cutoffdiff, 
           improve,
           improve.sig,
           dropoff,
           dropoffdiff,
           worsen) %>%
    mutate(target = case_when(statuslevel == 1 & ind == "chronic" ~  floor(currnumer + if_else(cutoffdiff > improve, cutoffdiff , improve )/100 * currdenom),
                              statuslevel == 1 & ind == "cci" ~  ceiling(curr_prep + if_else(cutoffdiff < improve, cutoffdiff , improve )/100 * currdenom),
                              statuslevel == 1 & ind == "grad" ~  ceiling(currnumer + cutoffdiff/100 * currdenom),
                              statuslevel == 1 & ind == "susp" ~  floor(currnumer + if_else(cutoffdiff > improve, cutoffdiff , improve )/100 * currdenom),
                              statuslevel == 1 & ind %in% c("ela","math")  ~  currstatus + improve,
                              
                              statuslevel == 2 & currdenom < 150 & ind == "cci" ~  ceiling(curr_prep + dropoffdiff /100 * currdenom),
                              statuslevel == 2 & currdenom < 150 & ind == "grad" ~  ceiling(currnumer + dropoffdiff/100 * currdenom),
                              
                              TRUE ~ NA_real_ ),
           target.type = case_when(ind == "chronic" ~  "students",
                                   ind == "cci" ~  "students",
                                   ind == "grad" ~  "students",
                                   ind == "susp" ~  "students",
                                   ind %in% c("ela","math")  ~  "DFS",
                                   TRUE ~ NA_character_),
    )

sheets_write(data = SoMoCo, ss = ss, sheet = dist)
