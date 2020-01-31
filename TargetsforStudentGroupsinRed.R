
library(googlesheets4)


dist <- "South Monterey"

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
           increase) %>%
    mutate(target = case_when(ind == "chronic" ~  floor(currnumer + if_else(cutoffdiff > increase, cutoffdiff , increase )/100 * currdenom),
                              ind == "cci" ~  ceiling(curr_prep + if_else(cutoffdiff < increase, cutoffdiff , increase )/100 * currdenom),
                              ind == "grad" ~  ceiling(currnumer + cutoffdiff/100 * currdenom),
                              ind == "susp" ~  floor(currnumer + if_else(cutoffdiff > increase, cutoffdiff , increase )/100 * currdenom),
                              ind %in% c("ela","math")  ~  currstatus + increase,
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
