


state.DA <- all %>%
    filter(# str_detect(countyname, "Monterey"), # Change County Name as Applicable
           year == yrs,
           #          color == "1",
           rtype == "D" 
           #        ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    select(cds, schoolname, districtname, charter_flag, studentgroup, ind, color, year) %>%
    group_by(cds, studentgroup) %>%
    mutate(Priority5 = if_else( (ind == "chronic" & color == 1)|(ind == "grad" & color == 1), TRUE, FALSE ),
           Priority6 = if_else( (ind == "susp" & color == 1), TRUE, FALSE ),
           Priority8 = if_else( (ind == "cci" & color == 1), TRUE, FALSE ),
           Prior4math = case_when( ind == "math" & color == 1  ~ 1, 
                                   ind == "math" & color == 2  ~ .5, 
                                   TRUE ~ 0),
           Prior4ela = case_when( ind == "ela" & color == 1  ~ 1, 
                                  ind == "ela" & color == 2  ~ .5, 
                                  TRUE ~ 0),
           Prior4elpi = case_when( ind == "elpi" & color == 1  ~ 1.5, # Will need to see if this is coded as color or only "Very low" 
                                   TRUE ~ 0),
    ) %>%
    mutate(Priority4.count = sum(Prior4ela + Prior4math + Prior4elpi), # Getting right combinations for Priority 4
           Priority4 = if_else(Priority4.count > 1, TRUE, FALSE )) %>%
    mutate(Priority5 = as.logical(sum(Priority5)),
           Priority6 = as.logical(sum(Priority6)),
           Priority8 = as.logical(sum(Priority8))
    ) %>%
    select(cds, districtname ,studentgroup, Priority4 , Priority5 , Priority6 , Priority8) %>%
    distinct() %>%  # simplifying
    mutate(DA.count =  Priority4 + Priority5 + Priority6 + Priority8 ) %>%
    filter(DA.count >= 2) %>%  # only those eligible for DA
    arrange(districtname, studentgroup)

state.DA2019 <- state.DA %>%
    filter(studentgroup != "ALL") %>%
    group_by(districtname) %>%
    count() 
    


yrs <- 2018


state.DA <- all %>%
    filter(# str_detect(countyname, "Monterey"), # Change County Name as Applicable
      #  year == yrs,
        #          color == "1",
        rtype == "D" 
        #        ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    select(cds, schoolname, districtname, charter_flag, studentgroup, ind, color, year) %>%
    group_by(cds, studentgroup, year) %>%
    mutate(Priority5 = if_else( (ind == "chronic" & color == 1)|(ind == "grad" & color == 1), TRUE, FALSE ),
           Priority6 = if_else( (ind == "susp" & color == 1), TRUE, FALSE ),
           Priority8b = if_else( (ind == "cci" & color == 1), TRUE, FALSE ),
           Prior4math = case_when( ind == "math" & color == 1  ~ 1, 
                                   ind == "math" & color == 2  ~ .5, 
                                   TRUE ~ 0),
           Prior4ela = case_when( ind == "ela" & color == 1  ~ 1, 
                                  ind == "ela" & color == 2  ~ .5, 
                                  TRUE ~ 0),
           Prior4elpi = case_when( ind == "elpi" & color == 1  ~ 1.5, # Will need to see if this is coded as color or only "Very low" 
                                   TRUE ~ 0),
    ) %>%
    mutate(Priority8 = replace_na(Priority8b,FALSE)) %>%
    mutate(Priority4.count = sum(Prior4ela + Prior4math + Prior4elpi), # Getting right combinations for Priority 4
           Priority4 = if_else(Priority4.count > 1, TRUE, FALSE )) %>%
    mutate(Priority5 = as.logical(sum(Priority5)),
           Priority6 = as.logical(sum(Priority6)),
           Priority8 = as.logical(sum(Priority8))
    ) %>%
    select(cds, districtname ,studentgroup, year, Priority4 , Priority5 , Priority6 , Priority8) %>%
    distinct() %>%  # simplifying
    mutate(DA.count =  Priority4 + Priority5 + Priority6 + Priority8 ) %>%
    filter(DA.count >= 2) %>%  # only those eligible for DA
    arrange(districtname, studentgroup, year)



state.DA2 <- state.DA %>% 
    filter(studentgroup != "ALL") %>%
    group_by(districtname, studentgroup ) %>%
    count()%>%
    filter(n >= 3)


state.DA3 <- state.DA2 %>%
    filter(n >= 3) %>%
    group_by(districtname) %>%
    count() %>%
    filter(n >= 3)
    


