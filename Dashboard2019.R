
#  at https://github.com/dobrowski/Dashboard2019

### Load Libaries --------

library(tidyverse)
library(here)
library(lemon)  # used for the plotting so that the x-axis repeats each small multiple


`%notin%` <- Negate(`%in%`)


### Define default variables --------



# files <- c("cci", "chronic", "ela", "elaprate", "elpi", "grad", "math", "mathprate", "susp")

files <- c( "chronic", "cci", "ela",  "grad", "math", "susp", "elpi")

years <- c("2018","2019", "2017")

county <- "Monterey"


pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")


ind_ord <- c("ela","math", "elpi","cci","chronic","grad","susp")
sg_ord <- c("ALL", "AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR", "EL", "ELO", "EO", "SED", "SWD", "FOS", "HOM")

# Caption for later that explains the student group codes

capt <- "ALL = All Students,
AA = Black/African American,
AI = American Indian or Alaska Native,
AS = Asian,
FI = Filipino,
HI = Hispanic,
PI = Pacific Islander,
WH = White,
MR = Multiple Races/Two or More,
EL = English Learner,
ELO = English Learners Only, 
RFP = RFEPs Only,
EO = English Only,
SED = Socioeconomically Disadvantaged,
SWD = Students with Disabilities,
FOS = Foster Youth,
HOM = Homeless Youth"

###  Load files ----------

# Put the data files in the 'data' folder, and then by year in subfolders


###  Compile all the forms from the state into a single dataframe

compile <- function(ind, yr) {
    read.delim(here("data",yr, paste0(ind ,"download",yr ,".txt"))) %>%
  #  select(-ends_with("ear")  )  #  fix this.   %>%
  #                 filter(countyname == county) %>%
  mutate(ind = ind,
         year = yr,
         reportingyear = "0")
}

all <- data_frame()

for(y in years){
for(i in files){
    
    print(y)
    
    print(i)
    
    try({
    temp <- compile(i, y)
    
    all <- bind_rows(all, temp)
    })
 #   assign(i, temp)
}
}


all <- all %>%
  mutate(color = if_else(as.character( ind) == "elpi", statuslevel, color),
    #     color = if_else(is.na(color), 0, color),
         studentgroup = if_else(as.character( ind) == "elpi", "EL", as.character(studentgroup))) %>%
mutate(ind = factor(ind, levels = ind_ord),
       studentgroup = factor(studentgroup, levels = sg_ord),
       color.factor = factor(color)
       ) %>%
    mutate(districtname = str_replace_all(districtname, "County Office of Education", "COE"),
           districtname = str_replace_all(districtname, "County Department of Education", "CDE"),
           districtname = str_replace_all(districtname, "County Superintendent of Schools", "CSOS"),
           districtname = str_replace_all(districtname, "Union", "U"),
           districtname = str_replace_all(districtname, "Unified", "U"),
           districtname = str_replace_all(districtname, "Elementary", "E"),
           districtname = str_replace_all(districtname, "County", "C")
    ) %>%
  filter(!is.na(studentgroup))





schools <-read.delim(here("data",  "pubschls.txt"))
narrow.schools <- schools %>% select(CDSCode,DOC:GSserved) %>%
  mutate(cds = str_pad( as.character(CDSCode), width = 14, side = "left", pad = "0"  ) )



all <- all %>%
  mutate(cds = str_pad( as.character(cds), width = 14, side = "left", pad = "0"  ) ) %>%
  # mutate(ind = factor(ind, levels = ind_ord),
  #        studentgroup = factor(studentgroup, levels = sg_ord),
  #        color.factor = factor(color)
  # ) %>%
  # mutate(districtname = str_replace_all(districtname, "County Office of Education", "COE"),
  #        districtname = str_replace_all(districtname, "County Department of Education", "CDE"),
  #        districtname = str_replace_all(districtname, "County Superintendent of Schools", "CSOS"),
  #        districtname = str_replace_all(districtname, "Union", "U"),
  #        districtname = str_replace_all(districtname, "Unified", "U"),
  #        districtname = str_replace_all(districtname, "Elementary", "E"),
  #        districtname = str_replace_all(districtname, "County", "C")
  # ) %>%
  left_join(narrow.schools ) %>%
  mutate(type = as.character(type)) %>%
  mutate(type = if_else(!is.na(type), type, case_when(EILCode == "ELEM" ~ "ES",
                                                      EILCode == "HS" ~ "HS" ,
                                                      EILCode == "INTMIDJR" ~ "MS", 
                                                      EILCode == "No Data" & str_detect(DOCType, "Elementary") ~ "ED",
                                                      EILCode == "No Data" & str_detect(DOCType, "High") ~ "HD", 
                                                      EILCode == "No Data" & str_detect(DOCType, "Unified") ~ "UD")   )) %>%
  mutate(cutoff = case_when(ind == "chronic" & statuslevel == 4 ~ 2.5, 
                            ind == "chronic" & statuslevel == 3 ~ 5.0,
                            ind == "chronic" & statuslevel == 2 ~ 10.0,
                            ind == "chronic" & statuslevel == 1 ~ 20.0,
                            
                            ind == "cci" & statuslevel == 1 ~ 10.0,
                            ind == "cci" & statuslevel == 2 ~ 35.0,
                            ind == "cci" & statuslevel == 3 ~ 55.0,
                            ind == "cci" & statuslevel == 4 ~ 70.0,
                            
                            ind == "grad" & statuslevel == 1 ~ 68.0,
                            ind == "grad" & statuslevel == 2 ~ 80,
                            ind == "grad" & statuslevel == 3 ~ 90.5,
                            ind == "grad" & statuslevel == 4 ~ 95,                                  
                            
                            ind == "ela" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -70,
                            ind == "ela" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -5,
                            ind == "ela" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 10,
                            ind == "ela" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 45,                                  
                            
                            ind == "ela" & statuslevel == 1 & type %in% c("HS", "HD") ~ -45,
                            ind == "ela" & statuslevel == 2 & type %in% c("HS", "HD") ~ 0,
                            ind == "ela" & statuslevel == 3 & type %in% c("HS", "HD") ~ 30,
                            ind == "ela" & statuslevel == 4 & type %in% c("HS", "HD") ~ 75,                                  
                            
                            ind == "math" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -95,
                            ind == "math" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -25,
                            ind == "math" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 0,
                            ind == "math" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 35,                                  
                            
                            ind == "math" & statuslevel == 1 & type %in% c("HS", "HD") ~ -115,
                            ind == "math" & statuslevel == 2 & type %in% c("HS", "HD") ~ -60,
                            ind == "math" & statuslevel == 3 & type %in% c("HS", "HD") ~ 0,
                            ind == "math" & statuslevel == 4 & type %in% c("HS", "HD") ~ 25,   
                            
                            ind == "susp" & statuslevel == 4 & type == "ED" ~ 0.5,
                            ind == "susp" & statuslevel == 3 & type == "ED" ~ 1.5,
                            ind == "susp" & statuslevel == 2 & type == "ED" ~ 3.0,
                            ind == "susp" & statuslevel == 1 & type == "ED" ~ 6.0,
                            
                            ind == "susp" & statuslevel == 4 & type == "HD" ~ 1.5,
                            ind == "susp" & statuslevel == 3 & type == "HD" ~ 3.5,
                            ind == "susp" & statuslevel == 2 & type == "HD" ~ 6.0,
                            ind == "susp" & statuslevel == 1 & type == "HD" ~ 9.0,
                            
                            ind == "susp" & statuslevel == 4 & type == "UD" ~ 1.0,
                            ind == "susp" & statuslevel == 3 & type == "UD" ~ 2.5,
                            ind == "susp" & statuslevel == 2 & type == "UD" ~ 4.5,
                            ind == "susp" & statuslevel == 1 & type == "UD" ~ 8.0,
                            
                            
                            ind == "susp" & statuslevel == 4 & type == "ES" ~ 0.5,
                            ind == "susp" & statuslevel == 3 & type == "ES" ~ 1.0,
                            ind == "susp" & statuslevel == 2 & type == "ES" ~ 3.0,
                            ind == "susp" & statuslevel == 1 & type == "ES" ~ 6.0,
                            
                            ind == "susp" & statuslevel == 4 & type == "MS" ~ 0.5,
                            ind == "susp" & statuslevel == 3 & type == "MS" ~ 2.0,
                            ind == "susp" & statuslevel == 2 & type == "MS" ~ 8.0,
                            ind == "susp" & statuslevel == 1 & type == "MS" ~ 12.0,
                            
                            ind == "susp" & statuslevel == 4 & type == "HS" ~ 0.5,
                            ind == "susp" & statuslevel == 3 & type == "HS" ~ 1.5,
                            ind == "susp" & statuslevel == 2 & type == "HS" ~ 6.0,
                            ind == "susp" & statuslevel == 1 & type == "HS" ~ 10.0,
                            TRUE ~ NA_real_),
         cutoffdiff = cutoff - currstatus,
         increase = case_when(ind == "chronic" ~ -0.5,
                              ind == "cci"  ~ 2.0,
                              ind == "grad" & dass_flag != "Y" ~ 1.0 ,
                              ind == "grad" & dass_flag == "Y" ~ 3.0 ,
                              
                              ind == "ela" & type %in% c("ES", "ED", "MS", "UD") ~ 3,
                              ind == "ela" & type %in% c("HS", "HD") ~ 3,

                              ind == "math" & type %in% c("ES", "ED", "MS", "UD") ~ 3,
                              ind == "math" & type %in% c("HS", "HD") ~ 3,
                              
                              ind == "susp" & type == "ED" ~ -0.3,
                              ind == "susp" & type == "HD" ~ -0.5,
                              ind == "susp" & type == "UD" ~ -0.3,
                              ind == "susp" & type %in% c("ES", "MS", "HS") ~ -0.3,
                              TRUE ~ NA_real_)
                
  ) 






####  Exploration --------


yrs <-2019

mry.DA <- all %>%
    filter(str_detect(countyname, "Monterey"), # Change County Name as Applicable
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


write_csv(mry.DA, here("output", "Districts in DA and why.csv"))

#### Graphing -------



graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           year == yrs,
#            charter_flag == "Y"
          rtype == "D"
#           ( rtype == "D"|  charter_flag == "Y")
           ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname))


ggplot(graphthis, aes(ind, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Differentiated Assistance for ", yrs),
         caption = capt)




ggsave(here("figs","DA graph for 2019.png"), width = 16, height = 16)



ggsave(here("figs","DA graph for Charter 2018.png"), width = 16, height = 16)


ggsave(here("figs","DA_Charter.png"), width = 16, height = 16)

### End -----

