


### Load Libaries --------

library(tidyverse)
library(here)


### Define default variables --------

# files <- c("cci", "chronic", "ela", "elaprate", "elpi", "grad", "math", "mathprate", "susp")

files <- c( "chronic", "cci", "ela",  "grad", "math", "susp", "elpi")

years <- c("2018","2019")

county <- "Monterey"


pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")


ind_ord <- c("ela","math","cci","chronic","grad","susp", "elpi")
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


###  Compile all the forms from the state into a single dataframe

compile <- function(ind, yr) {
    read.delim(here("data",yr, paste0(ind ,"download",yr ,".txt"))) %>%
        #                 filter(countyname == county) %>%
        mutate(ind = ind,
               year = yr)
}

all <- data_frame()

for(y in years){
for(i in files){
    
    print(y)
    
    print(i)
    
    temp <- compile(i, y)
    
    all <- bind_rows(all, temp)
    
 #   assign(i, temp)
}
}



all <- all %>%
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
    )




year <- 2019
ind <- "chronic"

chronic <- read.delim(here("data",year, paste0( ind ,"download2019.txt")))


####  Exploration --------

chronic.mry <- chronic %>%
    filter(str_detect(countyname, "Monterey"), 
         #  color == "1",
          ( rtype == "D"|  charter_flag == "Y")
           )

write.csv(chronic.mry, here("output","Chronic Monterey Districts in Red.csv"))


mry.DA <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           year == "2019",
            color == "1",
           ( rtype == "D"|  charter_flag == "Y")
    ) %>%
    select(cds, schoolname, districtname, charter_flag, studentgroup, color, year) %>%
    group_by(cds, studentgroup) %>%
    mutate(DA.count = sum(color))


#### Graphing -------

graphthis <- all %>%
    filter(str_detect(countyname, "Monterey"), 
           year == "2019",
           ( rtype == "D"|  charter_flag == "Y")
           ) %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname))


library(lemon)

ggplot(graphthis, aes(ind, y = fct_rev(studentgroup))) + 
    geom_tile(aes(fill = color.factor)) +
    facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) +
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = "Differentiated Assistance",
         caption = capt)


### End -----

