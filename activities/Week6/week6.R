## (Title) R Script for Stats 506, F20 Week 6 Activity
##
## Author(s): Tianshi Wang, wangts@umich.edu
## Updated: October 13, 2020 - Last modified date
#! Update the date every time you work on a script. 

#! Limit lines to 79 characters with rare exceptions. 
# 79: -------------------------------------------------------------------------

#! Use the following structure to label distinct code chunks that accomplish
#! a single or related set of tasks. 

#! Load libraries at the top of your script.
# libraries: ------------------------------------------------------------------
library(tidyverse)

#! store directories you read from or write to as objects here.
# directories: ----------------------------------------------------------------
path = "C:/Users/lenovo/Desktop/Week6"

#! you should generally read/load data in a single place near the start.
# data: -----------------------------------------------------------------------
ohxden_path = sprintf("%s/nhanes_ohxden.csv", path)
ohxden = read_delim(ohxden_path ,delim = ",") 

demo_path = sprintf("%s/nhanes_demo.csv", path)
demo = read_delim(demo_path ,delim = ",")

## Part 1
#(2)
merged = ohxden %>% select(SEQN, OHDDESTS) %>%
  left_join(demo, by = "SEQN")

View(merged)

#(3)
clean = merged %>% mutate(id = SEQN, gender = RIAGENDR, age = RIDAGEYR,
                          under_20 = as.integer(RIDAGEYR<20),
                          college = ifelse(under_20 == 1, "No college",
                                           ifelse(DMDEDUC2>=4, "College", "No college")),
                          exam_status = RIDSTATR,
                          ohx_status = OHDDESTS
)%>% select(id, age, gender, under_20, college, exam_status, 
            ohx_status)

#(4)
clean2 = clean %>% mutate(
  ohx = ifelse(is.na(ohx_status),ifelse((exam_status == 1 & ohx_status == 2),
               "complete", "missing"), "missing")
  )

#(5)
filtered = clean2 %>% filter(exam_status == 2)


## Part 2
#(1)
table1 = filtered %>% group_by(ohx, under_20) %>%
  summarise(n = n())




#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------
