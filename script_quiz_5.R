library(tidyverse)
library(apaTables)
library(haven)
library(predictionInterval)


#load file
my.data <- read_csv("reg_quiz2_data.csv")
#glimpse(my.data)


#Q1 - Correlation table
apa.cor.table(my.data,filename="Table 1.doc",table.number=1)


#make sure data isn't curvilinear, so regression makes sense
psych::pairs.panels(as.data.frame(my.data))
#lines look pretty straight, procees

#Q2a - selfEsteem on aSuc beyond PAS
block1 = lm(aSuc ~ PAS,data=my.data)
block2 = lm(aSuc ~ PAS + selfEsteem,data=my.data)
apa.reg.table(block1, block2, filename = "Table 2.doc",table.number=2)



#Q2b - selfEsteem on aSuc beyond NAS
block1 = lm(aSuc~NAS,data=my.data)
block3 = lm(aSuc ~ NAS + selfEsteem ,data=my.data)
apa.reg.table(block1, block3, filename = "Table 3.doc",table.number=3)



#Q2c - block reg - selfEsteem on aSuc beyond NAS + PAS
block4 = lm(aSuc ~ NAS + PAS,data=my.data)
block5 = lm(aSuc ~ NAS + PAS + selfEsteem,data=my.data)
apa.reg.table(block4, block5, filename = "Table 4.doc",table.number=4)
