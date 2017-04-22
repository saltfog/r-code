#Result of inferential test Gender Male Female Native language Greek Anxiety 

Survey<-read.csv("SurveySP16.csv",na.strings="") 
#to create a neew variable for "Misconduct in writing" 
Survey$misconduct_writing<-factor(ifelse(Survey$Writing_A=="0 times" & 
Survey$Writing_B=="0 times" & Survey$Writing_C=="0 times" & 
Survey$Writing_D=="0 times" & Survey$Writing_E=="0 times" & 
Survey$Writing_F=="0 times"& Survey$Writing_G=="0 times", "no", "yes")) 
#to omit all the N/As in the data 
Survey=na.omit(Survey) 
#to see the table for the new vriable misconduct_writing to get the "n" values for the chart 
table(Survey$misconduct_writing) 
#to factor any GPAs that are greater than 4 as NA since it is on a 4.0 scale 
Survey$GPA[Survey$GPA>4]<-NA 
#to omit any NAs 
Survey=na.omit(Survey) 
#to see the summary of GPA
library(mosaic)
favstats(Survey$GPA) 
#to see the summary for GPA and misconduct 
favstats(Survey$GPA~Survey$misconduct_writing) 
#to conduct a t test for GPA 
t.test(Survey$GPA~Survey$misconduct_writing,var.equal=FALSE) 
#to see the mean for num_credithrs and confirm that there are 0 "n missing" 
favstats(Survey$num_credithrs) 
#to see the summary for num_credithrs and misconduct 
favstats(Survey$num_credithrs~Survey$misconduct_writing) 
#to conduct a t test for num_credithrs 
t.test(Survey$num_credithrs~Survey$misconduct_writing,var.equal=FALSE) 
#to take out any numbers that are not between 1 and 10 for anxiety 
Survey$anxiety[Survey$anxiety>10]<-NA 
Survey$anxiety[Survey$anxiety<1]<-NA 
#to omit any NAs 
Survey=na.omit(Survey) 
#to see the summary of anxiety 
favstats(Survey$anxiety) 
#to see the summary for anxiety and misconduct 
favstats(Survey$anxiety~Survey$misconduct_writing) 
#to conduct a t test for anxiety 
t.test(Survey$anxiety~Survey$misconduct_writing,var.equal=FALSE) 
#to create a table for Gender 
table(Survey$gender) 
GenderTable<-table(Survey$gender) 
#to create a proportion table of gender 
prop.table(GenderTable) 
#to create a proportion table for gender and misconduct 
table(Survey$gender,Survey$misconduct_writing) 
Gender_MisconductTable<-table(Survey$gender,Survey$misconduct_writing) 
prop.table(Gender_MisconductTable, margin=2) 
#to conduct a chisquare for gender 
chisq.test(Survey$gender,Survey$misconduct_writing,correct = F) 
#to create a table for Native language 
summary(Survey$English) 
table(Survey$English) 
EnglishTable<-table(Survey$English) 
#to create a proportion table of native language 
prop.table(EnglishTable) 
#to create a proportion table for english and misconduct 
table(Survey$English,Survey$misconduct_writing) 
English_MisconductTable<-table(Survey$English,Survey$misconduct_writing) 
prop.table(English_MisconductTable, margin=2) 
#to conduct a chisquare for English 
chisq.test(Survey$English,Survey$misconduct_writing,correct = F)
#to create a table for Greek table(Survey$Greek) 
GreekTable<-table(Survey$Greek) 
#to create a proportion table of Greek 
prop.table(GreekTable) 
#to create a proportion table for Greek and misconduct 
table(Survey$Greek,Survey$misconduct_writing) 
Greek_MisconductTable<-table(Survey$Greek,Survey$misconduct_writing) 
prop.table(Greek_MisconductTable,margin = 2) 
#to conduct a chi-squared for gender 
