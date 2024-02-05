install.packages("tidyverse")
setwd("C:/Users/CHARLEY/Desktop/Group project")
library(haven)
data_shit = read_dta("Sexratio_crime.dta")
data_CHNS2 = read_dta("CHNS_1.dta")
data_UHS = read_dta("UHS.dta")

reg = lm(crime~provmf1625,data = data_shit)
reg1 = lm(crime~provmf2645,data = data_shit)
#non linear regression
data_shit$provmf2645_2 = data_shit$provmf2645^2
reg1_poly2 = lm(crime~provmf2645 + provmf2645_2, data = data_shit)
summ(reg1_poly2, robust = TRUE, digit = 4)
data_shit$provmf2645_3 = data_shit$provmf2645^3
reg1_poly3 = lm(crime~provmf2645 + provmf2645_2 + provmf2645_3, data = data_shit)
reg1_log = lm(data = data_shit,crime~lprovmf2645)
summ(reg1_poly3, robust = TRUE, digit = 4)
export_summs(reg1, reg1_poly2, reg1_poly3, reg1_log ,  robust=TRUE,digits=3,to.file = "xlsx",file.name="nonlinear_26to45.xlsx")

#F test
library(car)
linearHypothesis(reg1_poly3, c('provmf2645_2 = 0', 'provmf2645_3 = 0' ), white.adjust = 'hc1')
#we reject the H0
#must be non linear 
reg2 = lm(crime~provmf4665,data = data_shit)
reg3 = lm(crime~provmf1015,data = data_shit)
summary(reg)
install.packages("jtools")
library("jtools")
install.packages("huxtable")
install.packages("officers")#(different formate document)
install.packages("flextable")
export_summs(reg,reg1,reg2,reg3,robust=TRUE,digits=3,to.file = "xlsx",file.name="age group.xlsx")
library(ggplot2)
ggplot(data_shit, aes(x =  provmf2645, 
                     y = crime)) + geom_point() + geom_smooth()
boxplot(data_shit$crime)
boxplot(data$provmf2645)

reg_fuck = lm(inequality~provmf2645+  eximfdi_gdp + urbanization + immigration,data = data_shit)
summ(reg_fuck, robust = TRUE,  confint = TRUE,digits = 3)

data_shit.cor = cor(data_shit)
data_shit.cor
write.table (data_shit.cor, file ='data_shit.cor.csv', sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

#thus, we mainly focus on the sex ratio is age group 26 to 45
reg4 = lm(crime~provmf2645+inequality, data = data_shit)
summ(reg4, robust = TRUE,  confint = TRUE,digits = 3)
reg5 = lm(crime~provmf2645 +  inequality + eximfdi_gdp, data = data_shit)
summ(reg5, robust = TRUE,  confint = TRUE,digits = 3)
reg6 = lm(crime~provmf2645  + eximfdi_gdp + urbanization, data = data_shit)
summ(reg6, robust = TRUE,  confint = TRUE,digits = 3)
reg7 = lm(crime~provmf2645 +  inequality + eximfdi_gdp + urbanization +immigration, data = data_shit)
summ(reg7, robust = TRUE,  confint = TRUE,digits = 3)
export_summs(reg1,reg4,reg5,reg6,reg7,robust=TRUE,digits=3,to.file = "xlsx",file.name="damn.xlsx")
#we try log(inequality)
reg8 = lm(crime~provmf2645+log(inequality), data = data_shit)
summ(reg4, robust = TRUE,  confint = TRUE,digits = 3)
reg5 = lm(crime~provmf2645 +log(inequality) + eximfdi_gdp, data = data_shit)
summ(reg5, robust = TRUE,  confint = TRUE,digits = 3)
reg6 = lm(crime~provmf2645  + eximfdi_gdp + urbanization, data = data_shit)
summ(reg6, robust = TRUE,  confint = TRUE,digits = 3)
reg7 = lm(crime~provmf2645 +  inequality + eximfdi_gdp + urbanization +immigration, data = data_shit)
summ(reg7, robust = TRUE,  confint = TRUE,digits = 3)
export_summs(reg1,reg4,reg5,reg6,reg7,robust=TRUE,digits=3,to.file = "xlsx",file.name="damn.xlsx")

data_shit$construction_100 = data_shit$construction/100
reg9 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization + immigration + welfare + police + big4 + construction_100, data = data_shit)
export_summs(reg9, robust = TRUE, digit = 3)
reg_clear = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration, data = data_shit)
export_summs(reg9, reg_clear,robust = TRUE, digit = 4)
reg10 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration + lwelfare + lpolice + big4 + construction, data = data_shit)
export_summs(reg10, robust = TRUE, digit = 4)
#pick up variables
data_shit$construction
reg_damn1 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration + welfare,data = data_shit)
reg_damn2 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration +  police, data =data_shit)
reg_damn3 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration +  big4, data  = data_shit)
reg_damn4 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration +  construction_100, data  = data_shit)
reg_damn5 = lm(crime~provmf2645 + inequality + eximfdi_gdp + urbanization +immigration + police + construction_100, data = data_shit)
export_summs(reg9, reg_clear, reg_damn1,reg_damn2,reg_damn3,reg_damn4,robust = TRUE, digits = 4, to.file = "xlsx",file.name="variable choice.xlsx")
export_summs(reg9, reg_clear, reg_damn1,reg_damn2,reg_damn3,reg_damn4, reg_damn5,robust = TRUE, digits = 4, to.file = "xlsx",file.name="variable choice2.xlsx")

# Though intuitively they are relevant to the crime rate, but they are not statistically significant


#interactions between two variables 
reg11 = lm(crime~ (provmf2645*inequality), data = data_shit)
export_summs(reg11, robust = TRUE,  confint = TRUE,digits = 3)

#imperfect multicollinearity, F test
linearHypothesis(reg11, c('inequality = 0', 'provmf2645:inequality= 0' ), white.adjust = 'hc1')
#we reject the H0, inequality does matter 
reg12 = lm(crime~ (provmf2645*inequality)+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)
export_summs(reg11,reg12, robust = TRUE,  confint = TRUE,digits = 3, to.file = "xlsx",file.name="hello1.xlsx")
# P40 TO 41
#we should try nonlinear model 
#1 inter
reg13 = lm(crime~provmf2645 + provmf2645_2  +  inequality + (provmf2645*inequality)+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)
#2 inter
reg14 = lm(crime~provmf2645+ provmf2645_2 +  inequality + (provmf2645*inequality) + (provmf2645_2*inequality)+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)
#3 inter
reg15 = lm(crime~provmf2645+ provmf2645_2 + provmf2645_3 +inequality + (provmf2645*inequality) + (provmf2645_2*inequality) +(provmf2645_3*inequality)+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)

reg16 = lm(crime~provmf2645 + provmf2645_2  +  inequality+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)

reg17 = lm(crime~provmf2645 + provmf2645_2 + provmf2645_3 +  inequality+ eximfdi_gdp + urbanization +immigration+police+construction_100, data = data_shit)
export_summs(reg13, reg14, reg15,reg16, reg17, robust = TRUE,  confint = TRUE,digits = 3,to.file = "xlsx",file.name="damn4.xlsx")

#model5 is not good
linearHypothesis(reg15, c('provmf2645:inequality= 0', 'provmf2645_2:inequality= 0','provmf2645_3:inequality= 0' ), white.adjust = 'hc1')


#model2 is the best 

#
data_marr = read_dta("marriage.dta")



