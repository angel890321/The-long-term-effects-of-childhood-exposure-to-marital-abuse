getwd()
setwd("D:/資料視覺化")
data <- read.csv("./trauma.csv", sep = "," , header = T)

library(dplyr)
trauma_ori <- subset(data, select = c(family, ptsd1:ptsd21))
trauma <- subset(trauma_ori, ptsd1 %in% c(0:4) &ptsd2 %in% c(0:4) &ptsd3 %in% c(0:4) &
                   ptsd4 %in% c(0:4) &ptsd5 %in% c(0:4) &ptsd6 %in% c(0:4) &ptsd7 %in% c(0:4) &ptsd8 %in% c(0:4) &
                   ptsd9 %in% c(0:4) &ptsd10 %in% c(0:4) &ptsd11 %in% c(0:4) &ptsd12 %in% c(0:4) &ptsd13 %in% c(0:4) &
                   ptsd14 %in% c(0:4) &ptsd15 %in% c(0:4) &ptsd16 %in% c(0:4) &ptsd17 %in% c(0:4) &ptsd18 %in% c(0:4) &
                   ptsd19 %in% c(0:4) &ptsd20 %in% c(0:4) &ptsd21 %in% c(0:4))

trauma <- within(trauma, {
  family <- dplyr::recode(trauma$family, "三代同堂" = 0, "大家庭"= 0, "隔代教養家庭"= 0,
                "核心家庭"  = 1, "單親家庭" = 1, "繼親家庭" = 1,"其他" = 1)
})
ptsd <- subset(trauma, select = c(ptsd1:ptsd21))
ptsd2 <- subset(trauma, select = c(family, ptsd1:ptsd21))
####
library(tidySEM); library(lavaan); library(dplyr); library(ggplot2)
library(psych)
## EFA
fa.parallel(ptsd, fa = "fa", fm = "ml")
ptsd.fam.efa <- fa(ptsd, nfactors = 4, rotate = "Promax",
               residuals = TRUE, SMC = TRUE, fm = "pa")
ptsd.fam.efa$loadings
fa.diagram(ptsd.fam.efa)


##  CFA
ptsd.fam.model <- ' 
                    passive_attitude =~ ptsd10 + ptsd11 + ptsd14 + ptsd19 + ptsd21
                    physical_and_mental_symptom =~ ptsd1 + ptsd4 + ptsd7 + ptsd8 + ptsd9 + ptsd12 + ptsd13 + ptsd15 + ptsd16 + ptsd17
                    repeated_experience =~ ptsd2 + ptsd3 + ptsd5 + ptsd6 + ptsd18 + ptsd20
'

PTSD.cfa <- cfa(ptsd.fam.model, data = trauma,
                estimator = "WLSMV")
summary(PTSD.cfa, fit.measures = T)

lay1 <- get_layout("passive_attitude", "", "", "","","","","","","repeated_experience","","","","ptsd2","ptsd3","",
                   "", "", "", "physical_and_mental_symptom", "", "","","", "","","","","ptsd5","ptsd6",
                   "ptsd10", "ptsd11", "ptsd14", "ptsd19", "ptsd21","", "", "", "", "","","","","ptsd18","ptsd20",
                   "", "", "","","", "ptsd1", "ptsd4", "ptsd7", "ptsd8", "ptsd9","ptsd12","ptsd13","ptsd15","ptsd16","ptsd17",rows = 4)
graph_sem(PTSD.cfa,layout = lay1)


# 衡等性 
#### configural invariance 
PTSD.cfa.ci <- cfa(ptsd.fam.model, data = trauma,
                   estimator = "WLSMV",
                   group = "family")
fitMeasures(PTSD.cfa.ci,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(PTSD.cfa.ci, fit.measures = T,standardized = TRUE)
PTSD.cfa.allequal <- cfa(ptsd.fam.model, data = trauma,
                         estimator = "WLSMV",
                         group = "family",
                         group.equal = c("loadings", "intercepts", "residuals",
                                         "lv.variances", "lv.covariances"))
fitMeasures(PTSD.cfa.allequal,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(PTSD.cfa.allequal, fit.measures = T,standardized = TRUE)


anova(PTSD.cfa.ci, PTSD.cfa.allequal) 
# p值為0.5724 > 0.05，表示構造恆等性成立。

#### weak invariance
PTSD.cfa.wi <- cfa(ptsd.fam.model, data = trauma,
                   estimator = "WLSMV",
                   group = "family",
                   group.equal = c("loadings"))
fitMeasures(PTSD.cfa.wi,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(PTSD.cfa.wi, fit.measures = TRUE,
        standardized = TRUE)
anova(PTSD.cfa.ci, PTSD.cfa.wi) 
# p 值為 0.4732 > 0.05，表示弱恆等性成立

##### srrong invariance
PTSD.cfa.si <- cfa(ptsd.fam.model, data = trauma,
                   estimator = "WLSMV",
                   group = "family",
                   group.equal = c("loadings", "intercepts"))
fitMeasures(PTSD.cfa.si,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(PTSD.cfa.si, fit.measures = TRUE,
        standardized = TRUE)
anova(PTSD.cfa.wi, PTSD.cfa.si)
# p 值為0.3772 > 0.05，表示強恆等性成立
anova(PTSD.cfa.si, PTSD.cfa.allequal)
# p 值為0.5635 > 0.05，表示完全恆等性成立

##### 潛在變項平均數同質性
PTSD.cfa.means <- cfa(ptsd.fam.model, data = trauma,
                      estimator = "WLSMV",
                      group = "family",
                      group.equal = c("loadings", "intercepts", "residuals",
                                      "lv.variances", "lv.covariances","means"))
fitMeasures(PTSD.cfa.means,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(PTSD.cfa.means, fit.measures = TRUE,
        standardized = TRUE)
anova(PTSD.cfa.means, PTSD.cfa.allequal) 
# p 值為 0.5864 > 0.05，表示潛在變項平均數同質性成立

fit.all <- data.frame(
  fit.ci = round(c(fitMeasures(PTSD.cfa.ci,
                               fit.measures = c("chisq", "df", "pvalue",
                                                "cfi", "tli", "rmsea",
                                                "rmsea.pvalue", "srmr"))), digits = 4),
  fit.wi = round(c(fitMeasures(PTSD.cfa.wi,
                               fit.measures = c("chisq", "df", "pvalue",
                                                "cfi", "tli", "rmsea",
                                                "rmsea.pvalue", "srmr"))), digits = 4),
  fit.si = round(c(fitMeasures(PTSD.cfa.si,
                               fit.measures = c("chisq", "df", "pvalue",
                                                "cfi", "tli", "rmsea",
                                                "rmsea.pvalue", "srmr"))), digits = 4),
  fit.allequal = round(c(fitMeasures(PTSD.cfa.allequal,
                                      fit.measures = c("chisq", "df", "pvalue",
                                                       "cfi", "tli", "rmsea",
                                                       "rmsea.pvalue", "srmr"))), digits = 4),
  

  fit.means = round(c(fitMeasures(PTSD.cfa.means,
                                  fit.measures = c("chisq", "df", "pvalue",
                                                   "cfi", "tli", "rmsea","rmsea.pvalue", "srmr"))), digits = 4))
fit.all.t <- t(fit.all)
fit.all.t

## MIMIC 
ptsd.model.fam <- ' 
              # measurement model
              passive_attitude =~ ptsd10 + ptsd11 + ptsd14 + ptsd19 + ptsd21
              physical_and_mental_symptom =~ ptsd1 + ptsd4 + ptsd7 + ptsd8 + ptsd9 + ptsd12 + ptsd13 + ptsd15 + ptsd16 + ptsd17
              repeated_experience =~ ptsd2 + ptsd3 + ptsd5 + ptsd6 + ptsd18 + ptsd20 
              # structural model
              passive_attitude + physical_and_mental_symptom + repeated_experience ~ family
              # covariance
              passive_attitude ~~ physical_and_mental_symptom
              passive_attitude ~~ repeated_experience
              physical_and_mental_symptom ~~ repeated_experience
'
## 可以寫成 visual(+textual(+speed)) (分三行寫)~ sex 

ptsd.mimic.fam <- sem(ptsd.model.fam, data = ptsd2,
                    estimator = "WLSMV")
fitMeasures(ptsd.mimic.fam,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(ptsd.mimic.fam, fit.measures = TRUE, standardized = T)
graph_sem(ptsd.mimic.fam)

#install.packages("semPlot")
library(semPlot)
semPaths(ptsd.mimic.fam, intercept = FALSE, whatLabel = "std",
         residuals = FALSE, exoCov = FALSE,layout = "tree",rotation = 2
         ,sizeMan = 3,sizeMan2 = 1.5, sizeLat =5,sizeLat2 = 4,
         nCharNodes=40, label.prop=0.9, node.width = 3,edge.width = 0.5, 
         edge.label.cex = 0.5,shapeMan = "rectangle", shapeLat = "ellipse")
##
ptsd.model.fam2 <- ' 
              # measurement model
              passive_attitude =~ ptsd10 + ptsd11 + ptsd14 + ptsd19 + ptsd21
              physical_and_mental_symptom =~ ptsd1 + ptsd4 + ptsd7 + ptsd8 + ptsd9 + ptsd12 + ptsd13 + ptsd15 + ptsd16 + ptsd17
              repeated_experience =~ ptsd2 + ptsd3 + ptsd5 + ptsd6 + ptsd18 + ptsd20 
              # covariance
              passive_attitude ~~ physical_and_mental_symptom
              passive_attitude ~~ repeated_experience
              physical_and_mental_symptom ~~ repeated_experience
'
## 可以寫成 visual(+textual(+speed)) (分三行寫)~ sex 

ptsd.mimic.fam2 <- sem(ptsd.model.fam2, data = ptsd2,
                      estimator = "WLSMV")
fitMeasures(ptsd.mimic.fam2,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))
summary(ptsd.mimic.fam2, fit.measures = TRUE, standardized = T)
graph_sem(ptsd.mimic.fam2)
semPaths(ptsd.mimic.fam2, intercept = FALSE, whatLabel = "std",
         residuals = FALSE, exoCov = FALSE,layout = "tree",rotation = 2
         ,sizeMan = 3,sizeMan2 = 1.5, sizeLat =5,sizeLat2 = 4,
         nCharNodes=40, label.prop=0.9, node.width = 3,edge.width = 0.5, 
         edge.label.cex = 0.5,shapeMan = "rectangle", shapeLat = "ellipse")



fit.mimic <- data.frame(
  fit.mimic.fam = round(c(fitMeasures(ptsd.mimic.fam,
                               fit.measures = c("chisq", "df", "pvalue",
                                                "cfi", "tli", "rmsea",
                                                "rmsea.pvalue", "srmr"))), digits = 4),
  fit.mimic.fam2 = round(c(fitMeasures(ptsd.mimic.fam2,
                               fit.measures = c("chisq", "df", "pvalue",
                                                "cfi", "tli", "rmsea",
                                                "rmsea.pvalue", "srmr"))), digits = 4))
t(fit.mimic)
