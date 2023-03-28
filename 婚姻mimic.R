library(dplyr)
data <- read.csv("D:/資料視覺化/期末報告/trauma.csv")




library(tidySEM); library(lavaan)
PTSD.model.marriage <- '
     # measurement model
    passive_attitude =~ ptsd10+ptsd11+ptsd14+ptsd19+ptsd21
    physical_and_mental_symptom =~ ptsd1+ptsd4+ptsd7+ptsd8+ptsd9+ptsd12+ptsd13+ptsd15+ptsd16+ptsd17
    repeated_experience =~ ptsd2+ptsd3+ptsd5+ptsd6+ptsd18+ptsd20
    # structural model
    passive_attitude + physical_and_mental_symptom + repeated_experience ~ marriage
'

PTSD.mimic.marriage <- sem(PTSD.model.marriage, data = data,
                    estimator = "WLSMV")
summary(PTSD.mimic.marriage, fit.measures = TRUE, standardized = T)
##regressions部分皆為不顯著
##Covariances部分有顯著

graph_sem(PTSD.mimic.marriage)

ggsave("PTSD_marriage_mimic.png",  # 檔案名稱
       plot = graph_sem(PTSD.cfa,layout = lay1),   
       path = "D:/資料視覺化/期末報告", # 檔案路徑，不包含檔名
       width = 21, # 圖片寬度
       height = 9, # 圖片長度
       dpi = 300) # dpi

#畫圖另一種
library(semPlot)
png("PTSD.mimic.png", width = 1920, height =1080)
semPaths(PTSD.mimic.marriage, intercept = FALSE, whatLabel = "std",
         residuals = FALSE, exoCov = FALSE,layout = "tree",rotation = 2
         ,sizeMan = 3,sizeMan2 = 1.5, sizeLat =5,sizeLat2 = 4,
         nCharNodes=40, label.prop=0.9, node.width = 3,edge.width = 0.5, 
         edge.label.cex = 0.5,shapeMan = "rectangle", shapeLat = "ellipse")
dev.off()

#移除不顯著的路徑
PTSD.model.marriage2 <- '
    # measurement model
    passive_attitude =~ ptsd10+ptsd11+ptsd14+ptsd19+ptsd21
    physical_and_mental_symptom =~ ptsd1+ptsd4+ptsd7+ptsd8+ptsd9+ptsd12+ptsd13+ptsd15+ptsd16+ptsd17
    repeated_experience =~ ptsd2+ptsd3+ptsd5+ptsd6+ptsd18+ptsd20
    # Covariances
    passive_attitude ~~ physical_and_mental_symptom
    passive_attitude ~~ repeated_experience                  
    physical_and_mental_symptom ~~  repeated_experience  
'

PTSD.mimic.marriage2 <- sem(PTSD.model.marriage2, data = data,
                           estimator = "WLSMV")
summary(PTSD.mimic.marriage2, fit.measures = TRUE, standardized = T)

graph_sem(PTSD.mimic.marriage2)

fitMeasures(PTSD.mimic.marriage2,
            fit.measures = c("chisq", "df", "pvalue",
                             "cfi", "tli", "rmsea", 
                             "rmsea.pvalue", "srmr"))

ggsave("PTSD_marriage_mimic2.png",  # 檔案名稱
       plot = graph_sem(PTSD.cfa,layout = lay1),   
       path = "D:/資料視覺化/期末報告", # 檔案路徑，不包含檔名
       width = 21, # 圖片寬度
       height = 9, # 圖片長度
       dpi = 300) # dpi
