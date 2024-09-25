library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

nc <- read_xlsx("North_Carolina_District_all.xlsx", 
                sheet = "North_Carolina_District_all",col_names = TRUE)
nc1 <- nc[,!grepl("MOE$",names(nc))]

#Remove rows with totals
nc1 <- nc1[!grepl("Total population|All people", nc1$Title),]

# Unpivot the district columns
unpivoted_nc1 <- nc1 %>% 
    pivot_longer(
      colnames(nc1)[4:17]
      ,names_to = "District"
      ,values_to = "Population"
      ,values_drop_na = TRUE
    )




#Box Plot

# fig <- plot_ly(unpivoted_nc1, y = ~Population, color = ~Topic, type = "box")
# 
# 
# fig

