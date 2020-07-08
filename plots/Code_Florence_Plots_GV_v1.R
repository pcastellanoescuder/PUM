
setwd("~/Copia13.01.20/Concursos/Florence Nightgale/bbdd")
load("proc_data.rdata")
names(data)
data$Month_date <-as.Date(as.yearmon(data$month))
data$total_deaths <- data$zymotic_diseases_deaths +data$wounds_injuries_deaths + data$all_other_causes_deaths


# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
# install.packages("babynames")
library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

############################################################################################################################

### 1) Cumulative causes of deaths

Month_dates <- rep(data$Month_date,3)
aux <- as.data.frame(Month_dates)
aux$type <- c(rep("Wounds",nrow(data)),rep("Zymotic",nrow(data)),rep("Other causes",nrow(data)) )
aux$type <- as.factor(aux$type)
aux$type = factor(aux$type,c("Zymotic","Other causes", "Wounds"))

val <- "yes"  # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
aux$n <- c(data$wounds_injuries_deaths,data$zymotic_diseases_deaths,data$all_other_causes_deaths)
ylab_name <- "Cumulative number of deaths"
} else{
aux$n <- c(data$wounds_injuries_MR1000,data$zymotic_diseases_MR1000,data$all_other_causes_MR1000)
ylab_name <- "Cumulative mortality rate per 1000"  
}

p <- aux%>% 
  ggplot( aes(x=Month_dates, y=n, fill=type, text=type)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Causes of death") +
  ylab("Cumulative number of deaths") +
  theme_ipsum() +
  theme(legend.position="none")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(p, tooltip="text")

############################################################################################################################

### 2)  Causes of death (Interactive time series plot)

val <- "yes"   # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
dd <- subset(data, select=c("zymotic_diseases_deaths","wounds_injuries_deaths","all_other_causes_deaths","Month_date"))
}else {
  dd <- subset(data, select=c("zymotic_diseases_MR1000","wounds_injuries_MR1000","all_other_causes_MR1000","Month_date")) 
}

don <- xts(x = dd, order.by = dd$Month_date)
p_death <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = T, colors=RColorBrewer::brewer.pal(4, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1);p_death

############################################################################################################################

### 3)  Army size and total number of deaths (Interactive time series plot)
dd <- subset(data, select=c("average_size_of_army","total_deaths"))
don <- xts(x = dat[,c(2,10)], order.by = data$Month_date);don
p_overall <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = F, colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = F)  %>%
  dyRoller(rollPeriod = 1);p_overall


############################################################################################################################

### 4) Correlation plot I

val <- "yes"   # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
data_corr  <- subset(data, select=c("zymotic_diseases_MR1000", "wounds_injuries_MR1000" ,
                                               "all_other_causes_MR1000","average_size_of_army"))
  }else {
  data_corr <- subset(data, select=c("zymotic_diseases_deaths", "wounds_injuries_deaths" ,
                                               "all_other_causes_deaths","average_size_of_army"))
}

library(corrplot)
M <- cor(as.matrix(data_corr ))
corrplot.mixed(M,size=0.5,tl.cex=0.8)

############################################################################################################################

### 5) Correlation plot II (as scatterplots)

val <- "yes"   # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
  data_corr  <- subset(data, select=c("zymotic_diseases_MR1000", "wounds_injuries_MR1000" ,
                                      "all_other_causes_MR1000","average_size_of_army"))
}else {
  data_corr <- subset(data, select=c("zymotic_diseases_deaths", "wounds_injuries_deaths" ,
                                     "all_other_causes_deaths","average_size_of_army"))
}

library(GGally)
ggpairs(data_corr, title="correlogram with ggpairs()") 

############################################################################################################################

### 6) Stacked barplot with causes of death

val <- "yes"   # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
  dat2 <- t(as.matrix(subset(data, select=c("zymotic_diseases_deaths", "wounds_injuries_deaths" ,
                                          "all_other_causes_deaths"))))
  ylab_label <- "Number of deaths"
  }else {
  dat2 <- t(as.matrix(subset(data, select=c("zymotic_diseases_MR1000", "wounds_injuries_MR1000" ,
                                          "all_other_causes_MR1000"))))
  ylab_label <- "Mortality rate per 1000"
}

colnames(dat2) <- data$month
windows(20,14)
barplot(dat2, 
        col=RColorBrewer::brewer.pal(3, "Set2") , 
        border="white", 
        space=0.04, 
        font.axis=2, 
        ylab=ylab_label,las=2)
legend("topright",c("Zymotic","Wounds","Other causes") ,bty = "n",text.font=2,fill=RColorBrewer::brewer.pal(3, "Set2") ,
       border=RColorBrewer::brewer.pal(3, "Set2"),cex=1.4)

############################################################################################################################

### 7) Percent stacked barplot with causes of death

val <- "yes"   # Perquè l'usuari decideixi si vol graficar les morts o el MR1000
if (val== "yes"){
  dat2 <- t(as.matrix(subset(data, select=c("zymotic_diseases_deaths", "wounds_injuries_deaths" , "all_other_causes_deaths"))))
  ylab_label <- "Percentatge of deaths"
  }else {
  dat2 <- t(as.matrix(subset(data, select=c("zymotic_diseases_MR1000", "wounds_injuries_MR1000" , "all_other_causes_MR1000"))))
  ylab_label <- "Percentatge of total mortality rate per 1000"
}

colnames(dat2) <- data$Month
library(RColorBrewer)
coul <- brewer.pal(3, "Pastel2") 

# Transform this data in %
data_percentage <- apply(dat2, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
windows(20,14)
cols <- RColorBrewer::brewer.pal(3, "Set2")
barplot(data_percentage, col=RColorBrewer::brewer.pal(3, "Set2") , border="white", ylab= ylab_label,xlab=" ",las=2, ylim=c(0,115),
        cex.lab=1.2, cex.axis=1.2)
legend("topright",c("Zymotic","Wounds","Other causes") ,bty = "n",text.font=2,fill=cols ,border=cols)
