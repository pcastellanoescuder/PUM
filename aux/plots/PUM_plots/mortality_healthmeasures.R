
# Based on
# Script: http://finzi.psych.upenn.edu/R/library/HistData/html/Nightingale.html

library(HistData)
data(Nightingale) 
require(reshape)

Night<- Nightingale[,c(1,8:10)]
melted <- melt(Night, "Date")
names(melted) <- c("Date", "Cause", "Deaths")
melted$Cause <- sub("\\.rate", "", melted$Cause)
melted$Regime <- ordered( rep(c(rep('Before', 12), rep('After', 12)), 3), 
                          levels=c('Before', 'After'))
Night <- melted

# subsets, to facilitate separate plotting
Night1 <- subset(Night, Date < as.Date("1855-04-01"))
Night2 <- subset(Night, Date >= as.Date("1855-04-01"))

# sort according to Deaths in decreasing order, so counts are not obscured [thx: Monique Graf]
Night1 <- Night1[order(Night1$Deaths, decreasing=TRUE),]
Night2 <- Night2[order(Night2$Deaths, decreasing=TRUE),]

# merge the two sorted files
Night <- rbind(Night1, Night2)

# Causes of Mortality according to health measures
windows() 
colors <- c("blue", "red", "black")
with(Nightingale, {
  plot(Date, Disease.rate, type="n", cex.lab=1.25, 
       ylab="Annual Death Rate", xlab="Date",  
       main="Causes of Mortality of the British Army in the East");
  # background, to separate before, after
  rect(as.Date("1854/4/1"), -10, as.Date("1855/3/1"), 
       1.02*max(Disease.rate), col=gray(.90), border="transparent");
  text( as.Date("1854/4/1"), .98*max(Disease.rate), "Before health measures", pos=4);
  text( as.Date("1855/4/1"), .98*max(Disease.rate), "After health measures", pos=4); 
  points(Date, Disease.rate, type="b", col=colors[1], lwd=3);
  points(Date, Wounds.rate, type="b", col=colors[2], lwd=2);
  points(Date, Other.rate, type="b", col=colors[3], lwd=2)
}
) 
axis.Date(1, at=seq(as.Date("1854/4/1"), as.Date("1856/3/1"), "3 months"), format="%b %Y")
legend(as.Date("1855/10/20"), 700, c("Preventable disease", "Wounds and injuries", "Other"),
       col=colors, fill=colors, title="Cause", cex=0.6)

