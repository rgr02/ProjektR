source("loadingData.R")
source("util.R")

data <- plot.db[,c(2:19)]

roots <- plot.db$Roots.1.mm+plot.db$Roots.2.mm+plot.db$Roots.5.mm+plot.db$Roots.7.mm
summary(roots)

data <- cbind(data, roots)
data <- cbind(data, plot.db[,26:31])
data <- cbind(data, plot.db[,33:44])

values <- sapply(data, summary.stats)

# Building the table
table1 <- matrix(nrow = 37, ncol = 6)
table1[,1] <- c("Dominant tree species (Latin name)","Dominant tree species code","Tree species composition",
                "Age structure of stands (even  or uneven aged)","Origin (natural or planted)","Site index",
                "Mean stand age (year)",
                "Average height of the stand height of a tree with average DBH on the plot",
                "Average DBH diameter at breast height (1.3 m above ground)",
                "Number of trees per hectare ",
                "Relative stocking ratio of basal area of a plot to basal area of the normal stand",
                "Basal area total cross sectional area of live trees at breast height in a plot Growing stock volume of stems of all living trees",
                "Stem over bark","Bark of stem","Crown wood over bark","Leaves & needles","Stump & roots of trees", 
                "Fine roots with several thresholds","Undergrowth & shrubs above ground","Undergrowth & shrubs including roots",
                "Green forest ﬂoor above ground","Green forest ﬂoor including roots","Dead organic matter", 
                "Snags","Logs","Dead branches of living trees","Dead roots","Litter","Country code (ISO ALPHA 3)",
                "Latitude (8 N to 72 N)","Longitude (8 W to 160 E)","Altitude (m a.s.l.)","Year of measurement",
                "Number of trees selected for destructive sampling","Leaf area index of trees",
                "Ecoregion","Reference")
table1[,2] <- values[1,]
table1[,3] <- values[2,]
table1[,4] <- values[3,]
table1[,5] <- values[4,]
table1[,6] <- values[5,]
colnames(table1) <- c("Field", "Number", "Unique", "Median", "Min", "Max")

write.csv2(table1, file = "table1.csv", fileEncoding = "UTF-8")
