source("loadingData.R")
source("util.R")


# Deleting ID and location because the variables are not in the table
tree.db <- tree.db[,c(-1,-18)]

# apply does not work although margin = 2 should apply on columns
# table2 <- apply(tree.db,2,summary.stats)

# Function works with sapply only if function parameter is parsed to a vector
# I assume, because of tree.db beeing a dataframe
values <- sapply(tree.db, summary.stats)

# Building the table
table2 <- matrix(nrow = 25, ncol = 6)
table2[,1] <- c("Tree species (Latin name)","Age (years) ","Diameter at breast height—DBH (cm)",
                   "Height of the tree (m)","Height to crown base (m)","Diameter (maximal) of the crown (m)",
                   "Stem over bark volume (dm3)","Stem bark volume (dm3)","Origin (natural or planted)",
                   "Stem over bark","Bark of stem",
                   "Crown wood over bark","Leaves & needles","Above ground","Stump & roots of trees",
                   "Total tree","Country code (ISO ALPHA-3)","Latitude",
                   "Longitude","Altitude (m a.s.l.)","Number of trees per hectare","Reference",
                   "Notes","Ecoregion","Link to the sample plot (ID of the sample plot dataset)")
table2[,2] <- values[1,]
table2[,3] <- values[2,]
table2[,4] <- values[3,]
table2[,5] <- values[4,]
table2[,6] <- values[5,]
colnames(table2) <- c("Field", "Number", "Unique", "Median", "Min", "Max")

write.csv2(table2, file = "table2.csv")
