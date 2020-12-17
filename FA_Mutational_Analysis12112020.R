#load data 
read.csv("~/Desktop/FA_Mutational_Data.csv", header = T)
data <- read.csv("~/Desktop/FA_Mutational_Data.csv")
data

str(data)
head(data)
tail(data)

#packages 

library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plyr")
library(plyr)

data
data$Gene = factor(data$Gene,levels=rev(unique(data$Gene)))

data <- rename(data, c("JHU.FaDu" = "JHU-FaDu", "UM.SCC.1" = "UM-SCC-1", "SFCI.SCC.9"="SFCI-SCC-9", "CAL.27"="CAL-27", "CAL.33"="CAL-33",
                       "CCH.SCC.FA1"="CCH-SCC-FA1","CCH.SCC.FA2"="CCH-SCC-FA2","OHSU.974"="OHSU-974","VU.SCC.1131"="VU-SCC-1131","VU.SCC.1365"="VU-SCC-1365",
                       "VU.SCC.1604"="VU-SCC-1604"))
row.names(data) <- data$Gene
str(data)
melted_data <- melt(data)
head(melted_data)
tail(melted_data)

#plot 
x <- ggplot(melted_data, aes(variable, Gene, fill= c("#E69F00","#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00")[value + 1 ])) + 
  geom_tile(color = "black", size = 0.2) + 
  scale_fill_discrete() + 
  theme_minimal() + 
  ggtitle("FA Cancer Cell Line Resource Mutational Characterization") + coord_flip()


mutational.plot <- x + xlab('') +
  ylab("HNSCC Cancer Driver Genes (mutational frequency)") + theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10, face =  "bold")
        , axis.title.y = element_text(size =10, face = "bold")
        , axis.text.x = element_text(size = 7.0, color = "black", face="bold", angle = 90, hjust = 0.1)
        , axis.text.y = element_text(size =7.0, color = "black"))

mutational.plot

#final touch up and legend
final <- mutational.plot + theme(panel.grid = element_blank(), panel.border = element_blank(), legend.position = "right", 
                                 legend.direction = "vertical", legend.title = element_text(size = 10, face = "bold"), legend.text = element_text(size =9)) +
  scale_y_discrete(position = "left") + scale_x_discrete(position = "top") +
  scale_fill_discrete(name = "Mutations", labels = c( "Nonsense","Frameshift","Wildtype", "Other Mutations","Missense", "No Data")) +
  coord_equal(ratio = 0.8) 

final + theme(legend.key.size = unit(0.5,"line"))

#saving 

ggsave("FA Mutational Data Practice.pdf", width = 6, height = 8)
