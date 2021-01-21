library(patternplot)

library(ggplot2)
library(jpeg)
Tomato <-  readJPEG(system.file("img", "tomato.jpg", package="patternplot"))
Peas <- readJPEG(system.file("img", "peas.jpg", package="patternplot"))
Potatos <-  readJPEG(system.file("img", "potatos.jpg", package="patternplot"))

#Example 1
data <- read.csv(system.file("extdata", "vegetables.csv", package="patternplot"))
pattern.type<-list(Tomatoes,Peas,Potatoes)
imagepie(group=data$group,pct=data$pct,label=data$label,pattern.type=pattern.type,
         label.distance=1.3,frame.color='burlywood4', frame.size=0.8, label.size=6,
         label.color='forestgreen')+ggtitle('Pie Chart with Images')