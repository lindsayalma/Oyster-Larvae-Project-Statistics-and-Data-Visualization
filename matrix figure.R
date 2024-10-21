library('plot.matrix')
library(readxl)
library(ggplot2)
library(reshape2)
matrix_figure_data <- read_excel("C:/Users/Lindsay Alma/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RaccoonOlyLarvalSurvival2/data/matrix_figure_data.xlsx")
View(matrix_figure_data)
x<-matrix_figure_data
dim(x)
View(x)
melted_cormat<-melt(x)
colnames(melted_cormat)=c("X1", "X2", "value")

melted_cormat$value<-round(melted_cormat$value,digits=2)

ggplot(data = melted_cormat, aes(x=variable, y=Value, fill="value")) 

ggplot(melted_cormat, aes(X1, X2)) +    geom_tile(aes(fill = value), colour="black")+ geom_text(aes(label=value),color="black",size=4)+
  scale_fill_gradient2(low="#8C65D3", mid="white", high="#ec844c", 
                       midpoint=1, limits=range(melted_cormat$value)) +
  theme_classic()+theme(axis.text = element_text(size = 10.5))        
