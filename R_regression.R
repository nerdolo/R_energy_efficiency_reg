library('xlsx')
library('dplyr')
library('corrplot')
library("Hmisc")
library('ggplot2')
library('car')


# data prep
data <- read.xlsx2('ENB2012_data.xlsx', 1)
colnames(data) <- c('comp','surface','wall','roof','height','orientation','glazing','glazing distr', 'heating', 'cooling')
data <- data %>% mutate_all(na_if,"")
data <- na.omit(data)
data <- mutate_all(data, function(x) as.numeric(as.character(x)))

# correlation matrix
data_cor <- data[,c('comp', 'surface', 'wall', 'roof', 'height', 'glazing','heating','cooling')]
cors <- cor(data_cor)
corrplot(cors,method = 'number',main = "Correlation matrix for dataset variables",mar=c(5,4,4,2))

#scatter plots
scatter_plot <- function(x,y,xlabel,ylabel, mainlabel){
  model <- lm(y ~ x)
  plot(x, y,
       xlab=xlabel, ylab=ylabel, main = mainlabel)
  abline(model, col = 4, lwd = 3)
  lines(lowess(x, y), col = 3, lwd = 3)
  legend(x="topleft", legend=c("Linear regression line", "Lowess regression curve"),  
         fill = c(4,3))
}

scatter_plot(data_cor[,'glazing'],data_cor[,'heating'],"Glazing area", "Heating load",
             "Heating load and glazing area scatter plot")
scatter_plot(data_cor[,'roof'],data_cor[,'heating'],"Roof area", "Heating load",
             "Heating load and roof area scatter plot") 
scatter_plot(data_cor[,'wall'],data_cor[,'heating'],"Wall area", "Heating load",
             "Heating load and wall area scatter plot")


#regression model
model <- lm(heating ~ glazing + roof + wall, data = data)
summary(model)


#estimation table
estimate_table <- data.frame(glazing=c(0.2,0.3,0.5),
                             roof=c(130,180,200),
                             wall=c(250,300,400))
estimate_table$estimates <- predict(model, estimate_table)

#added-variable plots
avPlots(model, id=FALSE, ylab="Heating load | others", main="Added-Variable Plots for glazing area, roof area and wall area")
