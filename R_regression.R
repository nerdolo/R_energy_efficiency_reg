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
corrplot(cors,method = 'number')

#scatter plots
x <- data_cor[,'glazing'] 
y <- data_cor[,'heating']
model <- lm(y ~ x)
plot(x, y,
     xlab="Glazing area", ylab="Heating")
abline(model, col = 4, lwd = 3)
lines(lowess(x, y), col = 3, lwd = 3)

x <- data_cor[,'roof'] 
y <- data_cor[,'heating']
model <- lm(y ~ x)
plot(x, y,
     xlab="Roof area", ylab="Heating")
abline(model, col = 4, lwd = 3)
lines(lowess(x, y), col = 3, lwd = 3)

x <- data_cor[,'wall'] 
y <- data_cor[,'heating']
model <- lm(y ~ x)
plot(x, y,
     xlab="Wall area", ylab="Heating")
abline(model, col = 4, lwd = 3)
lines(lowess(x, y), col = 3, lwd = 3)

#regression model
model <- lm(heating ~ glazing + roof + wall, data = data)
summary(model)

#model equation
# heating = 32.54 + 20.44*glazing - 0.18*roof + 0.05*wall

#estimation table
estimate_table <- data.frame(glazing=c(0.2,0.3,0.5),
                             roof=c(130,180,200),
                             wall=c(250,300,400))
estimate = c()
for(i in 1:nrow(estimate_table)){
  estimate = append(estimate, 32.54 + 20.44*estimate_table[i,'glazing'] - 0.18*estimate_table[i,'roof'] + 0.05*estimate_table[i,'wall'])
}
estimate_table$estimate <- estimate

#added-variable plots
avPlots(model)