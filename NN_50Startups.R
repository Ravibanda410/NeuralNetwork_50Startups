library(neuralnet)  # regression
library(nnet) # classification 

install.packages("NeuralNetTools")
library(NeuralNetTools)

library(plyr)


# Read the data
Startups <- read.csv(file.choose())
#C:/RAVI/Data science/Assignments/Module 23 Neural Network/Dataset1/50_Startups.csv/50_Startups.csv

View(Startups)
class(Startups)


Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(Startups)


Startups <- as.data.frame(Startups)
attach(Startups)


# Exploratory data Analysis :

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)

windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startups)

# Correlation coefficient - Strength & Direction of correlation
cor(Startups)

####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

summary(Startups) # Confirms on the different scale and demands normalizing the data.


# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

Startups_norm <- as.data.frame(lapply(Startups,FUN=normalize))

summary(Startups_norm$Profit) # Normalized form of profit

summary(Startups$profit) # Orginal profit value

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]


# Creating a neural network model on training data

startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)


plot(startups_model)
summary(startups_model)

par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)
#0.9556348


# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)

model_results2<-compute(Startups_model2,startups_test[1:4])

predicted_Profit2<-model_results2$net.result

cor(predicted_Profit2,startups_test$Profit)
#0.9639338

plot(predicted_Profit2,startups_test$Profit)

par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased


