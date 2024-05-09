library(diagram)
library(expm)
library(markovchain)
library(Matrix)
library(shape)
library(quantmod)
library(dplyr)

# Creatishape# Creating a matrix
tm.name <- matrix(c(0.3, 0.5, 0.2, 0.1, 0.8, 0.1, 0.5, 0.25, 0.25), nrow=3, ncol=3, byrow=TRUE)

print(tm.name)

plotmat(tm.name)
#get data

data = getSymbols("BIM.PA", src = "yahoo", from = "2015-01-01", to = "2023-12-31")

tail(data)
#Viz the data
chartSeries(BIM.PA, type='line')

returns = dailyReturn(BIM.PA)

chartSeries(returns)
returns_df <- as.data.frame(returns)  # Convert to data frame

colnames(returns_df)[1] <- "returns"


return_categories <- returns_df %>%
  mutate(Category = case_when(
    returns > 0.005 ~ "Up",    # Gains of more than 0.5%
    returns < -0.005 ~ "Down",  # Losses of more than 0.5%
    TRUE ~ "Steady"       # Minimal change, within +/-0.5%
  ))

create_transition_matrix = function(data){
  data = mutate(data, Prev_Category=lag(Category))#shift the data to align with previous day and today's return
  data = data[-1, ] # remove the first row 
  transition_table = table(data$Prev_Category, data$Category)
  transition_matrix = prop.table(transition_table, 1)
  return (as.matrix(transition_matrix))
  
}
#Calculate the transition matrix 
transition_matrix = create_transition_matrix(return_categories)

#Plot the transition matrix
plotmat(transition_matrix)
