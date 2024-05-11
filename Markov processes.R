library(diagram)
library(expm)
library(markovchain)
library(Matrix)
library(shape)
library(quantmod)
library(dplyr)
library(expm)
library(markovchain)
# Creatishape# Creating a matrix
tm.name <- matrix(c(0.3, 0.5, 0.2, 0.1, 0.8, 0.1, 0.5, 0.25, 0.25), nrow=3, ncol=3, byrow=TRUE)

print(tm.name)

plotmat(tm.name)
#get data

data = getSymbols("BIM.PA", src = "yahoo", from = "2020-01-01", to = "2023-12-31")

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

nstep_matrix = 2
#Calculate the transition matrix 
transition_matrix = create_transition_matrix(return_categories)
transition_matrixprint(transition_matrix)
#Plot the transition matrix
plotmat(transition_matrix)

#Calculate the probaility of 3 Downs followed by one Up
transition_4 = transition_matrix %^% 4
prob_up_after_3_downs = transition_4['Down','Up']
print(prob_up_after_3_downs)



#using the Chapman-Kolmogorov Equation n = 4 steps

transition_matrix_2 = transition_matrix %^% 2 #2-step transition
transition_matrix_4_ck = transition_matrix_2 %*% transition_matrix_2
print(transition_matrix_4_ck['Down','Up'])


#n-step transition probaility matrix for a specific n value 
n = 20
n_step_tm = transition_matrix %^%n 
print(n_step_tm)


# The inconditional 
# Define the transition matrix directly (as your matrix seems uniform for all states)
transition_matrix <- matrix(
  c(0.3577236, 0.2330623, 0.4092141,
   0.3927126 ,0.2348178 ,0.3724696,
   0.3414634, 0.2512195, 0.4073171),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Down", "Steady", "Up"), c("Down", "Steady", "Up"))
)

# Ensure the transition matrix is in matrix form
transition_matrix_mat <- as.matrix(transition_matrix)

# Compute eigenvalues and eigenvectors
eigen_result <- eigen(transition_matrix_mat)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors

# Print eigenvalues
print("Eigenvalues:")
print(eigenvalues)

# Identify the eigenvector corresponding to the eigenvalue closest to 1
index_of_one <- which.min(abs(eigenvalues - 1))
stationary_distribution <- eigenvectors[, index_of_one]

# Normalize the stationary distribution to sum to 1
stationary_distribution <- stationary_distribution / sum(stationary_distribution)
# Print the stationary distribution
print("Stationary Distribution:")
print(stationary_distribution)

determinant_value <- det(transition_matrix_mat)
print("Determinant of the Transition Matrix:")
print(determinant_value)


# Proprety classes : 
dtmc<-new("markovchain",transitionMatrix=transition_matrix,
               states=c("Down", "Steady", "Up"))


recurrentClasses(dtmc)
transientClasses(dtmc)
absorbingStates(dtmc)
steadyStates(dtmc)


#Simulations : 
if (!requireNamespace("markovchain", quietly = TRUE)) {
  install.packages("markovchain")
}
library(markovchain)

ntraj_sim_num <- 10
ntraj_n_steps <- 251
init_prob <- c(0.30, 0.4, 0.3)  # Assuming 3 states

states_matrix_name <- matrix(NA, nrow = ntraj_n_steps, ncol = ntraj_sim_num)

set.seed(123)

mc.name <- new("markovchain", states = c("Down", "Steady", "Up"), byrow = TRUE, transitionMatrix = transition_matrix)

# Simulating states
for (i in 1:ntraj_sim_num) {
  # Generating initial state within the loop for each simulation
  init_state_name <- sample(1:3, 1, prob = init_prob)
  
  # Simulate states using the Markov chain object
  states_matrix_name[, i] <- rmarkovchain(n = ntraj_n_steps - 1, 
                                          object = mc.name, 
                                          t0 = 'Down', 
                                          include.t0 = TRUE)
}


if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}
library(reshape2)

# Melt the states matrix to long format
long_data <- melt(states_matrix_name, varnames = c("Time", "Simulation"))

# Adjust Time to reflect actual time steps if include.t0 = TRUE
long_data$Time <- long_data$Time - 1

state_plot <- ggplot(long_data, aes(x = Time, fill = value)) +
  geom_histogram(stat = "count", position = "fill", binwidth = 1) +
  labs(title = "State Distribution Over Time in Markov Chain Simulations",
       x = "Time",
       y = "Proportion",
       fill = "State") +
  theme_minimal()

print(state_plot)


