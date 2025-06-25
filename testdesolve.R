install.packages("ggplot2")
install.packages("deSolve")
# ----------------------------
# 2. Define the SIR Model Function
# ----------------------------
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}
# ----------------------------
# 3. Set Parameters and Initial Values
# ----------------------------
parameters <- c(beta = 0.3, gamma = 0.1, N = 1000)
initial_state <- c(S = 999, I = 1, R = 0)
times <- seq(0, 100, by = 1)
# ----------------------------
# 4. Solve the Differential Equations
# ----------------------------
library(deSolve)
output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
output_df  <- as.data.frame(output)
# ----------------------------
# 5. Plot the Results
# ----------------------------
library(ggplot2)
ggplot(output_df, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible")) +
  geom_line(aes(y = I, color = "Infected")) +
  geom_line(aes(y = R, color = "Recovered")) +
  labs(title = "SIR Model Simulation using deSolve",
       x = "Time",
       y = "Population",
       color = "Compartment") +
  theme_minimal(base_size = 14)
