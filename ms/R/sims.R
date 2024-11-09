## Simulations for plotting 


# Load required libraries
library(ggplot2)

# Generate synthetic data
set.seed(123)
days <- 1:200
temperature <- 35 + 5 * sin(2 * pi * days / length(days)) + rnorm(length(days), 0, 1)

# Add heatwave events
heatwave_days <- c(10:12, 25:27, 85:98) # Days with heatwaves
temperature[heatwave_days] <- temperature[heatwave_days] + runif(length(heatwave_days), 5, 10)

# Create a data frame
data <- data.frame(Day = days, Temperature = temperature)

# Plot the time series
ggplot(data, aes(x = Day, y = Temperature)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = subset(data, Day %in% heatwave_days), color = "red", size = 3) +
  labs(x = "Day",
       y = "Temperature (°C)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = heatwave_days, color = "red", linetype = "dashed", alpha = 0.5)

# Save the plot
ggsave(filename = "time_series_plot.png", path = "./figs/", width = 8, height = 6, dpi = 300)

# Load required libraries
library(deSolve)
library(ggplot2)
library(tidyr)

# Define the model with density dependence and stochasticity
population_dynamics <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Introduce stochasticity (random environmental effects)
    noise <- rnorm(3, mean = 0, sd = 0.05)
    
    # ODEs for population dynamics with density dependence
    dN1 <- r1 * N1 * (1 - (N1 + alpha12 * N2 + alpha13 * N3) / K1) + noise[1] * N1
    dN2 <- r2 * N2 * (1 - (N2 + alpha21 * N1 + alpha23 * N3) / K2) + noise[2] * N2
    dN3 <- r3 * N3 * (1 - (N3 + alpha31 * N1 + alpha32 * N2) / K3) + noise[3] * N3
    
    # Prevent populations from going negative
    dN1 <- ifelse(N1 + dN1 < 0, -N1, dN1)
    dN2 <- ifelse(N2 + dN2 < 0, -N2, dN2)
    dN3 <- ifelse(N3 + dN3 < 0, -N3, dN3)
    
    # Add a chance for species 3 to crash (environmental catastrophe)
    if (runif(1) < 0.001) {
      dN3 <- -N3
    }
    
    list(c(dN1, dN2, dN3))
  })
}

# Initial state (population sizes)
state <- c(N1 = 50, N2 = 30, N3 = 20)

# Parameters
parameters <- c(
  r1 = 0.5, r2 = 0.3, r3 = 0.4,  # Growth rates
  K1 = 100, K2 = 80, K3 = 120,   # Carrying capacities
  alpha12 = 0.02, alpha13 = 0.01,  # Interaction coefficients
  alpha21 = 0.03, alpha23 = 0.02,
  alpha31 = 0.01, alpha32 = 0.02
)

# Time steps
time <- seq(0, 1000, by = 0.1)

# Solve the ODE
set.seed(123)  # Ensure reproducibility
out <- ode(y = state, times = time, func = population_dynamics, parms = parameters)

# Convert to a data frame
out_df <- as.data.frame(out)

# Reshape for ggplot
out_long <- out_df %>% pivot_longer(cols = -time, names_to = "Species", values_to = "Population")

# Plot the results
ggplot(out_long, aes(x = time, y = Population, color = Species)) +
  geom_line(size = 1) +
  labs(
       x = "Time",
       y = "Population Size",
       color = "Species") +
  theme_classic(base_size = 14) + theme(legend.position = "none", axis.text = element_blank())

  ggsave(filename = "population_dynamics_plot.png", path = "./figs/", width = 5, height = 4, dpi = 300)
