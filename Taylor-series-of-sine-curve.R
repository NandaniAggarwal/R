library(ggplot2)
library(animint2)
library(dplyr)

# 1. Base Sine Curve Data
x_vals <- seq(-2 * pi, 2 * pi, length.out = 200)
true_sin_df <- data.frame(
  x = x_vals,
  y = sin(x_vals)
)

# 2. Iterative Approximation Data
max_terms <- 10
taylor_df <- data.frame()
error_df <- data.frame()

for (n in 1:max_terms) {
  # Calculating polynomial for current number of terms
  y_approx <- rep(0, length(x_vals))
  
  for (k in 0:(n-1)) {
    term <- ((-1)^k) * (x_vals^(2*k + 1)) / factorial(2*k + 1)
    y_approx <- y_approx + term
  }
  
  # Storing approximation data
  temp_df <- data.frame(
    iteration = n,
    x = x_vals,
    y = y_approx
  )
  taylor_df <- rbind(taylor_df, temp_df)
  
  # Calculating and storing Mean Absolute Error for the second plot
  mean_error <- mean(abs(sin(x_vals) - y_approx))
  error_df <- rbind(error_df, data.frame(
    iteration = n,
    error = mean_error
  ))
}

plot_curve <- ggplot() +
  # Sine Curve
  geom_line(
    data = true_sin_df, aes(x = x, y = y),
    color = "black", size = 1.2, alpha = 0.5,
    help = "The true Sine wave."
  ) +

  geom_line(
    data = taylor_df, aes(x = x, y = y),
    color = "red", size = 1.2,
    showSelected = "iteration",  # This links to the animation
    help = "The Taylor Series approximation updating over iterations."
  ) +
  coord_cartesian(ylim = c(-3, 3)) + # Limit y-axis so wild polynomials don't break the view
  labs(
    title = "Sine Curve vs. Taylor Series Approximation",
    x = "x (Radians)",
    y = "f(x)"
  ) +
  theme_minimal()

plot_error <- ggplot() +
  
  geom_tallrect(
    data = error_df, 
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    clickSelects = "iteration",  # Clicking selects the iteration
    alpha = 0.2, fill = "gray",
    help = "Click here to select a specific iteration."
  ) +

  geom_line(
    data = error_df, aes(x = iteration, y = error),
    color = "blue", size = 1
  ) +

  geom_point(
    data = error_df, aes(x = iteration, y = error),
    color = "red", size = 4,
    showSelected = "iteration" # Updates with animation
  ) +
  labs(
    title = "Mean Absolute Error over Iterations",
    x = "Iteration (Number of Terms)",
    y = "Error"
  ) +
  scale_x_continuous(breaks = 1:max_terms) +
  theme_minimal()

viz <- animint(
  curve = plot_curve,
  error = plot_error,
  time = list(variable = "iteration", ms = 1000), # 1000ms = 1 second per frame
  title = "Interactive Taylor Series Expansion of Sine Curve"
)

animint2dir(viz, out.dir = "sine_curve_animint")
