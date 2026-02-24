library(ggplot2)
library(animint2)
library(dplyr)

# 1. Base Square Wave Data
x_vals <- seq(-2 * pi, 2 * pi, length.out = 500)
# Sign function creates a perfect square wave
true_square_df <- data.frame(
  x = x_vals,
  y = sign(sin(x_vals)) 
)

# 2. Iterative Fourier Approximation Data
max_iterations <- 15 # Number of harmonics to add
fourier_df <- data.frame()
error_df <- data.frame()

for (n in 1:max_iterations) {
  # Formula for Square Wave: (4/pi) * sum( sin(k*x)/k ) for odd k
  y_approx <- rep(0, length(x_vals))
  
  for (i in 1:n) {
    k <- 2 * i - 1 # Only odd harmonics: 1, 3, 5, 7...
    term <- (4 / pi) * (sin(k * x_vals) / k)
    y_approx <- y_approx + term
  }
  
  # Storing approximation data
  temp_df <- data.frame(
    n_terms = n,
    x = x_vals,
    y = y_approx
  )
  fourier_df <- rbind(fourier_df, temp_df)
  
  # Calculating Mean Absolute Error (MAE)
  mean_error <- mean(abs(true_square_df$y - y_approx))
  error_df <- rbind(error_df, data.frame(
    n_terms = n,
    error = mean_error
  ))
}

# GRAPH 1: Square Wave vs Fourier
plot_fourier <- ggplot() +
  # The Target Square Wave (Fixed)
  geom_line(
    data = true_square_df, aes(x = x, y = y),
    color = "black", size = 1, alpha = 0.3, linetype = "dashed",
    help = "The target square wave we want to approximate."
  ) +
 
  geom_line(
    data = fourier_df, aes(x = x, y = y),
    color = "darkgreen", size = 1.2,
    showSelected = "n_terms",
    help = "The Fourier Series approximation using sine waves."
  ) +
  labs(
    title = "Fourier Series: Approximating a Square Wave",
    x = "x (Radians)",
    y = "f(x)"
  ) +
  theme_minimal()

# GRAPH 2: Error Tracking with Interaction
plot_error <- ggplot() +
 
  geom_tallrect(
    data = error_df, 
    aes(xmin = n_terms - 0.5, xmax = n_terms + 0.5),
    clickSelects = "n_terms",
    alpha = 0.2, fill = "blue",
    help = "Click a bar to see the approximation for that number of harmonics."
  ) +
  
  geom_line(
    data = error_df, aes(x = n_terms, y = error),
    color = "red", size = 1
  ) +
 
  geom_point(
    data = error_df, aes(x = n_terms, y = error),
    color = "black", size = 3,
    showSelected = "n_terms"
  ) +
  labs(
    title = "Approximation Error (Residuals)",
    x = "Number of Harmonics (n)",
    y = "Mean Absolute Error"
  ) +
  scale_x_continuous(breaks = 1:max_iterations) +
  theme_minimal()

viz <- animint(
  curve = plot_fourier,
  error = plot_error,
  time = list(variable = "n_terms", ms = 1500), # 1.5 seconds per frame
  title = "Fourier Series Interactive Demonstration",
  source = "https://github.com/NandaniAggarwal/R/blob/main/fourier_series.R"
)

viz
