# generate a wiggly line for the planetary boundaries diagram
generate_wiggly_line <- function(
  n = 1000,
  x_min = 0,
  x_max = 12e3,
  y_min = -1,
  y_max = 1,
  window_size = 20
) {
  # Generate x values
  x_values <-
    seq(x_min, x_max, length.out = n)

  # Generate y values using random walk with smoothing for organic variability
  # Create random walk
  random_walk <-
    cumsum(rnorm(n, mean = 0, sd = 1))

  # Apply smoothing using moving average with edge padding

  # Pad the edges by reflecting the data
  half_window <- floor(window_size / 2)

  padded_walk <-
    c(
      rev(random_walk[1:half_window]),
      random_walk,
      rev(random_walk[(n - half_window + 1):n])
    )

  # Apply filter to padded data
  smoothed_padded <-
    stats::filter(
      padded_walk,
      rep(1 / window_size, window_size),
      sides = 2
    )

  # Remove padding to get back original length
  y_values <-
    smoothed_padded[(half_window + 1):(half_window + n)]

  # Handle any remaining NAs at the very edges
  y_values[is.na(y_values)] <- random_walk[is.na(y_values)]

  # Add small sine waves for subtle repetition within the noise
  x_seq <- seq(0, 1, length.out = n)

  small_waves <-
    0.5 * sin(2 * pi * 3.7 * x_seq) +
    0.35 * sin(2 * pi * 7.3 * x_seq) +
    0.25 * sin(2 * pi * 11.1 * x_seq)

  y_values <- y_values + small_waves

  # Normalize y values to be within the specified range
  y_values <-
    (y_values - min(y_values)) /
    (max(y_values) - min(y_values)) *
    (y_max - y_min) + y_min

  wiggly_line_df <-
    tibble::tibble(x = x_values, y = y_values)

  return(wiggly_line_df)
}