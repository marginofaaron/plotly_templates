
# Multiple filters --------------------------------------------------------

# Load required libraries
library(plotly)
library(dplyr)

# Example DataFrame with Multiple States
set.seed(123)
df <- data.frame(
  Year = rep(2012:2024, times = 6),
  Metric = rep(c("Metric A", "Metric B", "Metric C"), each = 26),
  State = rep(c("California", "Texas"), each = 13, times = 3),
  Ranking = sample(1:50, 78, replace = TRUE)
)

# Get unique Metric and State values
unique_metrics <- unique(df$Metric)
unique_states <- unique(df$State)

# Initialize an empty plot
plot_obj <- plot_ly()

# Create traces for each combination of Metric & State
trace_count <- length(unique_metrics) * length(unique_states)

for (i in seq_along(unique_metrics)) {
  for (j in seq_along(unique_states)) {
    selected_metric <- unique_metrics[i]
    selected_state <- unique_states[j]
    
    filtered_data <- df %>%
      filter(Metric == selected_metric, State == selected_state)
    
    plot_obj <- plot_obj %>%
      add_trace(
        data = filtered_data,
        x = ~Year,
        y = ~Ranking,
        type = 'scatter',
        mode = 'lines+markers',
        name = paste(selected_metric, "-", selected_state),  # Unique trace name
        visible = ifelse(i == 1 & j == 1, TRUE, FALSE)  # Show only the first metric & state initially
      )
  }
}

# Create an initial visibility matrix
default_visibility <- rep(FALSE, trace_count)
default_visibility[1] <- TRUE  # Show only the first combination initially

# Function to generate visibility arrays
generate_visibility <- function(selected_metric_index, selected_state_index) {
  visibility_vector <- rep(FALSE, trace_count)
  trace_index <- (selected_state_index - 1) * length(unique_metrics) + selected_metric_index
  visibility_vector[trace_index] <- TRUE
  return(visibility_vector)
}

# Create dropdown buttons for Metric selection
metric_buttons <- lapply(seq_along(unique_metrics), function(i) {
  list(
    method = "update",
    args = list(
      list(visible = generate_visibility(i, 1)),  # Initially, use first state
      list(yaxis = list(title = "Ranking", range = c(50, 0)))  # Keep y-axis fixed
    ),
    label = unique_metrics[i]
  )
})

# Create dropdown buttons for State selection
state_buttons <- lapply(seq_along(unique_states), function(j) {
  list(
    method = "update",
    args = list(
      list(visible = generate_visibility(1, j)),  # Initially, use first metric
      list(yaxis = list(title = "Ranking", range = c(50, 0)))  # Keep y-axis fixed
    ),
    label = unique_states[j]
  )
})

# Add dropdown menus for Metric & State
plot_obj <- plot_obj %>%
  layout(
    title = "Ranking Over Time by Metric and State",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Ranking", range = c(50, 0)),  # Fixed reversed range
    updatemenus = list(
      list(
        buttons = metric_buttons,
        direction = "down",
        x = 1.5, y = .75,
        showactive = TRUE,
        name = "Select Metric"
      ),
      list(
        buttons = state_buttons,
        direction = "down",
        x = 1.5, y = .5,  # Positioning the second dropdown to the right
        showactive = TRUE,
        name = "Select State"
      )
    )
  )

# Display the interactive plot
plot_obj