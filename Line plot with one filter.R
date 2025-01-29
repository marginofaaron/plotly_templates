
# Single Filter -----------------------------------------------------------

# Load required libraries
library(plotly)
library(dplyr)

# Example DataFrame
set.seed(123)
df <- data.frame(
  Year = rep(2012:2024, times = 3),
  Metric = rep(c("Metric A", "Metric B", "Metric C"), each = 13),  # Metric names
  State = rep("Example State", 39),
  Ranking = sample(1:50, 39, replace = TRUE)
)

# Get unique Metric values
unique_metrics <- unique(df$Metric)

# Initialize an empty plot
plot_obj <- plot_ly()

# Store traces for each metric
for (i in seq_along(unique_metrics)) {
  selected_metric <- unique_metrics[i]
  filtered_data <- df %>% filter(Metric == selected_metric)
  
  plot_obj <- plot_obj %>%
    add_trace(
      data = filtered_data,
      x = ~Year,
      y = ~Ranking,
      type = 'scatter',
      mode = 'lines+markers',
      name = selected_metric,
      visible = ifelse(i == 1, TRUE, FALSE)  # Show only first metric initially
    )
}

# Create dropdown buttons to toggle metric visibility
dropdown_buttons <- lapply(seq_along(unique_metrics), function(i) {
  visibility_vector <- rep(FALSE, length(unique_metrics))  # Start with all traces hidden
  visibility_vector[i] <- TRUE  # Make only the selected metric visible
  
  list(
    method = "update",
    args = list(
      list(visible = visibility_vector),  # Update trace visibility
      list(yaxis = list(title = "Ranking", autorange = "reversed"))  # Keep axis reversed
    ),
    label = unique_metrics[i]
  )
})

# Add dropdown menu with title
plot_obj <- plot_obj %>%
  layout(
    title = "Ranking Over Time by Metric",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Ranking", autorange = "reversed"),  # Ensure ranking is reversed initially
    updatemenus = list(
      list(
        buttons = dropdown_buttons,
        direction = "down",
        x = 0.2,
        y = 1.15,
        showactive = TRUE,
        name = "Select Metric"  # Adds dropdown title
      )
    )
  )

# Display the interactive plot
plot_obj


