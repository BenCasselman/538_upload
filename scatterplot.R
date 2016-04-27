# Interactive Scatters
# This is basic code for generating scatter plots with tooltips for exploratory analysis.
# It is intended as a VERY basic tool with minimal formatting or other complexity.
# Comments/questions, suggestions: ben.casselman@fivethirtyeight.com

# Code assumes your data is already cleaned and ready in a csv. Of course could also clean/format inside R.
# Default variables are: "label", "xvar", "yvar" and, optionally, "size". You can of course change these.

require(readr)
require(dplyr)
require(ggvis)

# Read in file from csv. 
mydata <- read_csv("myfile.csv")

# If you just want the label to show up:
chart <- mydata %>%
  ggvis(~xvar, ~yvar) %>% # Change variable names here
  layer_points(key := ~label) %>%
  add_tooltip(function(x) x$label, "hover")

chart

# Or, if you want size as well:
chart <- mydata %>%
  ggvis(~xvar, ~yvar) %>% # Change variable names here
  layer_points(key := ~label, size = ~size) %>% # "size" is a variable name -- change if needed
  add_tooltip(function(x) x$label, "hover")

chart

# If you want all three pieces of information listed in tooltip, then two steps:

# Set up tooltip:
labels <- function(x) {
  if(is.null(x)) return(NULL)
  row <- mydata[mydata$label == x$label, ] # Have to change variable name "label" if it's called something different in your file.
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

# Then make scatter:
chart <- mydata %>%
  ggvis(~xvar, ~yvar) %>% # Remember to change variable names
  layer_points(key := ~label) %>% # Or "layer_points(key := ~label, size = ~size)" if you want size
  add_tooltip(labels, "hover")

chart

# Add regression line
chart %>%
  layer_smooths(stroke := "red")

# You can of course do lots of formatting, etc. But this gets things started.

