# Create a lattice xy plot object in memory ------------------------------------------------
# Purpose:        to construct a valid lattice log_x-y plot of one or more series 
# Description:    The argument `the_data_frame` is in long form, where each row has data
#                 for a sub-group. The list of arguments to construct the plot are:
#                 1. the name of the variable that defines the subgroups, called subgroup_variable
#                 2. a list of text labels for title, x and y axis, called text_labels_for_plot
#                 3. a vector of the min and max values to use for the y axis, min_max_y_to_plot
#                 4. a vector of text descriptions for every series to plot, or NULL, called subgroup_descriptions
create_xyplot <- function(the_data_frame, 
                          args_to_plot) {
  # unpack arguments
  the_subgroup_variable_name <- args_to_plot[['subgroup_variable']]
  the_text_plot_labels <- args_to_plot[['text_labels_for_plot']]
  min_max_y_to_plot <- args_to_plot[['min_max_y_to_plot']]
  the_subgroup_descriptions <- args_to_plot[['subgroup_descriptions']]
  
  # setup values for plotting
  group_labels <- as.vector(unique(the_data_frame[the_subgroup_variable_name]))
  wager_labels <- as.vector(sapply( group_labels, FUN = function(x) {paste0("Wager ", x)}))
  if (is.null(the_subgroup_descriptions)) {
    the_subgroup_descriptions <- wager_labels
  }
  wagers_vector <- the_data_frame[[the_subgroup_variable_name]]
  
  max_x_value <- length(wagers_vector[wagers_vector==group_labels[[the_subgroup_variable_name]][1]])
  
  xyplot(
    runningprofit ~ tries,
    data = the_data_frame,
    groups = wager,
    par.settings = list(superpose.line = list(
      col = c("blue", "red", "green", "yellow", "brown", "cyan"),
      lwd = 1
    )),
    auto.key = list(
      space = "top",
      columns = 2,
      text = the_subgroup_descriptions,
      title = the_text_plot_labels[['title']],
      cex.title = 2,
      lines = TRUE,
      points = FALSE
    ),
    xlab = the_text_plot_labels[['x_label']],
    xlim = c(1, max_x_value),
    scales = list(
      cex = c(1.1, 1.1),            # increase font size
      x = list(log = T),            # log scale for x-axis
      y = list(log = F),
      alternating = 1,              # axes labels left/bottom
      tck = c(1, 0)
    ),                                # ticks only with labels
    ylab = the_text_plot_labels[['y_label']],
    ylim = min_max_y_to_plot,
    type = c("l"),
    panel = panel.superpose,
    panel.groups = function(x, y, group.number, ...) {
      panel.abline(h = y[which(y == 0.0)],
                   lty = "dotted",
                   col = "black")
      panel.grid(v = -1,
                 h = -1,
                 lty = 3)
      xt <- x[x == log(min(x) + 1)] # find x coordinate for first point
      yt <- y[x == min(x)] # find y coordinate for first point
      panel.text(xt,
                 yt,
                 labels = wager_labels[group.number],
                 pos = 4,  # show labels on right side
                 ...)
      panel.xyplot(x, y, ...)
    }
  )
}
