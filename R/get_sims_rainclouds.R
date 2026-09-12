get_sims_rainclouds <- function(data, group = NULL) {
  require(dplyr)
  require(ggplot2)
  require(ggrain)
  require(tidyselect)

  # Initialize list
  graphs_list <- list()

  # Set the number of variables to consider
  n <- ncol(data |> select(starts_with("SIMS"), -SIMS_trial, -SIMS_Success))

  # Get all the graphs of interest
  for (i in 1:n) {
    # Set variable name
    var_name <- names(data |> select(starts_with("SIMS"), -SIMS_trial, -SIMS_Success))[[i]]

    # Set y label name
    if (var_name == "SIMS_IM") ylabel <- "Intrinsic \nmotivation"
    if (var_name == "SIMS_ID") ylabel <- "Identified \nregulation"
    if (var_name == "SIMS_EX") ylabel <- "External \nregulation"
    if (var_name == "SIMS_AM") ylabel <- "Amotivation"
    if (var_name == "SIMS_MA") ylabel <- "Autonomous \nmotivation score"
    if (var_name == "SIMS_MC") ylabel <- "Controlled \nmotivation score"
    if (var_name == "SIMS_SD") ylabel <- "Self-determined \nmotivation index"

    if (is.null(group)) {
      g <-
        ggplot(data = data, aes(x = SIMS_trial, y = .data[[var_name]])) +
        geom_rain(
          id.long.var = "part",
          violin.args = list(fill = "grey", color = "black"),
          boxplot.args = list(linewidth = 0.5, outlier.shape = NA, width = 0.1, width = 0.2),
          boxplot.args.pos = list(
            position = ggpp::position_dodgenudge(x = 0.15, width = 0.3)
          ),
          point.args = list(color = "grey", alpha = 0.7)
        ) +
        stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
        stat_summary(fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), color = "red", width = 0.07, linewidth = 0.7) +
        stat_summary(aes(group = 1), fun = "mean", geom = "line", color = "red", linewidth = 0.7) +
        # scale_y_continuous(breaks = seq(0, 10, 2.5), limits = c(0, 10)) +
        labs(x = "Putting trial (n°)", y = ylabel) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12)
        )
    }

    if (!is.null(group)) {
      g <-
        ggplot(data = data, aes(x = SIMS_trial, y = .data[[var_name]], fill = .data[[group]], color = .data[[group]])) +
        geom_rain(
          id.long.var = "part",
          violin.args = list(color = "black", alpha = 0.3),
          boxplot.args = list(color = "black", linewidth = 0.5, outlier.shape = NA, width = 0.1, width = 0.2, alpha = 0.3),
          point.args = list(alpha = 0.3)
        ) +
        stat_summary(aes(group = .data[[group]]), fun = "mean", geom = "point", size = 2) +
        stat_summary(aes(group = .data[[group]]), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.07, linewidth = 0.7) +
        stat_summary(aes(group = .data[[group]]), fun = "mean", geom = "line", linewidth = 0.7) +
        scale_y_continuous(breaks = seq(0, 10, 2.5), limits = c(0, 10)) +
        labs(x = "Putting trial (n°)", y = ylabel, color = NULL, fill = NULL) +
        theme_bw() +
        theme(legend.position = "bottom")
    }


    graphs_list[[var_name]] <- g
  }

  return(graphs_list)
}
