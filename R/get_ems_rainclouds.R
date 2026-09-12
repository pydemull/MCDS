get_ems_rainclouds <- function(data, group = NULL) {
  require(dplyr)
  require(ggplot2)
  require(ggrain)
  require(tidyselect)

  # Select variable of interest
  data_select <- data |> select(starts_with("EMS"))

  # Initialize list
  graphs_list <- list()

  # Set the number of variables to consider
  n <- ncol(data_select)

  # Get all the graphs of interest
  for (i in 1:n) {
    # Set variable name
    var_name <- names(data_select)[[i]]

    # Set graphs limits
    if (substr(var_name, 1, 6) == "EMS_SD") {
      seq <- seq(0, 22, 2)
      lower_lim <- 0
      upper_lim <- 22
    } else {
      seq <- 0:7
      lower_lim <- 0
      upper_lim <- 7
    }

    # Set y label name
    if (var_name == "EMS_IM_mean") ylabel <- "Intrinsic motivation"
    if (var_name == "EMS_ID_mean") ylabel <- "Identified regulation"
    if (var_name == "EMS_IN_mean") ylabel <- "Introjected regulation"
    if (var_name == "EMS_EX_mean") ylabel <- "External regulation"
    if (var_name == "EMS_AM_mean") ylabel <- "Amotivation"
    if (var_name == "EMS_MA") ylabel <- "Autonomous motivation score"
    if (var_name == "EMS_MC") ylabel <- "Controlled motivation score"
    if (var_name == "EMS_SD") ylabel <- "Self-determined motivation index"

    if (is.null(group)) {
      g <-
        ggplot(data = data, aes(x = 0, y = .data[[var_name]])) +
        geom_rain(
          violin.args = list(fill = "grey", color = NA),
          boxplot.args = list(linewidth = 0.5, outlier.shape = NA, width = 0.02),
          boxplot.args.pos = list(
            position = ggpp::position_dodgenudge(x = 0.15, width = 0.3)
          ),
          point.args = list(color = "grey", alpha = 0.7)
        ) +
        stat_summary(aes(x = 0.1), fun = "mean", geom = "point", size = 2) +
        stat_summary(aes(x = 0.1), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.015) +
        coord_flip(xlim = c(-0.08, 0.55)) +
        theme_bw() +
        labs(y = ylabel) +
        scale_y_continuous(breaks = seq, limits = c(lower_lim, upper_lim)) +
        theme(
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
        )
    }

    if (!is.null(group)) {
      if (group == "SDI_group") {
        xlabel <- "Self-determination index group"
      }
      if (group == "cluster") {
        xlabel <- "Cluster"
      }

      g <-
        ggplot(data = data, aes(x = .data[[group]], y = .data[[var_name]], color = .data[[group]], fill = .data[[group]])) +
        geom_rain(
          boxplot.args = list(color = "black", linewidth = 0.5, outlier.shape = NA, width = 0.05),
          boxplot.args.pos = list(
            position = ggpp::position_dodgenudge(x = 0.15, width = 0.3)
          ),
          point.args = list(alpha = 0.2)
        ) +
        stat_summary(fun = "mean", geom = "point", size = 2) +
        stat_summary(fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.015) +
        theme_bw() +
        labs(x = xlabel, y = ylabel) +
        scale_y_continuous(breaks = seq, limits = c(lower_lim, upper_lim)) +
        theme(
          legend.position = "none",
          axis.title_x = element_blank(),
          axis.title_y = element_text(size = 8)
        )
    }


    graphs_list[[var_name]] <- g
  }

  return(graphs_list)
}
