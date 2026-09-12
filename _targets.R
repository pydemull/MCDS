# MANAGE SETUP ----
## Load packages required to define the pipeline ----
library(targets)
library(tarchetypes)

## Set target options ----
tar_option_set(
  ## Packages that targets need for their tasks
  packages = c(
    "cluster",
    "cowplot",
    "dplyr",
    "fmsb",
    "janitor",
    "factoextra",
    "flextable",
    "ggalluvial",
    "ggeffects",
    "ggplot2",
    "ggrain",
    "gtsummary",
    "hopkins",
    "lcmm",
    "lmerTest",
    "magick",
    "modelbased",
    "modelsummary",
    "officer",
    "patchwork",
    "performance",
    "quarto",
    "ragg",
    "readr",
    "scales",
    "skimr",
    "stringr",
    "tidyr"
  )
)

## Run the R scripts with custom functions ----
tar_source()

# COMPUTE TARGETS ----

list(
  ## Import data and rename variables to better fit English language ----
  tar_target(
    name = data_file_name,
    command = "data/putting.csv",
    format = "file"
  ),
  tar_target(
    name = df,
    command = read_csv2(data_file_name) |>
      rename(part = "CODE SUJET") |>
      rename_with(~ gsub("^Con_", "EMS_IM_KNOW_", .), starts_with("Con_")) |>
      rename_with(~ gsub("^Sti_", "EMS_IM_STIM_", .), starts_with("Sti_")) |>
      rename_with(~ gsub("^Acc_", "EMS_IM_ACCO_", .), starts_with("Acc_")) |>
      rename_with(~ gsub("^ReId_", "EMS_ID_", .), starts_with("ReId_")) |>
      rename_with(~ gsub("^ReIn_", "EMS_IN_", .), starts_with("ReIn_")) |>
      rename_with(~ gsub("^ReE_", "EMS_EX_", .), starts_with("ReE_")) |>
      rename_with(~ gsub("^Amot_", "EMS_AM_", .), starts_with("Amot_")) |>
      rename_with(~ gsub("^MI_", "SIMS_IM_", .), starts_with("MI_")) |>
      rename_with(~ gsub("^ID_", "SIMS_ID_", .), starts_with("ID_")) |>
      rename_with(~ gsub("^RE_", "SIMS_EX_", .), starts_with("RE_")) |>
      rename_with(~ gsub("^AM_", "SIMS_AM_", .), starts_with("AM_")) |>
      rename_with(~ gsub("^Reussite_", "SIMS_Success_", .), starts_with("Reussite_")) |>
      mutate(across(c(part, starts_with("SIMS_Success_")), as.factor)) |>
      filter(part != "PT20_67") # We removed participant 67 due to incomplete data
    # for contextual motivation.
  ),

  ## Build new datasets for facilitating subsequent analyses ----
  #### Datasets with contextual motivation data only ----
  ##### Wide format
  tar_target(
    name = df_ems,
    command = df |>
      select(part:EMS_AM_4)
  ),
  ##### Long format
  tar_target(
    name = df_ems_piv,
    command = df_ems |>
      pivot_longer(
        cols = starts_with("EMS_"),
        names_to = c("EMS_motivation", "EMS_item"),
        names_sep = "_(?=[^_]+$)",
        values_to = "EMS_value"
      ) |>
      mutate(EMS_motivation = factor(
        EMS_motivation,
        levels = c(
          "EMS_IM_KNOW",
          "EMS_IM_STIM",
          "EMS_IM_ACCO",
          "EMS_ID",
          "EMS_IN",
          "EMS_EX",
          "EMS_AM"
        )
      ))
  ),
  #### Datasets with situational motivation data only ----
  ##### Wide format
  tar_target(
    name = df_sims,
    command = df |>
      select(
        part,
        starts_with("SIMS_IM_"),
        starts_with("SIMS_ID_"),
        starts_with("SIMS_EX_"),
        starts_with("SIMS_AM_")
      )
  ),
  ##### Long format
  tar_target(
    name = df_sims_piv,
    command = df_sims |>
      pivot_longer(
        cols      = c(SIMS_IM_1:SIMS_AM_20),
        names_to  = c("SIMS_motivation", "SIMS_trial"),
        names_sep = "_(?=[^_]+$)",
        values_to = "SIMS_value"
      ) |>
      mutate(
        SIMS_motivation = factor(
          SIMS_motivation,
          levels = c(
            "SIMS_IM",
            "SIMS_ID",
            "SIMS_EX",
            "SIMS_AM"
          )
        ),
        SIMS_trial = factor(SIMS_trial, levels = as.character(1:20))
      )
  ),
  #### Datasets with success data only ----
  ##### Wide format
  tar_target(
    name = df_success,
    command = df |>
      select(part, starts_with("SIMS_Success"))
  ),
  ##### Long format
  tar_target(
    name = df_success_piv,
    command = df_success |>
      pivot_longer(
        cols      = starts_with("SIMS_Success"),
        names_to  = c("SIMS_success", "SIMS_trial"),
        names_sep = "_(?=[^_]+$)",
        values_to = "SIMS_Success"
      ) |>
      mutate(SIMS_trial = factor(SIMS_trial, levels = as.character(1:20))) |>
      select(part, SIMS_trial, SIMS_Success)
  ),
  ## Analyse contextual motivation scores ----
  ### Verify responses to questionnaire items by motivation type at the group level ----
  tar_target(
    name = gg_ems_raw_group,
    command = ggplot(data = df_ems_piv, aes(x = EMS_item, y = EMS_value)) +
      geom_rain(fill = "grey") +
      facet_wrap(~EMS_motivation) +
      theme_bw()
  ),
  ### Verify responses to questionnaire items by motivation type at the individual level ----
  tar_target(
    name = gg_ems_raw_indiv,
    command = ggplot(data = df_ems_piv, aes(x = EMS_item, y = EMS_value)) +
      geom_point(aes(color = EMS_motivation)) +
      geom_line(aes(color = EMS_motivation, group = EMS_motivation)) +
      facet_wrap(~part) +
      theme_bw()
  ),
  ### Compute scores ----
  tar_target(
    name = df_ems_summary,
    command = df_ems |>
      mutate(
        EMS_IM_KNOW_mean = (EMS_IM_KNOW_1 + EMS_IM_KNOW_2 + EMS_IM_KNOW_3 + EMS_IM_KNOW_4) / 4, # Mean score for Intrinsic motivation - Knowledge
        EMS_IM_STIM_mean = (EMS_IM_STIM_1 + EMS_IM_STIM_2 + EMS_IM_STIM_3 + EMS_IM_STIM_4) / 4, # Mean score for Intrinsic motivation - Stimulation
        EMS_IM_ACCO_mean = (EMS_IM_ACCO_1 + EMS_IM_ACCO_2 + EMS_IM_ACCO_3 + EMS_IM_ACCO_4) / 4, # Mean score for Intrinsic motivation - Accomplishment
        EMS_IM_mean = (EMS_IM_KNOW_mean + EMS_IM_STIM_mean + EMS_IM_ACCO_mean) / 3, # Mean global score for Intrinsic motivation
        EMS_ID_mean = (EMS_ID_1 + EMS_ID_2 + EMS_ID_3 + EMS_ID_4) / 4, # Mean score for Identified regulation
        EMS_IN_mean = (EMS_IN_1 + EMS_IN_2 + EMS_IN_3 + EMS_IN_4) / 4, # Mean score for Introjected regulation
        EMS_EX_mean = (EMS_EX_1 + EMS_EX_2 + EMS_EX_3 + EMS_EX_4) / 4, # Mean score for External regulation
        EMS_AM_mean = (EMS_AM_1 + EMS_AM_2 + EMS_AM_3 + EMS_AM_4) / 4, # Mean score for Amotivation
        EMS_MA = (EMS_IM_mean + EMS_ID_mean) / 2, # Autonomous motivation score
        EMS_MC = (EMS_IN_mean + EMS_EX_mean + EMS_AM_mean) / 3, # Controlled motivation score
        EMS_SD = (2 * EMS_IM_mean + EMS_ID_mean) - (2 * EMS_AM_mean + (EMS_IN_mean + EMS_EX_mean) / 2) # Self-determined motivation index
      ) |>
      select(part, EMS_IM_mean:EMS_SD)
  ),
  ### Build graphics for score distributions ----
  tar_target(
    name = gg_ems_summaries,
    command = get_ems_rainclouds(df_ems_summary) |> wrap_plots()
  ),
  ### Build motivational profiles (clusters) ----
  #### Determine the clusters ----
  ##### Scale variables
  tar_target(
    name = df_ems_summary_scaled,
    command = df_ems_summary |>
      select(EMS_IM_mean:EMS_AM_mean) |>
      scale() |>
      as.data.frame()
  ),
  ##### Explore cluster tendency
  ###### PCA graph
  tar_target(
    name = gg_pca,
    command =
      fviz_pca(
        prcomp(df_ems_summary_scaled),
        title = "PCA",
        geom = "point",
        ggtheme = theme_bw(),
        legend = "bottom"
      )
  ),
  ###### Distance matrix
  tar_target(
    name = gg_dist,
    command = fviz_dist(
      dist(df_ems_summary_scaled, method = "manhattan"),
      show_labels = TRUE
    ) +
      theme(axis.text = element_text(size = 5))
  ),
  ###### Hopkins statistic
  tar_target(
    name = hopkins_stat,
    command = {
      set.seed(123)
      hopkins(df_ems_summary_scaled)
    }
  ),
  ##### Determine the optimal number of clusters using K-Medoids approach
  tar_target(
    name = gg_cluster_determination,
    command = {
      # Build the figure for the Elbow method
      g1 <- fviz_nbclust(
        x = df_ems_summary_scaled,
        FUNcluster = cluster::pam,
        method = "wss",
        metric = "manhattan"
      ) +
        labs(subtitle = "Elbow method") +
        theme_classic()

      # Build the figure for the Silhouette method
      g2 <- fviz_nbclust(
        x = df_ems_summary_scaled,
        FUNcluster = cluster::pam,
        method = "silhouette",
        metric = "manhattan"
      ) +
        labs(subtitle = "Silhouette method") +
        theme_classic()

      # Build the figure for the Gap statistic method
      g3 <- fviz_nbclust(
        x = df_ems_summary_scaled,
        FUNcluster = cluster::pam,
        method = "gap_stat",
        metric = "manhattan",
        nstart = 25,
        nboot = 500
      ) +
        labs(subtitle = "Gap statistic method") +
        theme_classic()

      # Return figure
      g1 | g2 | g3
    }
  ),
  ##### Determine the final clusters using K-Medoids approach
  tar_target(
    name = clusters_ems,
    command = {
      set.seed(123)
      pam(
        x = df_ems_summary_scaled,
        k = 2,
        metric = "manhattan"
      )
    }
    ##################################################
    # The chosen number of clusters to determine is 2.
    ##################################################
  ),
  #### Build a figure showing the final clusters
  tar_target(
    name = gg_pca_ems_clusters,
    command = fviz_cluster(
      clusters_ems,
      ellipse.type = "convex",
      repel = TRUE,
      ggtheme = theme_minimal()
    )
  ),
  #### Add cluster variable to the `df_ems_summary` dataset
  tar_target(
    name = df_ems_summary_clust,
    command = df_ems_summary |>
      mutate(cluster = as.factor(paste0("Cluster ", clusters_ems$cluster)))
  ),
  #### Describe contextual motivation scores across clusters ----
  ##### Graphical views
  ###### Line chart
  tar_target(
    name = gg_ems_profiles_line,
    command = ggplot(
      data = df_ems_summary_clust |>
        pivot_longer(
          cols = c(starts_with("EMS"), -c(EMS_MA:EMS_SD)),
          names_to = "Motivation",
          values_to = "Score"
        ) |>
        mutate(Motivation = factor(
          Motivation,
          levels = c(
            "EMS_IM_mean",
            "EMS_ID_mean",
            "EMS_IN_mean",
            "EMS_EX_mean",
            "EMS_AM_mean"
          ),
          labels = c(
            "Intrinsic \nmotivation",
            "Identified \nregulation",
            "Introjected \nregulation",
            "External \nregulation",
            "Amotivation"
          )
        )),
      aes(x = Motivation, y = Score, color = cluster)
    ) +
      geom_line(aes(group = part), alpha = 0.3) +
      stat_summary(fun = "median", geom = "point") +
      stat_summary(aes(group = cluster), fun = "median", geom = "line", size = 2) +
      labs(color = "Clusters") +
      theme_bw()
  ),
  ###### Radar chart
  tar_target(
    name = gg_ems_profiles_radar,
    command = {
      # Get medians by cluster
      data1 <-
        df_ems_summary_clust |>
        group_by(cluster) |>
        summarise(
          across(EMS_IM_mean:EMS_AM_mean, median)
        ) |>
        select(EMS_IM_mean, EMS_AM_mean, EMS_EX_mean, EMS_IN_mean, EMS_ID_mean, cluster)

      # Add graphics bounds
      data2 <- rbind(rep(7, 5), rep(0, 5), data1 |> select(-cluster))

      # Set colors
      colors_border <- c(
        rgb(col2rgb("#F8766D")[1], col2rgb("#F8766D")[2], col2rgb("#F8766D")[3], 0.9 * 255, maxColorValue = 255),
        rgb(col2rgb("#619CFF")[1], col2rgb("#619CFF")[2], col2rgb("#619CFF")[3], 0.9 * 255, maxColorValue = 255)
      )

      colors_in <- c(
        rgb(col2rgb("#F8766D")[1], col2rgb("#F8766D")[2], col2rgb("#F8766D")[3], 0.5 * 255, maxColorValue = 255),
        rgb(col2rgb("#619CFF")[1], col2rgb("#619CFF")[2], col2rgb("#619CFF")[3], 0.3 * 255, maxColorValue = 255)
      )

      # Set path
      path <- "out/gg_ems_profiles_radar.png"

      # Open graphic device
      ragg::agg_png(filename = path, width = 5, height = 5, res = 300, units = "cm", scaling = 0.3)

      # Build chart (thanks to https://r-graph-gallery.com/)
      radarchart(
        data2,
        axistype = 1,
        seg = 7,
        # custom polygon
        pcol = colors_border,
        pfcol = colors_in,
        plwd = 3,
        plty = 1,
        # custom the grid
        cglcol = "grey", cglty = 1,
        axislabcol = "grey",
        caxislabels = seq(0, 7, 1),
        cglwd = 0.8,
        # custom labels
        vlcex = 0.8,
        calcex = 0.7,
        vlabels = c(
          "Intrinsic \nmotivation",
          "Amotivation",
          "External regulation",
          "Introjected \nregulation",
          "Identified \n regulation"
        )
      )
      legend(
        x = 0.7,
        y = 1.3,
        legend = c("Cluster 1", "Cluster 2"),
        bty = "n",
        pch = 20,
        col = colors_in,
        text.col = "black",
        cex = 0.8,
        pt.cex = 2
      )

      # Close device
      dev.off()

      # Return path
      return(path)
    },
    format = "file"
  ),
  ###### Raincloud plots
  tar_target(
    name = gg_ems_summaries_by_clust,
    command = get_ems_rainclouds(df_ems_summary_clust, group = "cluster") |>
      wrap_plots() +
      plot_layout(axis_titles = "collect", guides = "collect") &
      theme(
        legend.position = "none",
        axis.title_x = element_blank(),
        axis.title_y = element_text(size = 8)
      )
  ),
  ##### Tabular view
  tar_target(
    name = tab_ems_by_clust,
    command = df_ems_summary_clust |>
      tbl_summary(
        by = cluster,
        include = starts_with("EMS"),
        label = list(
          EMS_IM_mean = "Intrinsic motivation",
          EMS_ID_mean = "Identified regulation",
          EMS_IN_mean = "Introjected regulation",
          EMS_EX_mean = "External regulation",
          EMS_AM_mean = "Amotivation",
          EMS_MA = "Autonomous motivation",
          EMS_MC = "Controlled motivation",
          EMS_SD = "Self-determination index"
        ),
        statistic = list(
          all_continuous() ~ "{mean} \n({sd})"
        ),
        missing = "no",
      ) |>
      add_overall() |>
      modify_header(
        label = c("**Variable**")
      ) |>
      remove_footnote_header()
  ),
  ### Combine relevant datasets: contextual motivation scores and clusters, ----
  ### situational motivation variables (rearranged wide format), and success ----
  ### (long format) ----
  tar_target(
    name = df_join,
    command = df_ems_summary_clust |>
      left_join(
        df_sims_piv |>
          pivot_wider(
            names_from = SIMS_motivation,
            values_from = SIMS_value,
            names_sep = "_"
          ) |>
          mutate(
            SIMS_MA = (SIMS_IM + SIMS_ID) / 2, # Add autonomous motivation score
            SIMS_MC = (SIMS_EX + SIMS_AM) / 2, # Add controlled motivation score
            SIMS_SD = (2 * SIMS_IM + SIMS_ID) - (2 * SIMS_AM + SIMS_EX) # Add self-determined motivation index
          )
      ) |>
      left_join(df_success_piv)
  ),
  ## Investigate the effect of contextual motivation in sport on situational ----
  ## motivation ----
  ### Perform general descriptive analysis ----
  #### Graphical view
  tar_target(
    name = gg_sims_trials,
    command = {
      # Rainclouds for SIMS data
      g_sims <-
        get_sims_rainclouds(df_join) |>
        wrap_plots(ncol = 1)

      # Alluvial plots for putting success data
      g_success <-
        ggplot(
          df_join |>
            mutate(
              Freq = 1,
              SIMS_Success = factor(SIMS_Success, labels = c("Failure", "Success"))
            ),
          aes(
            x = SIMS_trial,
            stratum = SIMS_Success,
            alluvium = part,
            y = Freq,
            fill = SIMS_Success,
            color = SIMS_Success
          )
        ) +
        geom_flow(alpha = 0.2) +
        geom_stratum(aes(fill = SIMS_Success), alpha = .5, linewidth = 0.6) +
        scale_y_continuous(breaks = seq(0, 75, 25), expand = expansion(0)) +
        geom_text(
          aes(label = percent(after_stat(prop), accuracy = .1)),
          stat = "stratum",
          size = 4,
          color = "black"
        ) +
        labs(x = "Putting trial (n°)", y = "Outcome \nfrequency", color = NULL, fill = NULL) +
        theme_bw() +
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank()
        )

      # Build final figure
      wrap_plots(c(g_sims, g_success), ncol = 1) +
        plot_layout(heights = c(7.5, 0.5), axes = "collect_x") & theme(axis.title = element_text(size = 13))
    }
  ),
  #### Tabular view
  tar_target(
    name = tab_sims,
    command = {
      # Get trial IDs
      trials <- unique(df_join$SIMS_trial)

      # Get a list of gtsummary tables (Thanks to Claude AI for the code snippet)
      gt_tables <-
        lapply(trials, function(x) {
          df_join |>
            filter(SIMS_trial == x) |>
            tbl_summary(
              include = c(starts_with("SIMS"), -SIMS_trial),
              value = list(SIMS_Success ~ "1"),
              label = list(
                SIMS_IM = "Intrinsic motivation",
                SIMS_ID = "Identified regulation",
                SIMS_EX = "External regulation",
                SIMS_AM = "Amotivation",
                SIMS_MA = "Autonomous motivation",
                SIMS_MC = "Controlled motivation",
                SIMS_SD = "Self-determination index",
                SIMS_Success = "Success rate"
              ),
              statistic = list(all_continuous() ~ "{mean} \n({sd})"),
              missing = "no"
            ) |>
            remove_footnote_header()
        })

      # Merge tables
      tab_sims <-
        tbl_merge(gt_tables, tab_spanner = paste("Trial", trials))

      return(tab_sims)
    }
  ),
  ##### Convert the `part` and `SIMS_trial` variables to numeric format for
  ##### the purpose of the analyses
  tar_target(
    name = df_join_modified,
    command = df_join |>
      mutate(
        part = as.numeric(sub(".*_", "", part)),
        SIMS_trial = as.numeric(as.character(SIMS_trial)),
        # shift the SIMS_trial variable for better
        # interpretation of the intercept in
        # future models
        SIMS_trial_cent = SIMS_trial - 1
      ) |>
      as.data.frame()
  ),
  ### Latent class analysis approach ----
  #### Effect of contextual motivation on situational autonomous motivation ----
  ##### Determine the latent classes of situational motivation trajectories ----
  ###### Determine the best initial linear mixed model ----
  ####### Model 1a ----
  # Latent class linear mixed model with a fixed linear effect of trial and success
  # and a random intercept only, assuming a single class (ng=1), allowing
  # the baseline level to vary across participants
  tar_target(
    name = model_MA_1a,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent,
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1b ----
  # Latent class linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success and a random intercept only, assuming a
  # single class (ng=1), allowing the baseline level to vary across participants
  tar_target(
    name = model_MA_1b,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1c ----
  # Latent class linear mixed model with fixed linear, quadratic and cubic effects of
  # trial and a fixed effect of success and a random intercept only, assuming a
  # single class (ng=1), allowing the baseline level to vary across participants
  tar_target(
    name = model_MA_1c,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1d ----
  # Latent class linear mixed model with a fixed linear effect of trial and success,
  # a random intercept and a random linear slope over trials, assuming a single class
  # (ng=1), allowing both the baseline level and the rate of change to vary across
  # participants
  tar_target(
    name = model_MA_1d,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent,
      random = ~SIMS_trial_cent,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1e ----
  # Latent class linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # and random quadratic slope over trials, assuming a single class (ng=1), allowing
  # the baseline level, the rate of change, and the curvature of the trajectory to
  # vary across participants
  tar_target(
    name = model_MA_1e,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
      random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1f ----
  # Latent class mixed model with fixed linear, quadratic, and cubic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # random quadratic slope, and random cubic slope over trials, assuming a single
  # class (ng=1), allowing the baseline level, the rate of change, the curvature,
  # and the rate of change of curvature of the trajectory to vary across participants
  tar_target(
    name = model_MA_1f,
    command = hlme(
      fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
    ##########################################################
    # ==> model_MA_1e is the best (lowest BIC)
    # ==> We keep this model structure for subsequent analyses
    ##########################################################
  ),
  ###### Determine the best latent class mixed model ----
  # The models below are latent class mixed models with n>=2 classes, where both the
  # fixed linear and quadratic effects of trial vary across classes (mixture), with
  # random  intercept, random linear slope, and random quadratic slope over trials,
  # allowing the baseline level, the rate of change, and the curvature of the
  # trajectory to vary across participants within each class.

  ####### 2-class model ----
  tar_target(
    name = model_MA_2,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 2
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MA_1e
      )
    }
  ),
  ####### 3-class model ----
  tar_target(
    name = model_MA_3,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 3
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MA_1e
      )
    }
  ),
  ####### 4-class model ----
  tar_target(
    name = model_MA_4,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 4
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MA_1e
      )
    }
  ),
  ####### 5-class model ----
  tar_target(
    name = model_MA_5,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MA ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 5
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MA_1e
      )
    }
    ########################################################################
    # The 2-class model is chosen based on AIC, BIC, entropy and class size.
    ########################################################################
  ),
  ###### Build figure showing the fixed effects of the chosen latent class mixed ----
  ###### model ----
  tar_target(
    name = gg_best_MA_lcmm,
    command = {
      # Get predictions (with SIMS_Success = 0)
      df_dummy_fail <- data.frame(SIMS_trial = 1:20) |>
        mutate(
          SIMS_trial_cent = SIMS_trial - 1,
          SIMS_Success = as.factor(c(rep(0, 10), rep(0, 10)))
        )
      preds_fail <-
        predictY(model_MA_2, df_dummy_fail, var.time = "SIMS_trial", draws = TRUE)[[1]] |>
        as.data.frame() |>
        mutate(SIMS_trial = 1:20) |>
        pivot_longer(
          cols = c(everything(), -SIMS_trial),
          names_to = c("lines", "class"),
          names_pattern = "(.*)_(.*)",
          values_to = "pred"
        ) |>
        pivot_wider(names_from = lines, values_from = pred) |>
        mutate(
          class = factor(class, labels = c("Class 1", "Class 2")),
          SIMS_Success = as.factor("Trial result: Failure")
        )

      # Get predictions (with SIMS_Success = 1)
      df_dummy_succ <- data.frame(SIMS_trial = 1:20) |>
        mutate(
          SIMS_trial_cent = SIMS_trial - 1,
          SIMS_Success = as.factor(c(rep(1, 10), rep(1, 10)))
        )
      preds_succ <-
        predictY(model_MA_2, df_dummy_succ, var.time = "SIMS_trial", draws = TRUE)[[1]] |>
        as.data.frame() |>
        mutate(SIMS_trial = 1:20) |>
        pivot_longer(
          cols = c(everything(), -SIMS_trial),
          names_to = c("lines", "class"),
          names_pattern = "(.*)_(.*)",
          values_to = "pred"
        ) |>
        pivot_wider(names_from = lines, values_from = pred) |>
        mutate(
          class = factor(class, labels = c("Class 1", "Class 2")),
          SIMS_Success = as.factor("Trial result: Success")
        )

      # Build figure with predictions
      gg_best_lcmm <-
        ggplot() +
        geom_ribbon(data = preds_fail, aes(x = SIMS_trial, Ypred, fill = class, ymin = lower.Ypred, ymax = upper.Ypred), alpha = 0.2) +
        geom_line(data = preds_fail, aes(x = SIMS_trial, Ypred, color = class, linetype = SIMS_Success)) +
        geom_ribbon(data = preds_succ, aes(x = SIMS_trial, Ypred, fill = class, ymin = lower.Ypred, ymax = upper.Ypred), alpha = 0.2) +
        geom_line(data = preds_succ, aes(x = SIMS_trial, Ypred, color = class, linetype = SIMS_Success)) +
        labs(
          x = "Putting trial (n°)",
          y = "Predicted autonomous motivation",
          linetype = NULL,
          color = NULL,
          fill = NULL
        ) +
        scale_x_continuous(breaks = seq(1, 20, 1), limits = c(1, 20)) +
        scale_color_manual(values = c("purple", "orange")) +
        scale_fill_manual(values = c("purple", "orange")) +
        scale_linetype_manual(values = c(1, 1)) +
        coord_cartesian(xlim = c(1, 20)) +
        guides(linetype = "none") +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        ) +
        facet_wrap(~SIMS_Success)

      # Return figure
      gg_best_lcmm
    }
  ),
  ##### Link the latent classes of situational motivation trajectories to ----
  ##### motivational profiles (clusters) -----
  tar_target(
    name = model_join_clust_MA,
    command = externVar(
      model = model_MA_2,
      classmb = ~cluster,
      subject = "part",
      data = df_join_modified,
      method = "twoStageJoint"
    )
  ),
  #### Effect of contextual motivation on situational controlled motivation ----
  ##### Determine the latent classes of situational motivation trajectories ----
  ###### Determine the best initial linear mixed model ----
  ####### Model 1a ----
  # Latent class linear mixed model with a fixed linear effect of trial and success
  # and a random intercept only, assuming a single class (ng=1), allowing
  # the baseline level to vary across participants
  tar_target(
    name = model_MC_1a,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent,
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1b ----
  # Latent class linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success and a random intercept only, assuming a
  # single class (ng=1), allowing the baseline level to vary across participants
  tar_target(
    name = model_MC_1b,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1c ----
  # Latent class linear mixed model with fixed linear, quadratic and cubic effects of
  # trial and a fixed effect of success and a random intercept only, assuming a
  # single class (ng=1), allowing the baseline level to vary across participants
  tar_target(
    name = model_MC_1c,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      random = ~1,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1d ----
  # Latent class linear mixed model with a fixed linear effect of trial and success,
  # a random intercept and a random linear slope over trials, assuming a single class
  # (ng=1), allowing both the baseline level and the rate of change to vary across
  # participants
  tar_target(
    name = model_MC_1d,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent,
      random = ~SIMS_trial_cent,
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1e ----
  # Latent class linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # and random quadratic slope over trials, assuming a single class (ng=1), allowing
  # the baseline level, the rate of change, and the curvature of the trajectory to
  # vary across participants
  tar_target(
    name = model_MC_1e,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
      random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),
  ####### Model 1f ----
  # Latent class mixed model with fixed linear, quadratic, and cubic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # random quadratic slope, and random cubic slope over trials, assuming a single
  # class (ng=1), allowing the baseline level, the rate of change, the curvature,
  # and the rate of change of curvature of the trajectory to vary across participants
  tar_target(
    name = model_MC_1f,
    command = hlme(
      fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2) + I(SIMS_trial_cent^3),
      subject = "part",
      data = df_join_modified,
      ng = 1
    )
  ),

  ##########################################################
  # ==> model_MC_1e is the best (lowest BIC)
  # ==> We keep this model structure for subsequent analyses
  ##########################################################

  ###### Determine the best latent class mixed model ----
  # The models below are latent class mixed models with n>=2 classes, where both the
  # fixed linear and quadratic effects of trial vary across classes (mixture), with
  # random  intercept, random linear slope, and random quadratic slope over trials,
  # allowing the baseline level, the rate of change, and the curvature of the
  # trajectory to vary across participants within each class.

  ####### 2-class model ----
  tar_target(
    name = model_MC_2,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 2
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MC_1e
      )
    }
  ),
  ####### 3-class model ----
  tar_target(
    name = model_MC_3,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 3
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MC_1e
      )
    }
  ),
  ####### 4-class model ----
  tar_target(
    name = model_MC_4,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 4
        ),
        rep = 100,
        maxiter = 50, # the number of iterations has been increased to allow model convergence
        minit = model_MC_1e
      )
    }
  ),
  ####### 5-class model ----
  tar_target(
    name = model_MC_5,
    command = {
      set.seed(123)
      gridsearch(
        hlme(
          fixed = SIMS_MC ~ SIMS_Success + SIMS_trial_cent + I(SIMS_trial_cent^2),
          random = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          subject = "part",
          data = df_join_modified,
          mixture = ~ SIMS_trial_cent + I(SIMS_trial_cent^2),
          ng = 5
        ),
        rep = 100,
        maxiter = 30,
        minit = model_MC_1e
      )
    }
  ),

  ##################################################################################
  # End of analysis for controlled motivation because BIC increased when considering
  # latent class models with 2 classes or more compared to the initial LMM.
  ##################################################################################

  ### Linear mixed model analysis approach ----
  #### Effect of contextual motivation on situational autonomous motivation ----
  ##### Vizualise data
  tar_target(
    name = gg_sims_am_by_clust,
    command = ggplot(data = df_join, aes(x = SIMS_trial, y = SIMS_MA, color = cluster)) +
      stat_summary(aes(group = cluster), fun = "mean", geom = "point", size = 2, position = position_dodge(0.7)) +
      stat_summary(aes(group = cluster), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.07, linewidth = 0.7, position = position_dodge(0.7)) +
      stat_summary(aes(group = cluster), fun = "mean", geom = "line", linewidth = 0.7, position = position_dodge(0.7)) +
      scale_y_continuous(breaks = seq(0, 10, 2.5), limits = c(0, 10)) +
      labs(x = "Putting trial (n°)", y = "Autonomous \nmotivation score", color = "Cluster") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)
      )
  ),
  ##### Determine the most appropriate mixed model (here, Trial variable is scaled ----
  ##### to allow model convergence with lme4) ----
  ###### Model 6 ----
  # Linear mixed model with a fixed linear effect of trial and success
  # and a random intercept only, allowing the baseline level to vary across
  # participants
  tar_target(
    name = model_MA_6,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ###### Model 7 ----
  # Linear mixed model with fixed linear and quadratic effects of trial and a
  # fixed effect of success and a random intercept only, allowing the baseline
  # level to vary across participants
  tar_target(
    name = model_MA_7,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent^2)) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ###### Model 8 ----
  # Linear mixed model with fixed linear, quadratic and cubic effects of trial and a
  # fixed effect of success and a random intercept only, allowing the baseline
  # level to vary across participants
  tar_target(
    name = model_MA_8,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent^2)) * cluster +
        I(scale(SIMS_trial_cent^3)) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 9 ----
  # Linear mixed model with a fixed linear effect of trial and success, a random
  # intercept and a random linear slope over trials, allowing both the baseline
  # level and the rate of change to vary across participants
  tar_target(
    name = model_MA_9,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        (scale(SIMS_trial_cent) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 10 ----
  # Linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # and random quadratic slope over trials, allowing the baseline level, the rate
  # of change, and the curvature of the trajectory to vary across participants
  tar_target(
    name = model_MA_10,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent)^2) * cluster +
        (scale(SIMS_trial_cent) + I(scale(SIMS_trial_cent)^2) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 11 ----
  # Linear mixed model with fixed linear, quadratic, and cubic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # random quadratic slope, and random cubic slope over trials allowing the baseline
  # level, the rate of change, the curvature, and the rate of change of curvature
  # of the trajectory to vary across participants
  tar_target(
    name = model_MA_11,
    command = lmer(
      SIMS_MA ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent)^2) * cluster +
        I(scale(SIMS_trial_cent)^3) * cluster +
        (scale(SIMS_trial_cent) + I(scale(SIMS_trial_cent)^2) + I(scale(SIMS_trial_cent)^3) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  # Get a table with model coefficients
  tar_target(
    name = tab_mod_lmm_am,
    command = {
      models <- list(
        "Model 6" = model_MA_6,
        "Model 7" = model_MA_7,
        "Model 8" = model_MA_8,
        "Model 9" = model_MA_9,
        "Model 10" = model_MA_10,
        "Model 11" = model_MA_11
      )

      modelsummary(
        models,
        statistic = "({p.value})",
        stars = TRUE,
        gof_omit = "RMSE|ICC|R2",
        coef_map = c(
          "(Intercept)" = "Intercept",
          "SIMS_Success1" = "Success",
          "clusterCluster 2" = "Cluster 2",
          "scale(SIMS_trial_cent)" = "Trial (linear)",
          "I(scale(SIMS_trial_cent)^2)" = "Trial (quadratic)",
          "I(scale(SIMS_trial_cent)^3)" = "Trial (cubic)",
          "scale(SIMS_trial_cent):clusterCluster 2" = "Trial (linear) × Cluster 2",
          "clusterCluster 2:I(scale(SIMS_trial_cent)^2)" = "Trial (quadratic) × Cluster 2",
          "clusterCluster 2:I(scale(SIMS_trial_cent)^3)" = "Trial (cubic) × Cluster 2"
        ),
        output = "flextable"
      ) |>
        theme_zebra() |>
        valign(i = 1, part = "header", valign = "top") |>
        autofit()
    }
  ),
  ##############################################################################
  # The quadratic effect-based model with random intercept and slope is chosen
  # based on BIC
  ##############################################################################

  ####### Visualise the chosen model ----
  tar_target(
    name = gg_best_MA_lmm,
    command = ggpredict(model_MA_10, terms = c("SIMS_trial_cent [all]", "cluster", "SIMS_Success")) |>
      plot() +
      scale_x_continuous(breaks = 0:19, labels = 1:20) +
      scale_y_continuous(breaks = 1:10, limits = c(0, 10)) +
      scale_color_manual(values = scales::hue_pal()(2)) +
      scale_fill_manual(values = scales::hue_pal()(2)) +
      labs(title = NULL, x = "Putting trial (n°)", y = "Predictred autonomous motivation", fill = "Cluster", color = "Cluster") +
      facet_wrap(~facet, labeller = labeller(facet = c(
        "0" = "Trial result: Failure",
        "1" = "Trial result: Success"
      ))) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
  ),
  #### Effect of contextual motivation on situational controlled motivation ----
  ##### Vizualise data
  tar_target(
    name = gg_sims_cm_by_clust,
    command = ggplot(data = df_join, aes(x = SIMS_trial, y = SIMS_MC, color = cluster)) +
      stat_summary(aes(group = cluster), fun = "mean", geom = "point", size = 2, position = position_dodge(0.7)) +
      stat_summary(aes(group = cluster), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.07, linewidth = 0.7, position = position_dodge(0.7)) +
      stat_summary(aes(group = cluster), fun = "mean", geom = "line", linewidth = 0.7, position = position_dodge(0.7)) +
      scale_y_continuous(breaks = seq(0, 10, 2.5), limits = c(0, 10)) +
      labs(x = "Putting trial (n°)", y = "Controlled \nmotivation score", color = "Cluster") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)
      )
  ),
  ##### Determine the most appropriate mixed model (here, Trial variable is scaled ----
  ##### to allow model convergence with lme4) ----
  ###### Model 6 ----
  # Linear mixed model with a fixed linear effect of trial and success
  # and a random intercept only, allowing the baseline level to vary across
  # participants but assuming a constant rate of change over trials
  tar_target(
    name = model_MC_6,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ###### Model 7 ----
  # Linear mixed model with fixed linear and quadratic effects of trial and a
  # fixed effect of success and a random intercept only, allowing the baseline
  # level to vary across participants but assuming a constant rate of change over
  # trials
  tar_target(
    name = model_MC_7,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent^2)) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ###### Model 8 ----
  # Linear mixed model with fixed linear, quadratic and cubic effects of trial and a
  # fixed effect of success and a random intercept only, allowing the baseline
  # level to vary across participants but assuming a constant rate of change over
  # trials
  tar_target(
    name = model_MC_8,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent^2)) * cluster +
        I(scale(SIMS_trial_cent^3)) * cluster +
        (1 | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 9 ----
  # Linear mixed model with a fixed linear effect of trial and success, a random
  # intercept and a random linear slope over trials, allowing both the baseline
  # level and the rate of change to vary across participants
  tar_target(
    name = model_MC_9,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        (scale(SIMS_trial_cent) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 10 ----
  # Linear mixed model with fixed linear and quadratic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # and random quadratic slope over trials, allowing the baseline level, the rate
  # of change, and the curvature of the trajectory to vary across participants
  tar_target(
    name = model_MC_10,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent)^2) * cluster +
        (scale(SIMS_trial_cent) + I(scale(SIMS_trial_cent)^2) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  ####### Model 11 ----
  # Linear mixed model with fixed linear, quadratic, and cubic effects of
  # trial and a fixed effect of success, a random intercept, random linear slope,
  # random quadratic slope, and random cubic slope over trials allowing the baseline
  # level, the rate of change, the curvature, and the rate of change of curvature
  # of the trajectory to vary across participants
  tar_target(
    name = model_MC_11,
    command = lmer(
      SIMS_MC ~
        SIMS_Success +
        scale(SIMS_trial_cent) * cluster +
        I(scale(SIMS_trial_cent)^2) * cluster +
        I(scale(SIMS_trial_cent)^3) * cluster +
        (scale(SIMS_trial_cent) + I(scale(SIMS_trial_cent)^2) + I(scale(SIMS_trial_cent)^3) | part),
      data = df_join_modified |> dplyr::mutate(par = as.factor(part))
    )
  ),
  # Get a table with model coefficients
  tar_target(
    name = tab_mod_lmm_cm,
    command = {
      models <- list(
        "Model 6" = model_MC_6,
        "Model 7" = model_MC_7,
        "Model 8" = model_MC_8,
        "Model 9" = model_MC_9,
        "Model 10" = model_MC_10,
        "Model 11" = model_MC_11
      )
      modelsummary(
        models,
        statistic = "({p.value})",
        stars = TRUE,
        gof_omit = "RMSE|ICC|R2",
        coef_map = c(
          "(Intercept)" = "Intercept",
          "SIMS_Success1" = "Success",
          "clusterCluster 2" = "Cluster 2",
          "scale(SIMS_trial_cent)" = "Trial (linear)",
          "I(scale(SIMS_trial_cent)^2)" = "Trial (quadratic)",
          "I(scale(SIMS_trial_cent)^3)" = "Trial (cubic)",
          "scale(SIMS_trial_cent):clusterCluster 2" = "Trial (linear) × Cluster 2",
          "clusterCluster 2:I(scale(SIMS_trial_cent)^2)" = "Trial (quadratic) × Cluster 2",
          "clusterCluster 2:I(scale(SIMS_trial_cent)^3)" = "Trial (cubic) × Cluster 2"
        ),
        output = "flextable"
      ) |>
        theme_zebra() |>
        valign(i = 1, part = "header", valign = "top") |>
        autofit()
    }
  ),
  ##############################################################################
  # The quadratic effect-based model with random intercept and slope is chosen
  # based on BIC
  ##############################################################################

  ####### Visualise the chosen model ----
  tar_target(
    name = gg_best_MC_lmm,
    command = ggpredict(model_MC_10, terms = c("SIMS_trial_cent [all]", "cluster", "SIMS_Success")) |>
      plot() +
      scale_x_continuous(breaks = 0:19, labels = 1:20) +
      scale_y_continuous(breaks = 1:10, limits = c(0, 10)) +
      scale_color_manual(values = scales::hue_pal()(2)) +
      scale_fill_manual(values = scales::hue_pal()(2)) +
      labs(title = NULL, x = "Putting trial (n°)", y = "Predictred controlled motivation", fill = "Cluster", color = "Cluster") +
      facet_wrap(~facet, labeller = labeller(facet = c(
        "0" = "Trial result: Failure",
        "1" = "Trial result: Success"
      ))) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
  ),

  ## Export figures
  tar_target(
    name = fig_gg_ems_profiles_line,
    command = ggsave(
      "out/gg_ems_profiles_line.png",
      gg_ems_profiles_line,
      units = "cm",
      height = 5,
      width = 8,
      scale = 4
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_sims_trials,
    command = ggsave(
      "out/gg_sims_trials.png",
      gg_sims_trials,
      units = "cm",
      height = 10,
      width = 15,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_best_MA_lcmm,
    command = ggsave(
      "out/gg_best_MA_lcmm.png",
      gg_best_MA_lcmm,
      units = "cm",
      height = 5,
      width = 7,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_sims_am_by_clust,
    command = ggsave(
      "out/gg_sims_am_by_clust.png",
      gg_sims_am_by_clust,
      units = "cm",
      height = 5,
      width = 7,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_best_MA_lmm,
    command = ggsave(
      "out/gg_best_MA_lmm.png",
      gg_best_MA_lmm,
      units = "cm",
      height = 5,
      width = 7,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_sims_cm_by_clust,
    command = ggsave(
      "out/gg_sims_cm_by_clust.png",
      gg_sims_cm_by_clust,
      units = "cm",
      height = 5,
      width = 7,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = fig_gg_best_MC_lmm,
    command = ggsave(
      "out/gg_best_MC_lmm.png",
      gg_best_MC_lmm,
      units = "cm",
      height = 5,
      width = 7,
      scale = 3
    ),
    format = "file"
  ),
  tar_target(
    name = sect_properties2,
    command =
    # Set table export properties
      prop_section(
        page_size = page_size(
          orient = "landscape",
          width = 55,
          height = 17,
          unit = "cm"
        ),
        type = "continuous",
        page_margins = page_mar()
      )
  ),
  tar_target(
    name = doc_tab_ems_by_clust,
    command = save_as_docx(
      tab_ems_by_clust |>
        as_flex_table() |>
        valign(valign = "top", part = "header") |>
        add_footer_lines("Variables are shown as mean (SD)."),
      path = "out/tab_ems_by_clust.docx"
    ),
    format = "file"
  ),
  tar_target(
    name = doc_tab_sims,
    command = save_as_docx(
      tab_sims |>
        as_flex_table() |>
        delete_rows(i = 2, part = "header") |>
        compose(
          i = 1, j = 1,
          part = "header",
          value = as_paragraph("Variable")
        ) |>
        valign(valign = "top", part = "header") |>
        bold(i = 1, part = "header") |>
        add_footer_lines("Continuous variables are shown as mean (SD) and categorical variables as count (%)."),
      path = "out/tab_sims.docx",
      pr_section = sect_properties2
    )
  ),
  tar_target(
    name = doc_tab_mod_lmm_am,
    command = save_as_docx(
      tab_mod_lmm_am,
      path = "out/tab_mod_lmm_am.docx"
    ),
    format = "file"
  ),
  tar_target(
    name = doc_tab_mod_lmm_cm,
    command = save_as_docx(
      tab_mod_lmm_cm,
      path = "out/tab_mod_lmm_cm.docx"
    ),
    format = "file"
  ),

  ## Render report ----
  tar_quarto(report, "report.qmd")
)
