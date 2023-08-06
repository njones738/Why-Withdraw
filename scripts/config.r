config <- configure_report(add_introduce = TRUE,
                           add_plot_intro = TRUE,
                           add_plot_str = F,
                           add_plot_missing = TRUE,
                           add_plot_histogram = TRUE,
                           add_plot_density = TRUE,
                           add_plot_qq = T,
                           add_plot_bar = TRUE,
                           add_plot_correlation = TRUE,
                           add_plot_prcomp = T,
                           add_plot_boxplot = TRUE,
                           add_plot_scatterplot = TRUE,
                           introduce_args = list(),
                           plot_intro_args = list(geom_label_args = list(),
                                                  title = NULL,
                                                  ggtheme = theme_bw(),
                                                  theme_config = list()
                                                 ),
                           plot_str_args = list(type = "diagonal",
                                                fontSize = 35,
                                                width = 1000,
                                                margin = list(left = 350, right = 250) # nolint
                                               ),
                           plot_missing_args = list(group = list(Good = 0.3, OK = 0.4, Bad = 0.427, Remove = 1), # nolint
                                                    missing_only = FALSE,
                                                    geom_label_args = list(),
                                                    title = NULL,
                                                    ggtheme = theme_bw(),
                                                    theme_config = list(legend.position = c("bottom")) # nolint
                                                   ),
                           plot_histogram_args = list(geom_histogram_args = list(bins = 75L, # nolint
                                                                                 fill = "lavender", # nolint
                                                                                 col = "black"), # nolint
                                                      binary_as_factor = TRUE,
                                                      scale_x = "continuous",
                                                      title = NULL,
                                                      ggtheme = theme_bw(),
                                                      theme_config = list(),
                                                      nrow = 2L,
                                                      ncol = 5L,
                                                      parallel = FALSE
                                                     ),
                           plot_density_args = list(binary_as_factor = TRUE,
                                                    geom_density_args = list(fill = "lavender", col = "black"),
                                                    scale_x = "continuous",
                                                    title = NULL,
                                                    ggtheme = theme_bw(),
                                                    theme_config = list(),
                                                    nrow = 2L,
                                                    ncol = 5L,
                                                    parallel = FALSE
                                                   ),
                           plot_qq_args = list(sampled_rows = 10000L,
                                               by = NULL,
                                               geom_qq_args = list(),
                                               geom_qq_line_args = list(),
                                               title = NULL,
                                               ggtheme = theme_bw(),
                                               theme_config = list(),
                                               nrow = 2L,
                                               ncol = 5L,
                                               parallel = FALSE
                                              ),
                           plot_bar_args = list(with = NULL,
                                                by = NULL,
                                                by_position = "fill",
                                                maxcat = 50,
                                                order_bar = TRUE,
                                                binary_as_factor = TRUE,
                                                title = NULL,
                                                ggtheme = theme_bw(),
                                                theme_config = list(),
                                                nrow = 2L,
                                                ncol = 5L,
                                                parallel = FALSE
                                               ),
                           plot_correlation_args = list(cor_args = list(use = "pairwise.complete.obs"), # nolint
                                                        type = c("all", "discrete", "continuous"), # nolint
                                                        maxcat = 100L,
                                                        geom_text_args = list(),
                                                        title = NULL,
                                                        ggtheme = theme_bw(),
                                                        theme_config = list(legend.position = "bottom", axis.text.x = element_text(angle = 90, size = 6)) # nolint
                                                       ), # nolint
                           plot_prcomp_args = list(variance_cap = 0.95,
                                                   maxcat = 100L,
                                                   prcomp_args = list(scale. = TRUE),
                                                   geom_label_args = list(),
                                                   title = NULL,
                                                   ggtheme = theme_bw(),
                                                   theme_config = list(text = element_text(size = 12)),
                                                   nrow = 1L,
                                                   ncol = 1L,
                                                   parallel = FALSE
                                                  ),
                           plot_boxplot_args = list(by = NULL,
                                                    binary_as_factor = TRUE,
                                                    geom_boxplot_args = list(),
                                                    scale_y = "continuous",
                                                    title = NULL,
                                                    ggtheme = theme_bw(),
                                                    theme_config = list(),
                                                    nrow = 2L,
                                                    ncol = 5L,
                                                    parallel = FALSE
                                                   ),
                           plot_scatterplot_args = list(by = NULL,
                                                        sampled_rows = 100000L, # nolint
                                                        geom_point_args = list(), # nolint
                                                        scale_x = NULL,
                                                        scale_y = NULL,
                                                        title = NULL,
                                                        ggtheme = theme_bw(),
                                                        theme_config = list(),
                                                        nrow = 2L,
                                                        ncol = 5L,
                                                        parallel = FALSE
                                                        ),
                           global_ggtheme = quote(theme_bw()),
                           global_theme_config = list()
                          )