library(tidyverse)
library(stats)
library(perm)
library(patchwork)
library(jmuOutlier)
library(agricolae)

options(pillar.width = 240, scipen = 999999)
source("N:/Classes/2022_2FALL/Analytics Day/Code/usable_funcs.r")

# Since the result of the shapiro-wilk normality test is less than 0.05,
#   there is evidence to conclude the data is not normally distributed.
test_normality_test_2variable <- function(group1, group2, group1_name = "Variable 1", group2_name = "Variable 2", prt_plot = 0) { # nolint
        g1 <- shapiro.test(group1)
        g2 <- shapiro.test(group2)

        shapiro_test_res_str_g1 <- prt_result(paste0("Shapiro-Wilks Normality test (", group1_name, ") "), # nolint
                                              g1$p.value, p_value = p, # nolint
                                              "there is not enough evidence to conclude the data may not be from a normally distributed population.", # nolint
                                              "there is enough evidence to conclude the data may not be from a normally distributed population.") # nolint
        print(shapiro_test_res_str_g1)
        shapiro_test_res_str_g2 <- prt_result(paste0("Shapiro-Wilks Normality test (", group2_name, ") "), # nolint
                                              g2$p.value, p_value = p, # nolint
                                              "there is not enough evidence to conclude the data may not be from a normally distributed population.", # nolint
                                              "there is enough evidence to conclude the data may not be from a normally distributed population.") # nolint
        print(shapiro_test_res_str_g2)

        if(prt_plot != 0) {
            qq1 <- ggplot(tibble(var = group1), aes(sample = var)) +
                stat_qq() +
                stat_qq_line() +
                labs(title = paste0("Q-Q plot for Group 1: ", group1_name)) +
                xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
                theme_bw()
            qq2 <- ggplot(tibble(var = group2), aes(sample = var)) +
                stat_qq() +
                stat_qq_line() +
                labs(title = paste0("Q-Q plot for Group 2: ", group2_name)) +
                xlab("Theoretical Quantiles") + ylab(NULL) +
                theme_bw()
            print((qq1 | qq2))
        }

        res <- bind_rows(tibble(test = paste0(g1$method, " - Group 1: ", group1_name), # nolint
                                group_p_value = g1$p.value,
                                group_stat = g1$statistic,
                                results = shapiro_test_res_str_g1),
                         tibble(test = paste0(g2$method, " - Group 2: ", group2_name), # nolint
                                group_p_value = g2$p.value,
                                group_stat = g2$statistic,
                                results = shapiro_test_res_str_g2))
        return(res)
}


# H0: There is not enough evidence to conclude that the shape, center, and spread of the two distributions are different. # nolint
# Ha: There is enough evidence to conclude that that the shape, center, and spread of the two distributions are different. # nolint
test_ks_test <- function(group1, group2, test_type = "two.sided", p = 0.05) { # nolint 
        ks_test <- stats::ks.test(group1, group2,
                                  alternative = test_type)
        ks_test_res_str <- prt_result("ks-test", ks_test$p.value, p_value = p, # nolint
                                      "there is not enough evidence to conclude that the shape, center, and spread of the two distributions are different.", # nolint
                                      "there is enough evidence to conclude that that the shape, center, and spread of the two distributions are different.") # nolint
        print(ks_test_res_str)

        return(tibble(p_value = ks_test$p.value,
                      statistic = ks_test$statistic,
                      results = ks_test_res_str))
}


# H0: There is not enough evidence to conclude that the variation in the two groups are different. # nolint
# Ha: There is enough evidence to conclude that the variation in the two groups are different. # nolint
test_ratio_meandiff <- function(group1, group2, test_type = "two.sided", iters = 1000, p = 0.05) { # nolint 
        ratio_meandiff <- my_rmd_test(group1, group2,
                                      direction = test_type,
                                      nsamp = iters)
        ratio_meandiff_res_str <- prt_result("Ratio of Mean Difference test", ratio_meandiff$p.value, p_value = p, # nolint
                                             "there is not enough evidence to conclude that the variation in the two groups are different.", # nolint
                                             "there is enough evidence to conclude that the variation in the two groups are different.") # nolint
        print(ratio_meandiff_res_str)

        return(tibble(p_value = ratio_meandiff$p.value,
                      statistic = ratio_meandiff$tst.stat,
                      results = ratio_meandiff_res_str, # nolint
                      samp1_devs = ratio_meandiff$samp1.devs, # nolint
                      samp2_devs = ratio_meandiff$samp2.devs, # nolint
                      direction = ratio_meandiff$tst.direction))
}


# H0: There is not enough evidence to conclude that the distributions of the ranked data have different centers. # nolint 
# Ha: There is enough evidence to conclude that the distributions of the ranked data have different centers. # nolint 
test_wilcox_test <- function(group1, group2, test_type = "two.sided", clvl = 0.95, p = 0.05) { # nolint 
        wilcox_test <- stats::wilcox.test(group1, group2,
                                          alternative = test_type,
                                          conf.int = T, conf.level = clvl)
        wilcox_test_res_str <- prt_result("Wilcox Rank Sum test", wilcox_test$p.value, p_value = p, # nolint
                                          "there is not enough evidence to conclude that the distributions of the ranked data have different centers.", # nolint
                                          "there is enough evidence to conclude that the distributions of the ranked data have different centers.") # nolint
        print(wilcox_test_res_str)

        return(tibble(p_value = wilcox_test$p.value,
                      statistic = wilcox_test$statistic,
                      results = wilcox_test_res_str,
                      parameter = wilcox_test$parameter, # nolint
                      estimate = wilcox_test$estimate)) # nolint
}


# H0: There is not enough evidence to conclude that there is a difference between the means of the two groups. # nolint
# Ha: There is enough evidence to conclude that there is a difference between the means of the two groups. # nolint
test_ttst_2variable <- function(group1, group2, test_type = "two.sided", clvl = 0.95, p = 0.05) { # nolint
        ttst <- stats::t.test(group1, group2,
                              alternative = test_type,
                              mu = 0, paired = F,
                              var.equal = F, conf.level = clvl)
        ttst_res_str <- prt_result("t-test", ttst$p.value, p_value = p,
                                   "there is not enough evidence to conclude that there is a difference between the means of the two groups.", # nolint
                                   "there is enough evidence to conclude that there is a difference between the means of the two groups.") # nolint
        print(ttst_res_str)

        return(tibble(p_value = ttst$p.value,
                      statistic = ttst$statistic,
                      results = ttst_res_str,
                      parameter = ttst$parameter, # nolint
                      x_mean = ttst$estimate[1],
                      y_mean = ttst$estimate[2],
                      std_error = ttst$stderr)) # nolint
}


# H0: There is not enough evidence to conclude that the distributions are different. # nolint
# Ha: There is enough evidence to conclude that the distributions are different. # nolint
test_perm_mean <- function(group1, group2, test_type = "two.sided", iters = 1000, my_seed = 59914120, p = 0.05) { # nolint
        perm_Mean <- perm::permTS(group1, group2,
                                  stat = mean,
                                  method = "exact.mc",
                                  alternative = test_type,
                                  control = permControl(nmc = iters, seed = my_seed)) # nolint
        perm_Mean_res_str <- prt_result("Permutation Test on the Difference in Means", perm_Mean$p.value, p_value = p, # nolint
                                        "there is not enough evidence to conclude that the distributions are different.", # nolint
                                        "there is enough evidence to conclude that the distributions are different.") # nolint
        print(perm_Mean_res_str)

        return(tibble(p_value = perm_Mean$p.value,
                      statistic = perm_Mean$estimate,
                      results = perm_Mean_res_str,
                      parameter = perm_Mean$parameter, # nolint
                      estimate = perm_Mean$statistic,
                      null_value = perm_Mean$null.value,
                      p_twosided = perm_Mean$p.values[1],
                      p_twosidedAbs = perm_Mean$p.values[2],
                      p_lte = perm_Mean$p.values[3],
                      p_gte = perm_Mean$p.values[4],
                      p.equal = perm_Mean$p.values[5])) # nolint
}


# H0: There is not enough evidence to conclude that the distributions are different. # nolint
# Ha: There is enough evidence to conclude that the distributions are different. # nolint
test_perm_median <- function(group1, group2, prt_plot = 0, test_type = "two.sided", iters = 1000, p = 0.05) { # nolint
        perm_median <- jmuOutlier::perm.test(group1, group2,
                                             num.sim = iters,
                                             alternative = test_type,
                                             paired = F, stat = median,
                                             plot = F, all.perms = T)
        perm_median_res_str <- prt_result("Permutation Test on the Difference in Medians", perm_median$p.value, p_value = p, # nolint
                                          "there is not enough evidence to conclude that the distributions are different.", # nolint
                                          "there is enough evidence to conclude that the distributions are different.") # nolint
        print(perm_median_res_str)

        if(prt_plot != 0) {
            plot_median <- jmuOutlier::perm.test(group1, group2,
                                                 num.sim = iters,
                                                 alternative = test_type,
                                                 stat = median, paired = F,
                                                 plot = T, all.perms = T)
            tst_stat <- median(group2) - median(group1)
            tibble(breaks = plot_median$breaks, counts = c(0, plot_median$counts)) %>% # nolint
                mutate(percent = counts / sum(counts)) %>%
                ggplot(., aes(x = breaks, y = percent)) +
                    scale_x_continuous(breaks = seq(-500, 500, 0.01)) +
                    geom_vline(xintercept = 0, col = "red", lwd = 1) +
                    geom_hline(yintercept = 0, col = "black", lwd = 0.25, linetype = "dashed") + # nolint
                    geom_col(fill = "lavender", col = "black") +
                    geom_vline(xintercept = c(-1, 1) * tst_stat, # nolint
                            col = "blue", lwd = 0.75, linetype = "dotdash") +
                    xlab("Difference in Medians") + ylab("Proportion of Test Statistics") + # nolint
                    labs(title = paste0("The Proportion of Test Statistics by the Difference in Medians -- Test Statistic:", round(tst_stat, 4))) + # nolint
                    theme_bw()
        }
        return(tibble(p_value = perm_median$p.value,
                      statistic = perm_median$mu,
                      results = perm_median_res_str)) # nolint
}


# H0: There is not enough evidence to conclude that the centers are the same.
# Ha: There is enough evidence to conclude that the centers are different.
test_perm_trim <- function(group1, group2, prt_plot = 0, iters = 1000, t = 0.10, test_type = "two.sided", p = 0.05) { # nolint
        ts <- paste0("Permutation Test on the Difference in Trimmed Means (trim = ", t, ") ") # nolint
        perm_trim <- jmuOutlier::perm.test(group1, group2,
                                           num.sim = iters,
                                           trim = t,
                                           alternative = test_type,
                                           stat = mean, plot = F,
                                           all.perms = T, paired = F)
        perm_trim_res_str <- prt_result(ts, perm_trim$p.value, p_value = p, # nolint
                                        "there is not enough evidence to conclude that the centers are the same.", # nolint
                                        "there is enough evidence to conclude that the centers are different.") # nolint
        print(perm_trim_res_str)

        if(prt_plot != 0) {
            plot_trim <- jmuOutlier::perm.test(group1, group2,
                                               num.sim = iters,
                                               trim = t,
                                               alternative = test_type,
                                               all.perms = T, stat = mean,
                                               paired = F, plot = T)
            tst_stat <- mean(group2) - mean(group1) # nolint
            trim_tst_stat <- mean(group2, trim = t) - mean(group1, trim = t) # nolint
            tibble(breaks = plot_trim$breaks, counts = c(0, plot_trim$counts)) %>% # nolint
                mutate(percent = counts / sum(counts)) %>%
                ggplot(., aes(x = breaks, y = percent)) +
                    scale_x_continuous(breaks = seq(-500, 500, 0.01)) +
                    geom_vline(xintercept = 0, col = "red", lwd = 1) +
                    geom_hline(yintercept = 0, col = "black", lwd = 0.25, linetype = "dashed") + # nolint
                    geom_col(fill = "lavender", col = "black") +
                    geom_vline(xintercept = c(-1, 1) * trim_tst_stat, # nolint
                            col = "blue", lwd = 0.75, linetype = "dotdash") +
                    geom_vline(xintercept = c(-1, 1) * tst_stat, # nolint
                            col = "#b157e9", lwd = 0.25, linetype = "dotdash") +
                    xlab("Difference in Medians (BLUE LINE = Trim Mean - PURPLE LINE = Mean)") + ylab("Proportion of Test Statistics") + # nolint
                    labs(title = paste0("The Proportion of Test Statistics by the Difference in Trimmed Means (trim = ", t, ")", " -- Test Statistic:", round(tst_stat, 4))) + # nolint
                    theme_bw()
        }
        return(tibble(p_value = perm_trim$p.value,
                      statistic = perm_trim$mu,
                      results = perm_trim_res_str)) # nolint
}


test_normality_test_1variable <- function(vari, p = 0.05) {
        shap_wilk <- shapiro.test(vari)
        shapiro_test_res_str <- prt_result("Shapiro-Wilks Normality test", # nolint
                                           shap_wilk$p.value, p_value = p, # nolint
                                           "there is not enough evidence to conclude the data may not be from a normally distributed population.", # nolint
                                           "there is enough evidence to conclude the data may not be from a normally distributed population.") # nolint
        print(shapiro_test_res_str)

        return(tibble(test = shap_wilk$method, # nolint
                      p_value = shap_wilk$p.value,
                      statistic = shap_wilk$statistic,
                      results = shapiro_test_res_str))
}


test_ratio_maxmin_std <- function(quant, categ, p = 2.05) {
        ratio_maxmin_sd <- Ratio_of_MaxToMin_SD(quant = quant, categ = categ)
        ratio_maxmin_sd_test_res_str <- prt_result("ratio of max to min standard deviations", ratio_maxmin_sd$p.value, p_value = p, # nolint
                                        "there is no evidence that the data has violated the assumption of equal variance.", # nolint
                                        "there is evidence that the data has violated the assumption of equal variance.") # nolint
        return(tibble(test = "ratio max to min std deviations", ratio_maxmin_sd, results = ratio_maxmin_sd_test_res_str)) # nolint
}




test_center_1variable <- function(group, test_type = "two.sided", clvl = 0.95, pp = 0.5, p = 0.05) { # nolint
        ttst <- t.test(group,
                       alternative = test_type,
                       paired = F,
                       mu = median(group),
                       var.equal = F,
                       conf.level = clvl)
        ttst_res_str <- prt_result("t-test", ttst$p.value, p_value = p,
                                   "there is not enough evidence to conclude that there is a difference between the mean and median value.", # nolint
                                   "there is enough evidence to conclude that there is a difference between the mean and median value.") # nolint

        Hypo_val <- mean(group1) # nolint
        order_vari <- sort(group1) # nolint

        nonPara_tst <- sum(group1 > Hypo_val)
        n_size <- sum(!is.na(group1)) # nolint

        binom_test <- binom.test(x = nonPara_tst,
                                 n = n_size,
                                 p = pp,
                                 alternative = test_type,
                                 conf.level = clvl)

        binom_test_res_str <- prt_result("binomial test", binom_test$p.value, p_value = p, # nolint
                                        "there is not enough evidence to conclude that there is a difference between the mean and median value.", # nolint
                                        "there is enough evidence to conclude that there is a difference between the mean and median value.") # nolint

        return(bind_rows(tibble(test = "binomial test",
                                p_value = binom_test$p.value,
                                statistic = binom_test$statistic,
                                results = binom_test_res_str,
                                parameter = binom_test$parameter, # nolint
                                x_mean = binom_test$estimate[1],
                                std_error = binom_test$stderr),
                         tibble(test = "t-test",
                                p_value = ttst$p.value,
                                statistic = ttst$statistic,
                                results = ttst_res_str,
                                parameter = ttst$parameter, # nolint
                                x_mean = ttst$estimate[1],
                                std_error = ttst$stderr)))
}



test_battery_2variable <- function(group1, group2, g1_name = "Variable 1", g2_name = "Variable 2", test_type = "two.sided", iters = 1000, p = 0.05, t = 0.10, clvl = 0.95, my_seed = 59914120, prt_plot = 0) { # nolint
    res_normality_test <- test_normality_test_2variable(group1, group2,
                                                        group1_name = g1_name,
                                                        group2_name = g2_name,
                                                        prt_plot = prt_plot)
    res_ks_test <- test_ks_test(group1, group2,
                                test_type = test_type,
                                p = p)
    res_ratio_meandiff <- test_ratio_meandiff(group1, group2,
                                            test_type = test_type,
                                            iters = iters,
                                            p = p)
    res_wilcox_test <- test_wilcox_test(group1, group2,
                                        test_type = test_type,
                                        clvl = clvl,
                                        p = p)
    res_ttst <- test_ttst_2variable(group1, group2,
                        test_type = test_type,
                        clvl = clvl,
                        p = p)
    res_perm_mean <- test_perm_mean(group1, group2,
                                    test_type = test_type,
                                    iters = iters,
                                    my_seed = my_seed,
                                    p = p)
    res_perm_median <- test_perm_median(group1, group2,
                                        prt_plot = prt_plot,
                                        test_type = test_type,
                                        iters = iters,
                                        p = p)
    res_perm_trim <- test_perm_trim(group1, group2,
                                    prt_plot = prt_plot,
                                    iters = iters,
                                    t = t,
                                    test_type = test_type,
                                    p = p)
    return(bind_rows(res_normality_test %>% select(p_value = group_p_value,
                                                   statistic = group_stat,
                                                   results, test), # nolint
                     res_ks_test %>% select(p_value, statistic, results) %>%
                                     mutate(test = "ks-test"), # nolint
                     res_ratio_meandiff %>% select(p_value, statistic, results) %>% # nolint
                                            mutate(test = "ratio_mean_differences"), # nolint
                     res_wilcox_test %>% select(p_value, statistic, results) %>%
                                         mutate(test = "wilcox rank sum test"), # nolint
                     res_ttst %>% select(p_value, statistic, results) %>%
                                  mutate(test = "t-test"), # nolint
                     res_perm_mean %>% select(p_value, statistic, results) %>%
                                       mutate(test = "permutation test on mean difference"), # nolint
                     res_perm_median %>% select(p_value, statistic, results) %>%
                                         mutate(test = "permutation test on median difference"), # nolint
                     res_perm_trim %>% select(p_value, statistic, results) %>%
                                       mutate(test = "permutation test on trimmed mean difference")) %>% # nolint
                relocate(test, .before = p_value))
}



test_battery_1variable <- function(group, test_type = "two.sided", clvl = 0.95, pp = 0.5, p = 0.05) { # nolint
        res_norm <- test_normality_test_1variable(group, p = p) %>% select(test, p_value, statistic, results) # nolint
        res_center <- test_center_1variable(group,
                                            test_type = test_type,
                                            clvl = clvl,
                                            pp = pp,
                                            p = p) %>%
                        select(test, p_value, statistic, results)
        return(bind_rows(res_norm, res_center)) # nolint
}



test_normality_center_1cat1quant <- function(group1, group3, test_type = "two.sided", clvl = 0.95, pp = 0.5, p = 0.05) { # nolint
        hodl <- tibble(group1, group3) %>%
                    group_by(group3) %>%
                    count()

        temp <- tibble(category = NA_character_,
                    test = NA_character_,
                    p_value = NA_real_,
                    statistic = NA_real_,
                    results = NA_character_)

        for(i in hodl$group3) {
            hodl2 <- tibble(group1, group3) %>% filter(group3 == i)
            res <- test_battery_1variable(hodl2$group1,
                                        test_type = test_type,
                                        clvl = clvl,
                                        pp = pp,
                                        p = p)
            temp <- bind_rows(temp, tibble(category = i, res))
        }

        return(temp %>% filter(!is.na(category)))
}




