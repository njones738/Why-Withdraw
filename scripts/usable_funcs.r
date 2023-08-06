library(SciViews)

# clean up garbage
clean_garbage <- function(obj_lst) {
    rm(obj_lst)
    gc()
}

# Opens up plot view
rerun_hgd <- function(x) {
    httpgd::hgd()
    httpgd::hgd_browse()
}

# returns description of all variables in dataframe
see_descript <- function(df) {
        d1 <- as_tibble(lapply(df, function(x) min(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "min"
                       )
        d2 <- as_tibble(lapply(df, function(x) quantile(x, 1/10, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "pct10"
                       )
        d3 <- as_tibble(lapply(df, function(x) quantile(x, 1/4, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "pct25"
                       )
        d4 <- as_tibble(lapply(df, function(x) median(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "median"
                       )
        d5 <- as_tibble(lapply(df, function(x) mean(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "mean"
                       )
        d6 <- as_tibble(lapply(df, function(x) quantile(x, 3/4, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "pct75"
                       )
        d7 <- as_tibble(lapply(df, function(x) quantile(x, 9/10, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "pct90"
                       )
        d8 <- as_tibble(lapply(df, function(x) max(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "max"
                       )
        d9 <- as_tibble(lapply(df, function(x) sd(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "std_dev"
                       )
        d10 <- as_tibble(lapply(df, function(x) mad(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "mad"
                       )
        d11 <- as_tibble(lapply(df, function(x) var(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "variance"
                       )
        d12 <- as_tibble(lapply(df, function(x) n_distinct(x, na.rm = T))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "n_distinct")
        dn <- as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
                    pivot_longer(cols = everything(),
                                 names_to = "Variable",
                                 values_to = "n"
                       ) %>%
                    mutate(nmiss = max(n) - n,
                           n_pct = n / max(n),
                           nmiss_pct = nmiss / max(n),
                           percent_missing = case_when(nmiss_pct >= 1 ~ "100%",
                           nmiss_pct >= 0.9 ~ "90%",
                           nmiss_pct >= 0.8 ~ "80%",
                           nmiss_pct >= 0.7 ~ "70%",
                           nmiss_pct >= 0.6 ~ "60%",
                           nmiss_pct >= 0.5 ~ "50%",
                           nmiss_pct >= 0.4 ~ "40%",
                           nmiss_pct >= 0.3 ~ "30%",
                           nmiss_pct >= 0.2 ~ "20%",
                           nmiss_pct >= 0.1 ~ "10%",
                           nmiss_pct < 0.1 ~ "0%"),
                           percent_missing = factor(percent_missing,
                                                    levels = c("0%", "10%", "20%", "30%", "40%", "50%", # nolint
                                                               "60%", "70%", "80%", "90%", "100%"))) # nolint


        df <- left_join(dn, d1, by = "Variable") %>%
                left_join(., d2, by = "Variable") %>%
                left_join(., d3, by = "Variable") %>%
                left_join(., d4, by = "Variable") %>%
                left_join(., d5, by = "Variable") %>%
                left_join(., d6, by = "Variable") %>%
                left_join(., d7, by = "Variable") %>%
                left_join(., d8, by = "Variable") %>%
                left_join(., d9, by = "Variable") %>%
                left_join(., d10, by = "Variable") %>%
                left_join(., d11, by = "Variable") %>%
                left_join(., d12, by = "Variable")
        return(df)
}

# Returns a sample of that is imputed from a specific range
## s1 <- for the lower range; s2 <- for the middle range; s3 <- for the upper range;
get_s1 <- function(x, lb = 0.0001, s = 0.01) {
    lbs <- lb + s
    le01 <- sum(ifelse(!is.na(x), ifelse(x < quantile(x, lb, na.rm = T), 1, 0), 0), na.rm = T) # nolint
    step_size <- s / le01 # lb - lbs = lb - (lb + s) = s # nolint
    s1 <- sample(quantile(sort(x), seq(lb, lbs, step_size), na.rm = T, names = F), le01, replace = T) # nolint
    return(sort(s1))
}
get_s2 <- function(x, lb = 0.0001, ub = 0.9999, s = 0.15) {
    lbs <- lb + s
    ubs <- ub - s
    ublb <- ub - lb
    msn <- sum(is.na(x))
    step_size <- ublb / msn
    s2 <- sample(quantile(sort(x), seq(lbs, ubs, step_size), na.rm = T, names = F), msn, replace = T) # nolint
    return(s2)
}
get_s3 <- function(x, ub = 0.9999, s = 0.1) {
    ubs <- ub - s
    ge99 <- sum(ifelse(!is.na(x), ifelse(x > quantile(x, ub, na.rm = T), 1, 0), 0), na.rm = T) # nolint
    step_size <- s / ge99 # ub - ubs = ub - (ub + s) = s # nolint
    s3 <- sample(quantile(sort(x), seq(ubs, ub, step_size), na.rm = T, names = F), ge99, replace = T) # nolint
    return(sort(s3))
}

# returns the value k standard deviation steps from the mean
get_bound <- function(x, k = 6, b = "ub") {
    mu <- mean(x, na.rm = T)
    std_dev <- sd(x, na.rm = T)

    if (b == "lb") {
        return(mu - (k * std_dev))
    }
    if (b == "ub") {
        return(mu + (k * std_dev))
    }
}

# returns the count of observed and missing values from a dataframe
see_missing <- function(df) {
    as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable",
                         values_to = "n") %>%
            mutate(nmiss = max(n, na.rm = T) - n,
                   n_pct = n / max(n, na.rm = T),
                   nmiss_pct = nmiss / max(n, na.rm = T))
                            }

# Projects an extreme point
get_one <- function(x, mu, s, a = 4, b = 6.5) {
    k_step <- (x - mu) / s
    ka_pct <- 1 - (1 / (a^(2)))
    k_pct <- 1 - (1 / (k_step^(2)))
    d <- (k_pct - ka_pct)
    dd <- (1 / (d^(1 / 2)))
    scala <- (b - dd) + a
    y <- (scala * s) + mu
    return(y)
}
# get_one(2317866, mean(samp1$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(1203471, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(998047, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(990508, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(901871, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(837028, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(250000, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))
# get_one(208944.7, mean(samp$TSHIC, na.rm = T), sd(samp$TSHIC, na.rm = T))


# get probability of default, weight of evidence, information value, and a nice plot
get_prob <- function(df, nn, s, sz, r = 4) {  # get_prob(., "TOPENB75", s = 1, sz = 1)
      df2 <- df %>%
            summarise(count = n()) %>%
            pivot_wider(names_from = goodbad, values_from = count) %>%
            rename(good = "0", bad = "1") %>%
            mutate(n = good + bad,
                   pct_good = good / sum(good),
                   pct_bad = bad / sum(bad),
                   woe = ln(pct_good / pct_bad),
                   iv = (pct_good - pct_bad) * woe,
                   total_iv = sum(iv),
                   Bin_pct = n / sum(n),
                   cum_sum = accumulate(Bin_pct, sum),
                   prob_dflt = bad / n
                  ) %>%
            select(contains("ORD_"), n, good, bad,
                  Bin_pct, cum_sum, prob_dflt,
                  woe, iv, total_iv)

      mx_pd <- max(df2$prob_dflt)
      mn_pd <- min(df2$prob_dflt)
      mx_ov <- max(df2$ORD_var)
      mn_ov <- min(df2$ORD_var)
      l <- length(df2$ORD_var)

      print(df2 %>%
            select(x = contains("ORD"), everything()) %>%
            ggplot(., aes(x = x, y = prob_dflt)) +
                  geom_point() +
                  geom_line(col = "#4266df") +
                  labs(title = paste0("The Probability of Default by ", nn)) +
                  xlab(nn) + ylab("The Probability of Default") +
                  scale_y_continuous(breaks = df2$prob_dflt,
                                     labels = round(df2$prob_dflt, r),
                                     limits = c(mn_pd, mx_pd) # nolint
                                    ) +
                  scale_x_continuous(breaks = seq(mn_ov, mx_ov, s), # nolint
                                     limits = c(mn_ov, mx_ov)) + # nolint
                  geom_segment(aes(x = x, y = c(prob_dflt[2:l], 0),
                                   xend = x, yend = prob_dflt),
                                   col = "#8601AF",
                                   linetype = "dotdash", size = 1) +
                  geom_segment(aes(x = x, y = prob_dflt,
                                   xend = x + 1, yend = prob_dflt),
                                   col = "#8601AF",
                                   linetype = "dotdash", size = 1) +
                  geom_segment(aes(x = x, y = c(prob_dflt[2:l], 0),
                                   xend = x + 1, yend = c(prob_dflt[2:l], 0)),
                                   col = "#d1d100",
                                   linetype = "dashed", size = 0.5) +
                  geom_segment(aes(x = x, y = c(0, prob_dflt[1:l-1]),
                                   xend = x, yend = prob_dflt),
                                   col = "#0247FE",
                                   linetype = "dashed", size = 0.5) +
                  theme_bw())
      rm("mx_pd", "mn_pd", "mx_ov", "mn_ov", "l")
      return(df2)
}


pr <- function(df, r = 50) {
    return(df %>% print(n = r))
}

get_clustLst <- function(df, k) {
    clust <- df %>%
        filter(NumberOfClusters == k) %>%
        group_by(NumberOfClusters, Cluster) %>%
        arrange(RSquareRatio, .by_group = T) %>%
        distinct(Cluster, .keep_all = T) %>%
        select(Variable)
    return(tibble(Variable = clust$Variable))
}

get_profit <- function(TP, TN, FP, FN, TPG = 0, TNG = 250, FPG = 0, FNG = -750) {
  return((TP * TPG) + (TN * TNG) + (FP * FPG) + (FN * FNG))
}

ratio_of_MaxToMin_SD <- function(quant, categ, prnt = 1) { # nolint
    temp <- tapply(quant, categ, FUN = sd)
    ratio <- round(max(temp, na.rm = T) / min(temp, na.rm = T), 2)
    if(prnt == 1) {
        print("The standard deviations of the quantitative variable for the different categorical levels are:") # nolint
        print(temp)
        print(paste("The ratio of the largest standard deviation to the smallest standard deviation is:", ratio)) # nolint
    } else { return(temp) }
}

my_rmd_test <- function(samp1, samp2, direction = c('two.sided','less','greater')[1], nsamp = 10000) { # nolint
              devs1 <- samp1 - median(samp1)
              devs2 <- samp2 - median(samp2)
              devs <- c(devs1, devs2)

              RMD <- mean(abs(devs1)) / mean(abs(devs2))
              if (direction[1] == 'two.sided') { # nolint
                  RMD <- max(1 / RMD, RMD)
              }
              RMDperms <- rep(NA, nsamp)


              for (i in 1:nsamp) {
                  tempdevs <- devs[sample(length(devs), length(devs), replace = F)] # nolint

                  RMDperms[i] <- mean(abs(tempdevs[1:length(devs1)])) / mean(abs(tempdevs[-(1:length(devs1))])) # nolint
                  if (direction[1] == 'two.sided') RMDperms[i] <- max(1 / RMDperms[i], RMDperms[i]) # nolint
              }
              if (direction[1] == 'greater') p.value <- mean(RMDperms >= RMD) # nolint
              if (direction[1] == 'less') p.value <- mean(RMDperms <= RMD) # nolint
              if (direction[1] == 'two.sided') p.value <- mean(RMDperms >= RMD) # nolint

              samp1.devs <- round(mean(abs(devs1)), 2) # nolint
              samp2.devs <- round(mean(abs(devs2)), 2) # nolint
              tst.direction <- direction[1] # nolint
              tst.stat <- round(RMD, 4) # nolint

              return(tibble(samp1.devs, samp2.devs, tst.direction, tst.stat, p.value))
}

Ratio_of_MaxToMin_SD <- function(quant, categ) {
  temp <- as_tibble_row(tapply(quant, categ, FUN = sd)) # nolint
  ratio <- round(max(temp) / min(temp), 2)
  return(bind_cols(tibble(ratio), temp))
}


prt_result <- function(test, pval, hnot, haye, p_value = 0.05) {
    if(pval < p_value) {
        r1 <- "less"
        r2 <- haye
    } else {
        r1 <- "greater"
        r2 <- hnot
    }
    return(paste0("Since the result of the ", test, " is ", r1, " than ", p_value, ", ", r2)) # nolint
}








see_counts <- function(df) {
    df %>%
       count() %>%
       arrange(desc(n)) %>%
       print(n = Inf)
}
