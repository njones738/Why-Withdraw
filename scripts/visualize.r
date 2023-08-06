library(tidyverse)
library(visdat)
library(skimr)
library(DataExplorer)
library(lubridate)

options(pillar = 300, pillar.width = 120, digits = 5, scipen = 99999999)

# Opens up plot view
rerun_hgd <- function(x) {
    httpgd::hgd()
    httpgd::hgd_browse()
}
# rerun_hgd()

fold_pth <- "N:/Classes/2022_2FALL/Analytics Day/Code/"

agg_group_df_pth <- paste0(fold_pth, "agg_group_data.csv")
df_pth <- paste0(fold_pth, "output_results.csv")

df <- read_csv(df_pth) %>%
                    select(-...1)

df %>%
    select(where(is.numeric)) %>% 
    see_descript() %>% 
    view()

agg_group_df <- read_csv(agg_group_df_pth) %>%
                    select(-...1) %>%
                    mutate(AVGSTUDSENT_label = ifelse(AVGSTUDSENT < 0.5, "NEGATIVE", "POSITIVE")) # nolint


df %>%
    ggplot(aes(x = STUDSENT_label_rComments_tags, fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(position = "dodge", col = "grey") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                    )
                         ) +
        # scale_y_continuous(breaks = seq(0, 105000, 10000)) +
        labs(title = "Bar chart of Student Sentiment") +
        xlab("Quality Rating given by Reviewer") +
        ylab("Review Frequency") +
        theme_bw()

df %>%
    ggplot(aes(x = STUDSENT_label_rComments_tags, fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(position = "dodge", col = "grey") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                    )
                         ) +
        # scale_y_continuous(breaks = seq(0, 105000, 10000)) +
        labs(title = "Bar chart of Student Sentiment") +
        xlab("Quality Rating given by Reviewer") +
        ylab("Review Frequency") +
        facet_wrap(. ~ school_year) +
        theme_bw()

df %>%
    mutate(quality = factor(quality, levels = c("awful", "poor", "good", "average", "awesome"))) %>% # nolint
    ggplot(aes(x = quality, fill = STUDSENT_label_rComments_tags)) +
        geom_bar(position = "dodge", col = "grey") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                    )
                         ) +
        # scale_y_continuous(breaks = seq(0, 105000, 10000)) +
        labs(title = "Bar chart of Quality rating stratified by Student Sentiment") +
        xlab("Quality Rating given by Reviewer") +
        ylab("Review Frequency") +
        theme_bw()


df %>%
    ggplot(aes(x = as.character(rOverall), fill = STUDSENT_label_rComments_tags)) +
        geom_bar(position = "dodge", col = "grey") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                    )
                         ) +
        scale_y_continuous(breaks = seq(0, 105000, 10000)) +
        labs(title = "Bar chart of Overall rating stratified by Student Sentiment") +
        xlab("Overall Rating given by Reviewer") +
        ylab("Review Frequency") +
        theme_bw()

df %>%
    ggplot(aes(x = STUDSENT_score_rComments_tags, fill = STUDSENT_label_rComments_tags)) +
    geom_histogram(bins = 15, col = "grey") +
    scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                    "POSITIVE" = "#93BBA2"
                                )
                        ) +
    facet_wrap(. ~ school_year) +
    theme_bw()

df %>%
    ggplot(aes(y = STUDSENT_score_rComments_tags, x = as.numeric(str_sub(rDate, -4, -1)), col = STUDSENT_label_rComments_tags)) +
        geom_smooth() +
        facet_wrap(. ~ school_year) +
        theme_bw()





agg_group_df %>%
    ggplot(aes(x = WDRAW_ORIG_YR2_RT)) +
        geom_histogram(bins = 20, fill = "lavender", col = "grey") +
        theme_bw()
        

agg_group_df %>%
    ggplot(aes(x = WDRAW_ORIG_YR2_RT)) +
        geom_histogram(bins = 20, fill = "lavender", col = "grey") +
        facet_grid(AVGSTUDSENT_label ~ .) +
        theme_bw()

agg_group_df %>%
    ggplot(aes(x = as.numeric(str_sub(school_year, 1, 4)), y = WDRAW_ORIG_YR2_RT, col = AVGSTUDSENT_label)) + # nolint
        geom_smooth() +
        facet_grid(AVGSTUDSENT_label ~ .) +
        theme_bw()

agg_group_df %>%
    ggplot(aes(x = AVGSTUDSENT, y = WDRAW_ORIG_YR2_RT, col = AVGSTUDSENT_label)) + # nolint
        geom_smooth() +
        theme_bw()



agg_group_df %>%
    ggplot(aes(x = POSITIVE_count)) +
        geom_histogram(bins = 20, fill = "lavender", col = "grey") +
        theme_bw()


agg_group_df %>%
    mutate(count_ratio = NEGATIVE_count / (POSITIVE_count + NEGATIVE_count)) %>%
    ggplot(aes(x = count_ratio)) +
        geom_histogram(bins = 20, fill = "lavender", col = "grey") +
        theme_bw()


agg_group_df %>%
    ggplot(aes(x = AVGSTUDSENT_label, fill = AVGSTUDSENT_label)) +
        geom_bar() +
        theme_bw()



qqnorm(agg_group_df$WDRAW_ORIG_YR2_RT, pch = 1, frame = FALSE)
qqline(agg_group_df$WDRAW_ORIG_YR2_RT, col = "steelblue", lwd = 2)

shapiro.test(agg_group_df$WDRAW_ORIG_YR2_RT)


df %>% 
    select(clean_rComments_tags, STUDSENT_label_rComments_tags, STUDSENT_score_rComments_tags) %>%
    filter(STUDSENT_label_rComments_tags == "POSITIVE") %>%
    filter(STUDSENT_score_rComments_tags <= 0.75) %>%
    arrange(STUDSENT_score_rComments_tags)
    mutate(STUDSENT_score_rComments_tags = ifelse(STUDSENT_label_rComments_tags == "NEGATIVE", 1 + STUDSENT_score_rComments_tags, STUDSENT_score_rComments_tags))



mod <- lm(WDRAW_ORIG_YR2_RT ~ ., agg_group_df)


agg_group_df %>%
    group_by(OPEID, school_year) %>%
    count() %>%
    group_by(OPEID) %>%
    count() %>%
    arrange(n) %>%
    filter(n >= 3) %>%
    print(n = Inf)


agg_group_df %>%
    group_by(OPEID, school_year) %>%
    count() %>%
    group_by(OPEID) %>%
    count() %>%
    arrange(n) %>%
    filter(n >= 3) %>%
    print(n = Inf)

