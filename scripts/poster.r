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

fold_pth <- "N:/Classes/2022_2FALL/Analytics Day/"
data_pth <- paste0(fold_pth, "Code/", "rmp_df.csv")
output_pth <- paste0(fold_pth, "Data/", "output_df.csv")

image_fold_pth <- "N:/Classes/2022_2FALL/Analytics Day/poster/images/"

##################################################################################################
##################################################################################################

output_df <- read_csv(output_pth,
                      col_types = c("OPEID" = "c",
                                    "school_sid" = "c",
                                    "professor_tid" = "c",
                                    "onlineClass" = "c",
                                    "rClarity" = "c",
                                    "rEasy" = "c",
                                    "rHelpful" = "c",
                                    "rTextBookUse" = "c")
                     ) %>%
                select(-...1) %>%
                filter(!(school_sid %in% c("1774", "4007", "4683", "12856", "4374", "5027", "5164", "17133"))) %>% # nolint
                filter(school_year != "2000_01") %>%
                mutate(
                       SY_ind = case_when(school_year == "2001_02" ~ 0,
                                          school_year == "2002_03" ~ 1,
                                          school_year == "2003_04" ~ 2,
                                          school_year == "2004_05" ~ 3,
                                          school_year == "2005_06" ~ 4,
                                          school_year == "2006_07" ~ 5,
                                          school_year == "2007_08" ~ 6,
                                          school_year == "2008_09" ~ 7,
                                          school_year == "2009_10" ~ 8,
                                          school_year == "2010_11" ~ 9,
                                          school_year == "2011_12" ~ 10,
                                          school_year == "2012_13" ~ 11,
                                          school_year == "2013_14" ~ 12,
                                          school_year == "2014_15" ~ 13,
                                          school_year == "2015_16" ~ 14,
                                          school_year == "2016_17" ~ 15,
                                          school_year == "2017_18" ~ 16,
                                          school_year == "2018_19" ~ 17),
                       STUDSENT_score_comments_tags = case_when(STUDSENT_label_comments_tags == "NEGATIVE" ~ 1 - STUDSENT_score_comments_tags, # nolint
                                                                T ~ STUDSENT_score_comments_tags) # nolint
                      ) 

group_output_df <- output_df %>%
    group_by(school_sid, INSTNM, professor_tid, school_year, SY_ind) %>% # nolint
    summarise(
        n = n(),
        WDRAW_ORIG_YR2_RT = mean(WDRAW_ORIG_YR2_RT),
        AVGOVRALLRATING_prof = median(rOverall),
        AVGSTUDSENT_prof = median(STUDSENT_score_comments_tags)
    ) %>%
    ungroup() %>%
    group_by(school_sid, school_year, SY_ind) %>%
    summarise(
        num_reviews = n(),
        WDRAW_ORIG_YR2_RT = mean(WDRAW_ORIG_YR2_RT),
        AVGOVRALLRATING = median(AVGOVRALLRATING_prof),
        AVGSTUDSENT = median(AVGSTUDSENT_prof)
    ) %>%
    ungroup() %>% unique()

merge_group_output_df <- left_join(
    group_output_df,
    output_df %>%
        group_by(school_sid, professor_tid, school_year, STUDSENT_label_comments_tags) %>% # nolint
        summarise(
            n = n(),
            AVGOVRALLRATING_prof = median(rOverall),
            AVGSTUDSENT_prof = median(STUDSENT_score_comments_tags)
        ) %>%
        ungroup() %>%
        group_by(school_sid, school_year, STUDSENT_label_comments_tags) %>%
        summarise(
            num_reviews = sum(n),
            AVGOVRALLRATING = median(AVGOVRALLRATING_prof),
            AVGSTUDSENT = median(AVGSTUDSENT_prof)
        ) %>%
        ungroup() %>%
        group_by(school_sid, school_year) %>%    
        pivot_wider(
            names_from = STUDSENT_label_comments_tags,
            values_from = c(num_reviews, AVGOVRALLRATING, AVGSTUDSENT),
            values_fill = 0,
            names_sep = "_"
        ) %>% unique(),
    by = c("school_sid", "school_year")
    )

# merge_group_output_df %>% write_csv("N:/Classes/2022_2FALL/Analytics Day/Code/group_output_df4.csv")



output_df %>%
    mutate(rOverall = as.character(rOverall)) %>%
    mutate(STUDSENT_LABEL = case_when(
                                         STUDSENT_score_comments_tags <= 0.25 ~ "NEGATIVE",
                                         STUDSENT_score_comments_tags > 0.25 ~ "POSITIVE",
                                         T ~ "NEUTRAL")
          ) %>%
    ggplot(aes(x = rOverall, fill = STUDSENT_LABEL)) +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "NEUTRAL" = "grey",
                                     "POSITIVE" = "#93BBA2"
                                )
                        ) +
        geom_bar(position = "dodge") +
        theme_bw()

merge_group_output_df %>%
    ggplot(aes(x = SY_ind, group = as.character(school_sid), fill = as.character(school_sid))) +
        geom_smooth(
                 aes(y = AVGSTUDSENT),
                 se = F,
                 show.legend = F
                ) +
        theme_bw()




##################################################################################################
##################################################################################################

# Figure 1: Review Count Per School Per School Year
f1 <- output_df %>%
    group_by(school_year, INSTNM, school_sid) %>%
    count() %>% ungroup() %>%
    filter(INSTNM == "Kennesaw State University")

fig1 <- output_df %>%
    group_by(school_year) %>%
    count() %>%
    ggplot() +
        geom_col(aes(x = n, y = school_year),
                 fill = "grey", show.legend = FALSE) +
        geom_col(aes(x = f1$n, y = f1$school_year),
                 fill = "gold") +
        scale_x_continuous(breaks = seq(0, 25000, 5000)) +
        labs(title = "Figure 1: Stacked Bar Chart of Review Count Per School Per School Year") + # nolint
        xlab("Review Frequency") +
        ylab("School Year") +
        theme_bw()
ggsave(paste0(filename = image_fold_pth, "fig1", ".png"),
              plot = fig1,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )


# Figure 2: Review Sentiment Count
f2 <- output_df %>%
    group_by(STUDSENT_rComments_tags_lemma) %>%
    count() %>%
    mutate(posit = n + 3000)

fig2 <- output_df %>%
    ggplot(aes(x = STUDSENT_rComments_tags_lemma, fill = STUDSENT_rComments_tags_lemma)) + # nolint
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                )
                        ) +
        geom_bar(show.legend = F) +
        annotate("text", x = f2$STUDSENT_rComments_tags_lemma, y = f2$posit, label = f2$n) + # nolint
        theme_bw()
ggsave(paste0(filename = image_fold_pth, "fig2", ".png"),
              plot = fig2 ,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )

# Figure 3: Review Sentiment Count X Overall Review Rating
fig3 <- output_df %>%
    ggplot(aes(x = as.character(as.integer(rOverall)), fill = STUDSENT_rComments_tags_lemma)) + # nolint
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                                )
                        ) +
        geom_bar(position = "dodge", show.legend = F) +
        theme_bw()
ggsave(paste0(filename = image_fold_pth, "fig3", ".png"),
              plot = fig3,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )



# Figure 4: Average Overall Review Rating X Average Student Sentiment per School 
fig4 <- group_output_df %>%
    ggplot(aes(x = AVGSTUDSENT, y = as.character(round(AVGOVRALLRATING)), fill = as.character(round(AVGOVRALLRATING)))) +
        geom_boxplot(show.legend = F) +
        labs(title = "Figure 4: Average Overall Review Rating X Average Student Sentiment per School") +
        xlab("Average Student Sentiment") +
        ylab("Average Overall Review Rating") +
        theme_bw()
ggsave(paste0(filename = image_fold_pth, "fig4", ".png"),
              plot = fig4,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )


# Figure 5: Spaghetti Plot




output_df %>%
    filter(rOverall >= 5) %>%
    filter(STUDSENT_rComments_tags_lemma == "NEGATIVE") %>%
    select(rOverall, clean_rComments_tags_lemma, STUDSENT_rComments_tags_lemma)








ggsave(paste0(filename = image_fold_pth, "fig5", ".png"),
              plot = fig5,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )

# Figure 6: Residual Plot




ggsave(paste0(filename = image_fold_pth, "fig6", ".png"),
              plot = fig6,
              device = "png",
              width = 9,
              height = 8,
              units = "in",
             )






























# Count of Reviews for each school
output_df %>%
    group_by(school_sid) %>%
    count() %>%
    arrange(n)

# Count of School Years for each school
output_df %>%
    select(INSTNM, school_sid, professor_tid, rClass, school_year, rOverall, rComments) %>% # nolint
    unique() %>%
    group_by(school_sid, INSTNM, school_year) %>%
    count() %>%
    ungroup() %>%
    mutate(INSTNM_school_year = str_c(INSTNM, " - ", school_year)) %>%
    ggplot(aes(x = n, y = INSTNM, fill = school_year)) +
        geom_col() +
        theme_bw()

# Count of Schools for each School Year
output_df %>%
    group_by(school_sid, school_year) %>%
    count() %>%
    group_by(school_year) %>%
    count() %>%
    arrange(n)


output_df %>%
    select(INSTNM, school_sid, professor_tid, rClass, school_year, rOverall, rComments) %>%
    unique() %>%
    filter(INSTNM == "Kennesaw State University") %>%
    arrange(rComments)















output_df %>% names() #skim()










output_df %>%
mutate(
    SY_ind = case_when(school_year == "2001_02" ~ 0,
                        school_year == "2002_03" ~ 1,
                        school_year == "2003_04" ~ 2,
                        school_year == "2004_05" ~ 3,
                        school_year == "2005_06" ~ 4,
                        school_year == "2006_07" ~ 5,
                        school_year == "2007_08" ~ 6,
                        school_year == "2008_09" ~ 7,
                        school_year == "2009_10" ~ 8,
                        school_year == "2010_11" ~ 9,
                        school_year == "2011_12" ~ 10,
                        school_year == "2012_13" ~ 11,
                        school_year == "2013_14" ~ 12,
                        school_year == "2014_15" ~ 13,
                        school_year == "2015_16" ~ 14,
                        school_year == "2016_17" ~ 15,
                        school_year == "2017_18" ~ 16,
                        school_year == "2018_19" ~ 17,
                        T ~ -1)) %>%
    group_by(
        school_year,
        SY_ind,
        school_sid
    ) %>%
    count() %>%
    ungroup() %>%
    group_by(
        school_year,
        SY_ind
    ) %>%
    summarise(avg_rev = mean(n)) %>%
    ggplot(aes(x = SY_ind, y = avg_rev)) +
        geom_point() +
        geom_smooth(se = F, col = "black") +
        theme_bw()




# Figure 1: Frequency of Reviews
rev_freq_df <- output_df %>%
    group_by(
        school_year
    ) %>%
    count() %>%
    ungroup() 

rev_freq_df %>%
    ggplot(aes(x = n, y = school_year)) +
        geom_col(fill = "#7fdce9", col = "grey", show.legend = F) +
        labs(title = "Figure 1: Frequency of Reviews over Time") +
        xlab("Frequency of Reviews") +
        ylab("School Year") +
        theme_bw()


# Figure 2: Frequency of Schools
school_freq_df <- output_df %>%
    group_by(
        school_year,
        school_sid
    ) %>%
    count() %>%
    ungroup() %>%
    group_by(
        school_year
    ) %>%
    count() %>%
    ungroup() 

school_freq_df %>%
    ggplot(aes(x = n, y = school_year)) +
        geom_col(fill = "#ce79cb", col = "grey", show.legend = F) +
        labs(title = "Figure 2: Frequency of Schools over Time") +
        xlab("Frequency of Schools") +
        ylab("School Year") +
        theme_bw()


# Figure 3: Average Number of Reviews per school over time
avg_rev_df <- output_df %>%
    group_by(
        school_year,
        school_sid
    ) %>%
    count() %>%
    ungroup() %>%
    group_by(
        school_year
    ) %>%
    summarise(avg_rev = mean(n))

avg_rev_df %>%
    ggplot(aes(x = avg_rev, y = school_year)) +
        geom_col(fill = "", col = "grey", show.legend = F) +
        theme_bw()


merged_schoolrevavg_df <- left_join(
    rev_freq_df %>% rename("num_review" = "n"),
    school_freq_df %>% rename("num_school" = "n"),
    by = "school_year"
) %>%
left_join(
    .,
    avg_rev_df,
    by = "school_year"
) %>%
mutate(
    SY_ind = case_when(school_year == "2001_02" ~ 0,
                        school_year == "2002_03" ~ 1,
                        school_year == "2003_04" ~ 2,
                        school_year == "2004_05" ~ 3,
                        school_year == "2005_06" ~ 4,
                        school_year == "2006_07" ~ 5,
                        school_year == "2007_08" ~ 6,
                        school_year == "2008_09" ~ 7,
                        school_year == "2009_10" ~ 8,
                        school_year == "2010_11" ~ 9,
                        school_year == "2011_12" ~ 10,
                        school_year == "2012_13" ~ 11,
                        school_year == "2013_14" ~ 12,
                        school_year == "2014_15" ~ 13,
                        school_year == "2015_16" ~ 14,
                        school_year == "2016_17" ~ 15,
                        school_year == "2017_18" ~ 16,
                        school_year == "2018_19" ~ 17,
                        T ~ -1)
)

merged_schoolrevavg_df %>%
    ggplot(aes(x = SY_ind, y = avg_rev)) +
        geom_point(col = "#7ed6da") +
        geom_smooth(se = F, col = "#7a648b") +
        labs(title = "Figure 3: Average Number of Reviews per School over Time") + # nolint
        xlab("School Year") +
        ylab("Average Number of Reviews") +
        theme_bw()





# Figure 4: Frequency of Sentiment Analysis over Time
sent_df2 <- output_df %>%
    group_by(
        school_year,
        STUDSENT_label_comments_tags
    ) %>%
    count()

sent_df2 %>%
    ggplot(aes(x = n, y = school_year, fill = STUDSENT_label_comments_tags)) +
        geom_col(show.legend = T) +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                            )
                    ) +
        labs(title = "Figure 4: Frequency of Sentiment Analysis over Time") +
        xlab("Frequency of Reviews") +
        ylab("School Year") +
        theme_bw()


# Figure 5: Frequency of the Overall Review Ratings
ggplot(output_df, aes(x = rOverall, fill = STUDSENT_label_comments_tags)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                            )
                    ) +
        labs(title = "Figure 5: Frequency of the Overall Review Ratings") +
        xlab("Overall Review Rating") +
        ylab("Frequency of Reviews") +
        theme_bw()


# Figure 6:  Histogram of Withdraw Rate Overtime
with_df <- output_df %>%
    group_by(
        school_year,
        school_sid,
        WDRAW_ORIG_YR2_RT
    ) %>%
    summarise(
        avg_sent = mean(STUDSENT_score_comments_tags)
    ) %>%
    ungroup() %>%
    mutate(avg_sent = ifelse(avg_sent < 0.5, "NEGATIVE", "POSITIVE"))

with_df %>%
    ggplot(aes(x = WDRAW_ORIG_YR2_RT, fill = avg_sent)) +
        geom_histogram(col = "grey") +
        scale_fill_manual(values = c("NEGATIVE" = "#eb6f6f",
                                     "POSITIVE" = "#93BBA2"
                            )
                    ) +
        labs(title = "Figure 6: Histogram of Withdraw Rate") +
        xlab("Two-Year Withdrawal Rate") +
        ylab("Frequency of Schools") +
        theme_bw()



output_df %>%
    group_by(
        school_year,
        SY_ind,
        school_sid,
        WDRAW_ORIG_YR2_RT
    ) %>%
    count() %>%
    ungroup() %>%
mutate(
    SY_ind = case_when(school_year == "2001_02" ~ 0,
                        school_year == "2002_03" ~ 1,
                        school_year == "2003_04" ~ 2,
                        school_year == "2004_05" ~ 3,
                        school_year == "2005_06" ~ 4,
                        school_year == "2006_07" ~ 5,
                        school_year == "2007_08" ~ 6,
                        school_year == "2008_09" ~ 7,
                        school_year == "2009_10" ~ 8,
                        school_year == "2010_11" ~ 9,
                        school_year == "2011_12" ~ 10,
                        school_year == "2012_13" ~ 11,
                        school_year == "2013_14" ~ 12,
                        school_year == "2014_15" ~ 13,
                        school_year == "2015_16" ~ 14,
                        school_year == "2016_17" ~ 15,
                        school_year == "2017_18" ~ 16,
                        school_year == "2018_19" ~ 17,
                        T ~ -1)
) %>%
    ggplot(aes(x = SY_ind, y = WDRAW_ORIG_YR2_RT)) +
        geom_point(col = "#7ed6da") +
        geom_smooth(se = F, col = "#7a648b") +
        labs(title = "Figure 0: Two-Year Withdrawal Rate Over Time") + # nolint
        xlab("School Years since 2001-2002") +
        ylab("Proportion of Students that Withdrew in Two-Years") +
        theme_bw()

