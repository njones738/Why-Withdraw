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
data_pth <- paste0(fold_pth, "rmp_df.csv")
output_pth <- paste0(fold_pth, "output_results.csv")

df <- read_csv(data_pth) %>%
                filter(!(school_sid %in% c("1774", "4007", "4683", "12856", "4374", "5027", "5164", "17133"))) %>%
                filter(school_year != "2000_01")
output_df <- read_csv(output_pth) %>%
                filter(!(school_sid %in% c("1774", "4007", "4683", "12856", "4374", "5027", "5164", "17133"))) %>%
                filter(school_year != "2000_01")

df %>%
    vis_dat(warn_large_data = FALSE)

output_df %>%
    select(school_sid, professor_tid, rClass, school_year, contains("Comment"), contains("STUDSENT")) # nolint

################################################################
################################################################
################################################################

temp_studsent <- output_df %>%
    group_by(STUDSENT_label_rComments) %>%
    count() %>%
    mutate(y = n + 1250)


stud_sent_plot <- output_df %>%
    ggplot(aes(x = STUDSENT_label_rComments, fill = STUDSENT_label_rComments)) +
        geom_bar(show.legend = F) +
        annotate("text", x = temp_studsent$STUDSENT_label_rComments, y = temp_studsent$y, label = temp_studsent$n) + # nolint
        theme_bw()


temp_studsent <- output_df %>%
    group_by(STUDSENT_label_rComments_tags) %>%
    count() %>%
    mutate(y = n + 1250)


stud_sent_plot_tags <- output_df %>%
    ggplot(aes(x = STUDSENT_label_rComments_tags, fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(show.legend = F) +
        annotate("text", x = temp_studsent$STUDSENT_label_rComments_tags, y = temp_studsent$y, label = temp_studsent$n) + # nolint
        theme_bw()

(stud_sent_plot | stud_sent_plot_tags)

################################################################
################################################################
################################################################

# rOverall bar chart
df %>%
    ggplot(aes(x = as.character(rOverall))) +
        geom_bar(fill = "lavender", col = "grey") +
        theme_bw()

# rClarity bar chart
df %>%
    ggplot(aes(x = as.character(rClarity))) +
        geom_bar(fill = "lavender", col = "grey") +
        theme_bw()

# rHelpful bar chart
df %>%
    ggplot(aes(x = as.character(rHelpful))) +
        geom_bar(fill = "lavender", col = "grey") +
        theme_bw()

################################################################
################################################################
################################################################

# rOverall bar chart
output_df %>%
    ggplot(aes(x = as.character(rOverall), fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(position = "dodge", show.legend = F) +
        labs(title = "Overall stratified by student sentiment (with tags)") +
        xlab("Overall Rating") +
        ylab(NULL) +
        theme_bw()

# rClarity bar chart
temp_clarity <- output_df %>%
    group_by(rClarity, STUDSENT_label_rComments_tags) %>%
    count() %>%
    mutate(rClarity = case_when(STUDSENT_label_rComments_tags == "NEGATIVE" ~ rClarity - 0.225, # nolint
                                 STUDSENT_label_rComments_tags == "POSITIVE" ~ rClarity + 0.225, # nolint
                                ),
           y = n + 1200
          )

output_df %>%
    ggplot(aes(x = as.character(rClarity), fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(position = "dodge", show.legend = F) +
        labs(title = "Clarity stratified by student sentiment (with tags)") +
        xlab("Clarity Rating") +
        ylab(NULL) +
        annotate("text", x = temp_clarity$rClarity, y = temp_clarity$y, label = temp_clarity$n) + # nolint
        theme_bw()

# rHelpful bar chart
temp_helpful <- output_df %>%
    group_by(rHelpful, STUDSENT_label_rComments_tags) %>%
    count() %>%
    mutate(rHelpful = case_when(STUDSENT_label_rComments_tags == "NEGATIVE" ~ rHelpful - 0.225, # nolint
                                 STUDSENT_label_rComments_tags == "POSITIVE" ~ rHelpful + 0.225, # nolint
                                ),
           y = n + 1200
          )

output_df %>%
    ggplot(aes(x = as.character(rHelpful), fill = STUDSENT_label_rComments_tags)) + # nolint
        geom_bar(position = "dodge", show.legend = F) +
        labs(title = "Helpful stratified by student sentiment (with tags)") +
        xlab("Helpful Rating") +
        ylab(NULL) +
        annotate("text", x = temp_helpful$rHelpful, y = temp_helpful$y, label = temp_helpful$n) + # nolint
        theme_bw()

################################################################
################################################################
################################################################

output_df %>%
    group_by(school_sid, school_year) %>%
    count() %>%
    group_by(school_year) %>%
    count() %>%
    arrange(n)



too_few_occasions <- 



















