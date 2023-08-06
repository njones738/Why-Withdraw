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


rmp_data_pth <- "N:/Classes/2022_2FALL/Analytics Day/Data/ratemyprof_reviews_IMPORTANT.csv" # nolint
csc_data_pth <- "N:/Classes/2022_2FALL/Analytics Day/Data/MERGED_PP.csv" # nolint


df <- read_csv(csc_data_pth,
               na = c("", " ", "PrivacySuppressed", "NULL", "NA", "NaN", "Na"), # nolint
               col_types = c(UNITID = "c", OPEID = "c", OPEID6 = "c", INSTNM = "c", # nolint
                             ACCREDAGENCY = "c", ACCREDCODE = "c", REGION = "c", # nolint
                             CITY = "c", ZIP = "c", STABBR = "c", ST_FIPS = "c", # nolint
                             LOCALE = "c", OPEFLAG = "c", OPENADMP = "c", # nolint
                             CCBASIC = "c", CCSIZSET = "c", CCUGPROF = "c", # nolint
                             CONTROL = "c", CURROPER = "c", DISTANCEONLY = "c", # nolint
                             HIGHDEG = "c", PREDDEG = "c", ICLEVEL = "c", # nolint
                             SCH_DEG = "c", HCM2 = "c", MAIN = "c", HBCU = "c", # nolint
                             PBI = "c", ANNHI = "c", TRIBAL = "c", AANAPII = "c", # nolint
                             HSI = "c", NANTI = "c", MENONLY = "c", WOMENONLY = "c",  # nolint
                             RELAFFIL = "c" # nolint
                            )) %>% # nolint
        select(-'...1') %>% # nolint
        rename("school_sid" = sid) %>%
        mutate(school_sid = as.character(school_sid))

ga_rmp_df <- read_csv(rmp_data_pth, col_types = c("sid" = "c", "school_sid" = "c", "professor_tid" = "c", "id" = "c")) %>% # nolint
        select(-'...1', -rErrorMsg, -teacher, -usefulGrouping, -unUsefulGrouping, -rOverallString) %>% # nolint
        mutate(onlineClass = ifelse(is.na(onlineClass), "0", ifelse(str_to_lower(onlineClass) == "online", "1", "0")), # nolint
               rTimestamp = as_datetime(rTimestamp),
               teacherGrade = case_when((is.na(teacherGrade)) | (teacherGrade %in% c("Audit/No Grade", "INC")) ~ "Missing/Audit/No Grade/Inconclusive", # nolint
                                            teacherGrade == "Not sure yet" ~ "Undecided", T ~ teacherGrade), # nolint
               across(.cols = c(rInterest, attendance, rClass, rTextBookUse, rWouldTakeAgain, takenForCredit), # nolint
                      .fns = function(x) ifelse(is.na(x), "Missing", x)),  # nolint
               temp_rComments = str_to_lower(str_remove(rComments, "[:punct:]")), # nolint
               rComments = case_when(is.na(rComments) ~ NA_character_,
                                     rComments %in% c("!", "!!", "!!!", "****!") ~ NA_character_,  # nolint
                                     rComments == ":(" ~ "Frowning Face",
                                     rComments %in% c(":)", ":D", "=)", ":-)") ~ "Happy Face", # nolint
                                     str_to_lower(rComments) == "n/a" ~ NA_character_, # nolint
                                     temp_rComments == "smh" ~ "Shaking My Head", # nolint
                                     temp_rComments == "ok" ~ "okay",
                                     temp_rComments %in% c("no", "na") ~ "nope",
                                     rComments == "#1" ~ "Number One", # nolint
                                     rComments == "A+" ~ "A PLUS",
                                     rComments %in% c(".", "..", "...", ".....") ~ NA_character_,  # nolint
                                     rComments %in% c(":B", ":S", "aj", "&lt;3", "&#63;", "10", "s...") ~ NA_character_, # nolint
                                     str_length(rComments) <= 1 ~ NA_character_,
                                     T ~ rComments)) %>% # nolint
        filter(!is.na(rComments)) %>%
        unique() %>%
        mutate(semester = ifelse(as.integer(str_sub(rDate, 1, 2)) < 7, "Spring", "Fall"), # nolint
               year = str_sub(rDate, -4, -1) %>% as.numeric(),
               school_year = ifelse(semester == "Spring", paste0(as.character(year-1), "_", str_sub(rDate, -2, -1)), paste0(as.character(year), "_", as.character(as.numeric(str_sub(rDate, -2, -1))+1))) # nolint
              ) %>%
        select(school_sid, professor_tid, rClass, # *** IDENTITIES ***
               school_year, rDate,                # *** TIME VARIABLES ***
               teacherGrade, rOverall, quality,   # *** MAIN RATINGs ***
               rComments, teacherRatingTags,      # *** TEXT/LABEL ***
               attendance, onlineClass,           # *** INDICATORs & CATEGORICAL RATINGS *** # nolint
               rTextBookUse, takenForCredit,
               rWouldTakeAgain, rClarity, rEasy,
               rHelpful, helpCount, notHelpCount) %>%
        left_join(.,
                  df %>% select(OPEID, INSTNM, school_sid, school_year, STABBR, WDRAW_ORIG_YR2_RT) %>% unique(), # nolint
                  by = c("school_sid", "school_year")) %>%
        filter(STABBR == "GA") %>% 
        select(-STABBR) %>%
        filter(!is.na(WDRAW_ORIG_YR2_RT)) %>%
        filter(!(school_sid %in% c("1774", "4007", "4683", "12856", "4374", "5027", "5164", "17133"))) %>% # nolint
        filter(school_year != "2000_01") %>%
        unique() %>%
        mutate(across(.cols = c(rOverall, rClarity, rEasy, rHelpful), .fns = function(x) as.character(x)), # nolint
               com_length = str_length(rComments))


ga_rmp_df %>% skim()

ga_rmp_df %>% 
    group_by(school_year) %>%
    count()


df %>% select(OPEID, INSTNM, school_sid, school_year, HIGHDEG, STABBR, WDRAW_ORIG_YR2_RT) %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(school_sid)) %>%
    filter(!is.na(WDRAW_ORIG_YR2_RT))




# ga_rmp_df %>% write_csv("N:/Classes/2022_2FALL/Analytics Day/Code/rmp_df.csv") # nolint














df %>%
    filter(!is.na(WDRAW_ORIG_YR2_RT)) %>%
    filter(STABBR == "GA") %>%
    select(where(is.character)) %>%
    skim()



ga_rmp_df %>% 
    group_by(school_sid, school_year) %>%
    nest()


write_csv("Code/rmp_df.csv")




ga_rmp_df %>%
    create_report(., y = NULL,
                  output_dir = "report.csv",
                  output_file = "report.html",
                  config = config,
                  report_title = "Original Report"
                 )



ga_rmp_df %>%
    mutate(com_length = str_length(rComments)) %>%
    filter(com_length <= 5) %>%
    select(INSTNM, rComments, rOverall) %>%
    print(n = Inf)

ga_rmp_df %>%
    select(where(is.numeric))







ga_rmp_df %>% skim()

v <- ga_rmp_df %>%
    vis_dat(warn_large_data = FALSE)
ggsave(filename = "visdat", plot = v, device = "png")



ga_rmp_df %>%
    create_report()


ga_rmp_df %>%
    group_by(rOverall) %>% # equivalent to rOverallString
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)

ga_rmp_df %>%
    group_by(quality) %>% # quality
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)

ga_rmp_df %>%
    group_by(rEasy) %>% # difficulty
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)




ga_rmp_df %>%
    group_by(rInterest) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)

ga_rmp_df %>%
    group_by(attendance) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)



ga_rmp_df %>%
    group_by(rClass) %>%
    count() %>%
    arrange(desc(n))
ga_rmp_df %>%
    group_by(onlineClass) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(rTextBookUse) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)




ga_rmp_df %>%
    group_by(rClarity) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(clarityColor) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)



ga_rmp_df %>%
    group_by(rEasyString) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(easyColor) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)



ga_rmp_df %>%
    group_by(rHelpful) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(helpColor) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)


ga_rmp_df %>%
    group_by(rWouldTakeAgain) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(takenForCredit) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
    

ga_rmp_df %>%
    group_by(teacherGrade) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)












ga_rmp_df %>%
    group_by(rStatus) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)



ga_rmp_df %>%
    group_by(rTimestamp) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)
ga_rmp_df %>%
    group_by(teacherRatingTags) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)






ga_rmp_df %>%
    group_by(teacherGrade_cat) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)

ga_rmp_df %>%
    group_by(teacherGrade_num) %>%
    count() %>%
    print(n = 100)

ga_rmp_df %>% unique() %>% skim()



num_vars <- df %>%
    select(where(is.numeric))

num_vars %>% create_report()






config <- configure_report(add_introduce = TRUE,
                           add_plot_intro = TRUE,
                           add_plot_str = T, # nolint
                           add_plot_missing = TRUE,
                           add_plot_histogram = TRUE,
                           add_plot_density = TRUE,
                           add_plot_qq = T, # nolint
                           add_plot_bar = TRUE,
                           add_plot_correlation = TRUE,
                           add_plot_prcomp = T, # nolint
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
                           plot_histogram_args = list(geom_histogram_args = list(bins = 25L, # nolint
                                                                                 fill = "lavender", # nolint
                                                                                 col = "#878787"), # nolint
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
                                                    geom_density_args = list(fill = "lavender", col = "black"), # nolint
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
                                                   prcomp_args = list(scale. = TRUE), # nolint
                                                   geom_label_args = list(),
                                                   title = NULL,
                                                   ggtheme = theme_bw(),
                                                   theme_config = list(text = element_text(size = 12)), # nolint
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

df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    create_report(., y = NULL,
                  output_dir = "report2.csv",
                  output_file = "report2.html",
                  config = config,
                  report_title = "Report2"
                 )


shapiro.test()


C150_4

PCTPELL
UG25ABV
WDRAW_ORIG_YR2_RT
IND_WDRAW_ORIG_YR2_RT
DEP_WDRAW_ORIG_YR2_RT
PELL_WDRAW_ORIG_YR2_RT
PELL_WDRAW_ORIG_YR3_RT


d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(GT_THRESHOLD_P6)))$GT_THRESHOLD_P6
shapiro.test(d)


d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(NPT4_PUB)))$NPT4_PUB
shapiro.test(d)



d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(NPT4_PRIV)))$NPT4_PRIV
shapiro.test(d)



d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(PCTPELL)))$PCTPELL
shapiro.test(d)


d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(AVGFACSAL)))$AVGFACSAL
shapiro.test(d)



d <- (df %>%
    filter(HIGHDEG %in% c("3", "4")) %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(DEP_WDRAW_ORIG_YR2_RT)))$DEP_WDRAW_ORIG_YR2_RT
shapiro.test(d)






df %>%
    filter(STABBR == "GA") %>%
    filter(!is.na(WDRAW_ORIG_YR2_RT)) %>%
    filter(!is.na(school_sid)) %>%
    select(OPEID, WDRAW_ORIG_YR2_RT, school_year, school_sid) %>%
    summary()
    write_csv("csc_subdf.csv")




gor_df <- read_csv("Code/group_output_results.csv") %>%
                select(-...1, -rComments, -clean_rComments, -label, -score, -temp)

gor_df %>%
    ggplot(aes(x = avg_score, fill = school_year)) +
        geom_histogram() +
        # facet_wrap(school_year ~ .) +
        theme_bw()


gor_df %>% 
    pivot_wider(names_from = school_year,
                values_from = c(WDRAW_ORIG_YR2_RT, avg_score)
               ) %>%
    vis_dat








ga_rmp_df %>%
    write_csv("Code/rmp_df.csv")













ga_rmp_df %>%
    group_by(attendance) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)


ga_rmp_df %>%
    group_by(onlineClass) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)


ga_rmp_df %>%
    group_by(rTextBookUse) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)



ga_rmp_df %>%
    group_by(rEasy) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = 100)






