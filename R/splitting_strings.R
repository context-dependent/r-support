pacman::p_load(
    "tidyverse"
)

# The problem, broadly speaking:
# ==============================
#
# We have:
# --------
# fake_survey, a tibble (aka dataframe)
# with two columns:
#   response_id <int>, which holds made-up unique identifiers
#       for the 'responses'
#   hs_strengths <chr>, which holds made-up responses to
#       the following select-all question:
#       "In high school, which courses did you excel at?"
#       _select all that apply_
#           [] Math
#           [] Art
#           [] English
#           [] Lunch
fake_survey <- tibble(
    response_id = 1:8,
    hs_strengths = c(
        "Lunch",
        "Math,Art",
        "Math,English",
        "Art,English,Math",
        "",
        "English,Lunch",
        "Art",
        "English"
    )
)

# We want:
# --------
# fake_cleaned_data, a tibble
# with n + 2 columns, where n is the number of response options:
#   response_id, same as in fake_survey
#   hss_{response_option} <bool>,
#       NA    if no responses were selected
#       TRUE  if {response_option} was selected
#       FALSE if {response_option} wasn't selected
#   hss_missing <bool>,
#       TRUE  if no responses were selected
#       FALSE if one or more responses were selected

fake_cleaned_data <- tribble(
    ~response_id, ~hss_missing, ~hss_math, ~hss_art, ~hss_english, ~hss_lunch,
    1L, FALSE, FALSE, FALSE, FALSE, TRUE,
    2L, FALSE, TRUE, TRUE, FALSE, FALSE,
    3L, FALSE, TRUE, FALSE, TRUE, FALSE,
    4L, FALSE, TRUE, TRUE, TRUE, FALSE,
    5L, TRUE, NA, NA, NA, NA,
    6L, FALSE, FALSE, FALSE, TRUE, TRUE,
    7L, FALSE, FALSE, TRUE, FALSE, FALSE,
    8L, FALSE, FALSE, FALSE, TRUE, FALSE
)

# Note: {N}L syntax guarantees that N will be
#   treated as an integer

# Making it work:
# ==============

# Option 1: String Detection
# --------------------------
# In this option, we construct each of the response option
# columns individually, by asking whether the response option appears
# in the response string

option_one <- fake_survey |>
    mutate(
        hss_missing = nchar(hs_strengths) == 0,
        hss_math = case_when(
            # if it is the case that hss_missing is TRUE, then (~)
            # hss_math is NA
            hss_missing ~ NA,
            # Otherwise, hss_math is TRUE if hs_strengths contains
            # Math, and FALSE if it doesn't
            TRUE ~ str_detect(hs_strengths, "Math")
        ),
        hss_art = case_when(
            hss_missing ~ NA,
            TRUE ~ str_detect(hs_strengths, "Art")
        ),
        hss_english = case_when(
            hss_missing ~ NA,
            TRUE ~ str_detect(hs_strengths, "English")
        ),
        hss_lunch = case_when(
            hss_missing ~ NA,
            TRUE ~ str_detect(hs_strengths, "Lunch")
        )
    ) |>
    select(-hs_strengths)

all_equal(option_one, fake_cleaned_data)

# Option 2: Split Rows, Relabel, and Pivot Wide
# ---------------------------------------------
# Option 1 is straightforward and easy to think through,
# but can be tedious for many-optioned questions.
# Option 2 offers a more generalizable approach.
# The steps below generate the new column names and
# values programmatically, though they are a bit
# more complex to think and reason about.

# Step i): choose the response column and the id column
#   While this step is redundant in this example,
#   remember to pick just the columns you are working with
#   when you're reshaping a dataframe. A failure to do so
#   can result in lots of unnecessary computation

option_two_i <- fake_survey |>
    select(response_id, hs_strengths)

# Step ii) Spread the responses across multiple rows
#   This step yields a long version of the survey data,
#   with one row for each pair of (survey_response, selected_option)
option_two_ii <- option_two_i |>
    separate_rows(hs_strengths, sep = ",")

# | response_id|hs_strengths |
# |-----------:|:------------|
# |           1|Lunch        |
# |           2|Math         |
# |           2|Art          |
# |           3|Math         |
# |           3|English      |
# |           4|Art          |
# |           4|English      |
# |           4|Math         |
# |           5|             |
# |           6|English      |
# |           6|Lunch        |
# |           7|Art          |
# |           8|English      |

# Step iii) generate column labels, and TRUE flags, programatically

option_two_iii <- option_two_ii |>
    mutate(
        name = case_when(
            nchar(hs_strengths) == 0 ~ "hss_missing",
            TRUE ~ str_c("hss_", tolower(hs_strengths))
        ),
        val = TRUE
    )

# | response_id|hs_strengths |names       |val  |
# |-----------:|:------------|:-----------|:----|
# |           1|Lunch        |hss_lunch   |TRUE |
# |           2|Math         |hss_math    |TRUE |
# |           2|Art          |hss_art     |TRUE |
# |           3|Math         |hss_math    |TRUE |
# |           3|English      |hss_english |TRUE |
# |           4|Art          |hss_art     |TRUE |
# |           4|English      |hss_english |TRUE |
# |           4|Math         |hss_math    |TRUE |
# |           5|             |hss_missing |TRUE |
# |           6|English      |hss_english |TRUE |
# |           6|Lunch        |hss_lunch   |TRUE |
# |           7|Art          |hss_art     |TRUE |
# |           8|English      |hss_english |TRUE |

# Step iv) drop the original column, then pivot on the new column names
#   In this step, we make a column for each of the names we made
#   and give each cell in the new column the value
#   in the val column for its response_id.
#   Where there is no value in the val column, we fill the blank with FALSE.
#   This process outputs the correct result, except that the respondent who
#   didn't select anything has FALSEs where we want NAs.

option_two_iv <- option_two_iii |>
    select(-hs_strengths) |>
    pivot_wider(
        id_cols = response_id,
        names_from = name,
        values_from = val,
        values_fill = FALSE
    )

# Step v) make options for missing responses NA
#   In this last step, we go through each of the new columns
#   except hss_missing,
#   and set them to NA where hss_missing is TRUE

option_two_v <- option_two_iv |>
    mutate(
        across(
            c(-response_id, -hss_missing),
            function(x) {
                case_when(
                    hss_missing ~ NA,
                    TRUE ~ x
                )
            }
        )
    )

all_equal(option_two_v, fake_cleaned_data)


# BONUS ROUND: Summarizing!

# what's the easiest way to generate counts and
# percentages from this kind of data, once it's
# been booleanized?

# I'm going to illustrate the answer in two steps:

# 1) how to summarize one column,
# 2) how to do the same summary across all the columns of interest

# 1) summarizing once: hss_english
english_summary_tab <- fake_cleaned_data |>
    summarize(
        hss_english_N = sum(!is.na(hss_english)),
        hss_english_n = sum(na.omit(hss_english)),
        hss_english_p = mean(na.omit(hss_english))
    )

english_summary_tab

# 2a) summarizing `across` [Option A]
#   produces a dataframe with one result column per
#   source column per indicator.
#   In this example, we're calculating three indicators
#   for each of 5 columns, which will yield 15 total summary columns.
full_summary_tab_wide <- fake_cleaned_data |>
    summarize(
        across(
            matches("hss"),
            list(
                N = function(x) sum(!is.na(x)),
                n = function(x) sum(na.omit(x)),
                p = function(x) mean(na.omit(x))
            )
        )
    )


# 2b) pivot, group, summarize [Option B]
#   this approach yields a long dataframe with one row per
#   select-all option, and a column per indicator.

full_summary_tab_long <- fake_cleaned_data |>
    pivot_longer(
        matches("hss"),
        names_to = "response_option",
        values_to = "response_value"
    ) |>
    group_by(response_option) |>
    summarize(
        N = sum(!is.na(hss_english)),
        n = sum(na.omit(hss_english)),
        p = mean(na.omit(hss_english))
    )

# You'll note that the summarize() step in this chain looks
# very close to the first example, summarizing hss_english.
# It is! The difference in the output is driven by the
# changes we're making to the data by pivoting and then grouping.