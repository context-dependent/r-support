"
Dumbell Chart
=============

The dumbell chart is commonly proposed as an elevated
form of bar chart, in cases where one is aiming to
represent a change in a quantity for multiple groups
between two different time periods

In the example below, we visualize the changes in
life expectancy by continent between 1952 and 2007,
as reported in the gapminder data.
"

# Load our packages

pacman::p_load(
    "tidyverse",
    "gapminder"
)

# Prepare the data for the chart

d <- gapminder

dplt <- d |>
    # keep only rows where the year is either 1952 or 2007
    filter(year %in% c(1952, 2007)) |>
    # calculate average life expectancy by continent / year,
    # weighted by population
    group_by(continent, year) |>
    summarize(avg_lxp = mean(lifeExp, weights = pop)) |>
    ungroup()


# Start with the skeleton

plt0 <- dplt |>
    ggplot(aes(avg_lxp, continent)) +
    geom_path(aes(group = continent)) +
    geom_point(aes(color = year))

plt0

# Things are in the right place, but there are a couple
# of issues:
#  1. The points are too small.
#  2. The legend for year is continuous, but we want it to be
#     categorical
#  3. The theming is butt-ugl, and the labels are too crunchy

# (1) Bigger, better Points

plt1 <- dplt |>
    # Starts the same as before
    ggplot(aes(avg_lxp, continent)) +
    geom_path(aes(group = continent)) +
    # We switch it up here, by setting size = 4
    # OUTSIDE OF `aes()`. Size is a global parameter
    # for the chart, and doesn't depend on the data,
    # so it isn't included in the aesthetic mappings
    # for the point layer.
    geom_point(aes(color = year), size = 4)

plt1

# Look at those big juicy points!

# (2) categorizing year

plt2 <- dplt |>
    # Seen this before.
    ggplot(aes(avg_lxp, continent)) +
    geom_path(aes(group = continent)) +
    # Here, we turn year into a factor (categorical variable)
    # within the chart code. If we were going to make multiple
    # charts using this categorical year, we would want to make the
    # change earlier, in the underlying data, but this works just fine
    # for our current purposes.
    geom_point(aes(color = factor(year)), size = 4)

plt2

# (3) making it nice looking!

plt3 <- dplt |>
    ggplot(aes(avg_lxp, continent)) +
    geom_path(
        aes(group = continent),
        color = "#0000ff"
    ) +
    # Here, everything is the same EXCEPT
    # that we change the shape we're using for the points
    # to 23, a hollow circle that accepts `fill` aesthetic
    geom_point(
        aes(fill = factor(year)),
        shape = 21, size = 4, color = "#0000ff"
    ) +

    # There are lots of different ways to control the labels
    # of a ggplot. Over years of experimentation, I have found this to be the
    # most memorable and straightforward.
    labs(
        title = "Average Life Expectancy Around the World",
        subtitle = "Changes by continent from 1952 to 2007",
        x = "Average Life Expectancy",
        y = NULL,
        fill = "Year",
        caption = "Excerpt of data from gapminder.org\nprovided by Jennifer Bryan's gapminder package."
    ) +
    bptheme::theme_blueprint_2021(
        grid = "Xx"
    ) +
    scale_fill_manual(
        values = c("#e7e7e7", "#0000ff")
    )

plt3
