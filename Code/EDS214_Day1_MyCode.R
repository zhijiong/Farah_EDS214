''
'
Start from the workflow
1. Read into 4 raw CSV files: BQ1, BQ2, BQ3, PRM, inspect the data
2. Select the columns we need: Sample_Date, K, NO3 - N, Mg, Ca, NH4 - N
3. Add a Site column
4. Combine the 4 sites into one table
5. Write a function for the 9-week moving average
6. Calculate the averages
7. Try to plot
* Note: What we are not doing here is to clean the data (deal with the duplicates),
restrict each chemistry at different sites to different time periods.
'
''

# Source the function for calculating the 9-week moving average
source("Functions.R")

# 1. Read into 4 raw CSV files: BQ1, BQ2, BQ3, PRM, inspect the data

library(tidyverse)
library(lubridate)

BQ1 <- read_csv("~/Downloads/Farah_EDS214/Data/QuebradaCuenca1-Bisley.csv")
BQ2 <- read_csv("~/Downloads/Farah_EDS214/Data/QuebradaCuenca2-Bisley.csv")
BQ3 <- read_csv("~/Downloads/Farah_EDS214/Data/QuebradaCuenca3-Bisley.csv")
PRM <- read_csv("~/Downloads/Farah_EDS214/Data/RioMameyesPuenteRoto.csv")

# 2. Select the columns we need: Sample_Date, K, NO3 - N, Mg, Ca, NH4 - N

BQ1 <- BQ1 %>% select(Sample_Date, K, `NO3-N`, Mg, Ca, `NH4-N`)
BQ2 <- BQ2 %>% select(Sample_Date, K, `NO3-N`, Mg, Ca, `NH4-N`)
BQ3 <- BQ3 %>% select(Sample_Date, K, `NO3-N`, Mg, Ca, `NH4-N`)
PRM <- PRM %>% select(Sample_Date, K, `NO3-N`, Mg, Ca, `NH4-N`)

# 3. Add a Site column

BQ1 <- BQ1 %>% mutate(Site = "BQ1")
BQ2 <- BQ2 %>% mutate(Site = "BQ2")
BQ3 <- BQ3 %>% mutate(Site = "BQ3")
PRM <- PRM %>% mutate(Site = "PRM")

# 4. Combine the 4 sites into one table

all_sites <- bind_rows(BQ1, BQ2, BQ3, PRM)

# 5. Write a function for the 9-week moving average

''
'For a date t, we want the values satisfying t - 9 weeks < date ≤ t

Week: 1 2 3 4 5 6 7 8 9
At week 9: [-----------------]
           average weeks 1-9

Week: 2 3 4 5 6 7 8 9 10
At week 10: [-----------------]
              average weeks 2-10
'
''

# Function is in file Functions.R

# 6. Calculate the averages

# Calculate 9-week moving average for each chemical
all_sites <- calc_9week_ma(all_sites, "K", "K_ma")
all_sites <- calc_9week_ma(all_sites, "NO3-N", "NO3_N_ma")
all_sites <- calc_9week_ma(all_sites, "Mg", "Mg_ma")
all_sites <- calc_9week_ma(all_sites, "Ca", "Ca_ma")
all_sites <- calc_9week_ma(all_sites, "NH4-N", "NH4_N_ma")

# 7. Try to plot

# Prepare data for plotting

# (1) Select the columns we need for plotting
plot_data <- all_sites %>%
  select(Sample_Date, Site, K_ma, NO3_N_ma, Mg_ma, Ca_ma, NH4_N_ma)
# (2) Reshape for plotting
plot_data <- plot_data %>%
  pivot_longer(
    cols = c(K_ma, NO3_N_ma, Mg_ma, Ca_ma, NH4_N_ma),
    names_to = "Chemical",
    values_to = "Moving_Average"
  )
# (3) Rename the chemicals for better labels
plot_data <- plot_data %>%
  mutate(
    Chemical = recode(
      Chemical,
      "K_ma" = "K",
      "NO3_N_ma" = "NO3-N",
      "Mg_ma" = "Mg",
      "Ca_ma" = "Ca",
      "NH4_N_ma" = "NH4-N"
    )
  )

# (4) Filter to the years we need for plotting
plot_data <- plot_data %>%
  filter(
    Sample_Date >= as.Date("1988-01-01"),
    Sample_Date <= as.Date("1994-12-31")
  )

# Finally do the plot!

library(gridExtra)

plot_panel <- function(data, chemical, y_label) {
  ggplot(
    data %>% filter(Chemical == chemical),
    aes(x = Sample_Date, y = Moving_Average, linetype = Site)
  ) +

    # Plot the lines
    geom_line(linewidth = 0.5, na.rm = TRUE) +

    # Hurricane Hugo
    geom_vline(xintercept = as.Date("1989-09-19"), linetype = "longdash") +

    # Line types for the four sites
    scale_linetype_manual(
      values = c(
        "PRM" = "solid",
        "BQ1" = "dotted",
        "BQ2" = "dashed",
        "BQ3" = "dotdash"
      )
    ) +

    # X axis
    scale_x_date(
      limits = as.Date(
        c("1988-01-01", "1994-06-30")
      ),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +

    # Labels
    labs(x = "Years", y = y_label, linetype = NULL) +

    theme_classic() +
    theme(legend.position = "right")
}

# Create the five panels

p1 <- plot_panel(
  plot_data,
  "K",
  "K (mg/L)"
)

p2 <- plot_panel(
  plot_data,
  "NO3-N",
  "NO3-N (ug/L)"
)

p3 <- plot_panel(
  plot_data,
  "Mg",
  "Mg (mg/L)"
)

p4 <- plot_panel(
  plot_data,
  "Ca",
  "Ca (mg/L)"
)

p5 <- plot_panel(
  plot_data,
  "NH4-N",
  "NH4-N (ug/L)"
)

# Combine the five panels into one figure
p_all <- grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  ncol = 1
)

# Save the figure
ggsave(
  "~/Downloads/Farah_EDS214/Output/Reproduced_Fig3.png",
  p_all,
  width = 10,
  height = 15,
  dpi = 300
)
