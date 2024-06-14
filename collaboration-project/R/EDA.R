# load library
pacman::p_load(tidyverse, readxl, ggplot2, gt, moments)

# load data from raw-data folder
df = read_xlsx(path = here::here("raw-data","WIF-tis4d.xlsx"), na ="")

# clean data: convert to lower case
df_clean = df %>%
  mutate(across(where(is.character), tolower))

# split data into two dataframe based on the cell type
df_wild = head(df_clean, 44)
df_type101 = tail(df_clean, 44)

# table for wild type: statistics summary of 'gene-expression'
table_wild_type = df_wild |>
  group_by(name) |>
  summarise(
    mean = mean(gene_expression),
    sd = sd(gene_expression),
    min = min(gene_expression),
    q25 = quantile(gene_expression, 0.25),
    median = median(gene_expression),
    q75 = quantile(gene_expression, 0.75),
    max = max(gene_expression)
  ) |>
  gt() |>
  tab_header(
    title = "Statistics summary of 'gene-expression'
    for Wild-type cell lines",
    subtitle = "(Data is rounded to two decimal places)"
  ) |>
  fmt_number(decimals = 2) |> # highlight cell group treated with activating factor 42
  tab_style_body(
    style = cell_fill(color = "lightblue"),
    value = "gl-rjs" ,
    targets = "row"
  ) |>
  tab_style_body(
    style = cell_fill(color = "lightblue"),
    value = "gl-xik" ,
    targets = "row"
  ) |>
  tab_footnote( # add footnote to 'name'
    footnote = "Name of Wild-type cell lines.
      Marked in lightblue were treated with the new treatment (activating factor 42)
      and the rest were treated with the placebo.",
    locations = cells_column_labels(columns = name)
  )

# table for cell type 101: statistics summary of 'gene-expression'
table_type101 = df_type101 |>
  group_by(name) |>
  summarise(
    mean = mean(gene_expression),
    sd = sd(gene_expression),
    min = min(gene_expression),
    q25 = quantile(gene_expression, 0.25),
    median = median(gene_expression),
    q75 = quantile(gene_expression, 0.75),
    max = max(gene_expression)
  ) |>
  gt() |>
  tab_header(
    title = "Statistics summary of 'gene-expression'
    for cell lines of Cell-type 101",
    subtitle = "(Data is rounded to two decimal places)"
  ) |>
  fmt_number(decimals = 2) |> # highlight cell group treated with activating factor 42
  tab_style_body(
    style = cell_fill(color = "lightblue"),
    value = "gl-zhw" ,
    targets = "row"
  ) |>
  tab_style_body(
    style = cell_fill(color = "lightblue"),
    value = "gl-mfa" ,
    targets = "row"
  ) |>
  tab_footnote(
    footnote = "Name for cell lines of Cell-type 101.
      Marked in lightblue were treated with the new treatment (activating factor 42)
      and the rest were treated with the placebo.",
    locations = cells_column_labels(columns = name)
  )

# wild type; plot fitted curve to observe effect of concentration
plot_wild = df_wild |>
  ggplot(aes(x= conc, y= gene_expression, color = name, shape = treatment)) +
  geom_point() +
  geom_smooth() +
  labs(x= "concentration",
       y= "gene expression",
       caption = "Different curves represent different Wild-type cell lines.
        Different point shapes represent different treatments"
  ) +
  ggtitle(
    "Effect of concentration of different treatment on \nthe gene expression for Wild-type cell lines"
  )

# cell type 101; plot fitted curve to observe effect of concentration
plot_type101 = df_type101 |>
  ggplot(aes(x= conc, y= gene_expression, color = name, shape = treatment)) +
  geom_point() +
  geom_smooth() + #method='lm', formula= y~x) +
  labs(x= "concentration",
       y= "gene expression",
       caption = "Different curves represent different cell lines of Cell-type 101.
        Different point shapes represent different treatments"
  ) +
  ggtitle(
    "Effect of concentration of different treatment on \nthe gene expression for cell lines of Cell-type 101"
  )

# wild type; boxplot of gene expression
box_wild = df_wild |>
  ggplot(aes(x= treatment, y= gene_expression, fill = name)) +
  geom_boxplot() +
  labs(x= "treatment",
       y= "gene expression",
       fill = "cell lines"
  ) +
  ggtitle(
    "Box plots of gene expression for Wild-type cell lines"
  ) +
  harrypotter::scale_fill_hp("Ravenclaw", discrete = TRUE)

# cell type 101; boxplot of gene expression
box_type101 = df_type101 |>
  ggplot(aes(x= treatment, y= gene_expression, fill = name)) +
  geom_boxplot() +
  labs(x= "treatment",
       y= "gene expression",
       fill = "cell lines"
  ) +
  ggtitle(
    "Box plots of gene expression for cell lines of Cell-type 101"
  ) +
  harrypotter::scale_fill_hp("Ravenclaw", discrete = TRUE)


# save tables
gtsave( table_wild_type, here::here("tabs", "table-wild-type.html"))
gtsave( table_type101, here::here("tabs", "table-type101.html"))

# save plots
ggsave(filename = here::here("figs", "plot-wild.pdf"), plot = plot_wild)
ggsave(filename = here::here("figs", "plot-type101.pdf"), plot = plot_type101)
ggsave(filename = here::here("figs", "box-wild.pdf"), plot = box_wild)
ggsave(filename = here::here("figs", "box-type101.pdf"), plot = box_type101)
