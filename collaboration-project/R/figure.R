# load library
pacman::p_load(tidyverse, readxl, ggplot2, showtext, cowplot, ggrepel, openxlsx)

# load cleaned data
df = read_xlsx(path = here::here("data","2024-06-14-WIF-tis4d.xlsx"), na ="")

# Or clean the raw data
# ____________________
# load data
df = read_xlsx(path = here::here("raw-data","WIF-tis4d.xlsx"), na ="")

df_rename = df %>%
  mutate(
    name = case_when(
      name == "GL-cDZ" ~ "cDZ",
      name == "GL-kYH" ~ "kYH",
      name == "GL-rjS" ~ "rjS",
      name == "GL-Xik" ~ "Xik",
      name == "Gl-Cwn" ~ "cwN",
      name == "GL-MFA" ~ "MFA",
      name == "Gl-Xib" ~ "XIb",
      name == "Gl-Zhw" ~ "ZHw",
      name == "GL-cwN" ~ "cwN",
      name == "Gl-Rjs" ~ "rjS",
      name == "GL-XIb" ~ "XIb",
      name == "GL-ZHw" ~ "ZHw",
      T ~ ""
    )
  )
df_rename = df_rename %>%
  mutate(
    treatment = case_when(
      treatment == "Placebo" ~ "Placebo",
      treatment == "Activating factor 42" ~ "Activating factor 42",
      treatment == "placebo" ~ "Placebo",
      treatment == "activating factor 42" ~ "Activating factor 42",
      T ~ ""
    )
  )

names(df_rename)[names(df_rename) == "treatment"] <- "Treatment"
df = df_rename
# save the modified dataset in the data folder
folder = here::here("data")
filename = "2024-06-14-WIF-tis4d.xlsx"
filepath = file.path(folder, filename)
write.xlsx(df, filepath)
#________________________________

# import font
font_add(
  family = "Times New Roman",
  regular = #"C:/Windows/Fonts"
    here::here(
      "figs","Times New Roman.ttf"
    )
)

showtext_auto()

# split data into two dataframe based on the cell type
df_wild = head(df, 44)
df_type101 = tail(df, 44)


# indices of cells
a = c(21,22,43,44)

# construct dataframe of points to be add labels for wild type
label_point_wild <- data.frame(conc = c(10,10,10,10),
                          gene_expression = df_wild$gene_expression[a],
                          sample = c("XIb","cDZ",
                                        "rjS",
                                        "Xik") ,
                          Treatment =c("Placebo","Placebo",
                                       "Activating factor 42",
                                       "Activating factor 42"))

# wild type; plot of data points of gene expression against concentration
wild = df_wild |>
  ggplot(aes(x= conc, y= gene_expression,
             fill =  Treatment)) +
  geom_point(size = 3, shape = 21, stroke = 0.6)  +
  scale_fill_manual(values = c("Activating factor 42" = "#78A8D1", "Placebo" = "#CDB892")) + # points filled with colours
  labs(x= "ug/ml",
       y= "Gene Expression",
  ) +
  ggtitle(
    "Wild-type"
  ) +
  theme_minimal()+
  theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6), # plot border
      text = element_text(family = "Times New Roman",size = 20), # set the font
      legend.position = "none"
      )+
  xlim(-0.5, 11.5) # x-axis bounds

# combine the plot and the points' labels
wild_labels = wild + geom_label_repel(data = label_point_wild,aes(label = sample,
                     fill = factor(Treatment)), color = 'black',
                 size = 5)

# cell type 101; points to be add labels
label_point_type101 = data.frame(conc = c(10,10,10,10),
                          gene_expression = df_type101$gene_expression[a],
                          sample = c("cwN",
                                     "kYH",
                                     "ZHw",
                                     "MFA") ,
                          Treatment =c("Placebo","Placebo",
                                       "Activating factor 42",
                                       "Activating factor 42"))

# type101; plot of data points of gene expression against concentration
type101 = df_type101 |>
  ggplot(aes(x= conc, y= gene_expression,
             fill =  Treatment)) +
  geom_point(size = 3, shape = 21, stroke = 0.5)  +
  scale_fill_manual(values = c("Activating factor 42" = "#78A8D1", "Placebo" = "#CDB892")) +
  labs(x= "ug/ml",
       y= "Gene Expression",
  ) +
  ggtitle(
    "Wild-type"
  ) +theme_minimal()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    text = element_text(family = "Times New Roman",size = 20),
    legend.position = "none"
  )+
  xlim(-0.5, 11.5)

# combine the plot and the points' labels
type101_labels = type101 + geom_label_repel(data = label_point_type101,aes(label = sample,
                                    fill = factor(Treatment)), color = 'black',
                                    size = 5)


# combine the plots of wild type and cell type 101
p = ggpubr::ggarrange(wild_labels, type101_labels, # list of plots
                  labels = "AUTO", # labels A, B...
                  font.label = list(size = 20, family = "Times New Roman"),
                  common.legend = T, # common legend
                  legend = "bottom", # legend position
                  align = "h", # align horizontal
                  nrow = 1)  # number of rows

# save the plot
ggsave(
  filename = here::here("figs", "gene_plot_timesNewRoman.pdf"),
  plot = p,
  width = 9,
  height = 6,
  units = "in",
  dpi = 500
)
