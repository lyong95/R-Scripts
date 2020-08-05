# install.packages("ggplot2")
# install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

get_basepath = function(path_to_xlsx_file) {
  
  basepath = paste(dirname(path_to_xlsx_file), "/", sep="")
  
  return(basepath)
  
}

data_path = file.choose()

data = readxl::read_xlsx(data_path, 
                 skip = 20,
                 col_names=TRUE)

data = as.data.frame(data[, -c(1,14)])


layout_path = file.choose()

layout = readxl::read_xlsx(layout_path)

layout = as.data.frame(layout[,-1])

data_for_plot = data.frame(list(Name=unlist(layout), Value=unlist(data)))
data_for_plot = data_for_plot[data_for_plot$Value != 'OVRFLW',]
data_for_plot$Value = as.numeric(as.character(data_for_plot$Value))


blank_avg = as.data.frame(data_for_plot %>% filter(Name == "Blank")) 
blank_avg$Value = as.numeric(as.character(blank_avg$Value))

blank_avg = mean(blank_avg$Value)

data_for_plot$Value = data_for_plot$Value - blank_avg

tmp_df = data_for_plot %>% filter(!Name %in% c("Empty", "Blank"))

#Run the line below if you want to remove some strains
tmp_df = tmp_df %>% filter(!tmp_df$Name %in%c("UTI89", "Nissile 1917", "ZG17.6"))

reorder(x = tmp_df$Name, X = tmp_df$Value)

tmp_df$Name = factor(tmp_df$Name, 
                     levels=c("BW25113",
                              "BW25113 CsgB",
                              "BW25113 CsgD",
                              "W3110",
                              "AR3110 bscQ+",
                              "AR3110 bscQ+ CsgA",
                              "AR3110 bscQ+ CsgD"))

ggplot(data = tmp_df, 
       aes(x = Name, y = Value)) +
      # aes(x = reorder(x = tmp_df$Name, X = tmp_df$Value), y = Value)) + #  uncomment if sort by value
  geom_boxplot() +
  xlab("Strains") +
  ylab(expression('OD'["570nm"])) +
  labs(title = "CV plots") +
  scale_y_continuous(breaks=seq(0,1.1*max(data_for_plot$Value),.2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_light()


bp = get_basepath(data_path)

ggsave(paste(bp, "CV 06-03-2020 p1.pdf", sep=""), width = 25, height = 10, units = "cm")

