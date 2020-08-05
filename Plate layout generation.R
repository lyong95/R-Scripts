###--------------------------------------------------------------------------------
setwd("/Users/lyong/Desktop/projects/Pre-screening/Pre-screen 2 - 10-06-2020/")

library(dplyr)
library(tidyverse)
library(ggplot2)


#reads the original file for the list of position numbers and strain names
file <- read.csv2("initial 384 plates.csv", header=TRUE)


#combine the column and row numbers of the 384-well plate to get the position_id
file <- file %>% mutate(position_id = paste(Row.1, Column.1))
layout <- file  %>%  select(ECK.., position_id, Keio.384.Plate.., Row.1, Column.1)
###----------------------------------------------------------------------------------

###generate a table of position numbers that corresponds to the 384-well plate
###----------------------------------------------------------------------------------
rows<- LETTERS[1:16]
cols<- 1:24

layout_tab <- paste(rows[1], cols)

for (i in 2:15)
  layout_tab<-rbind(layout_tab, paste(rows[i], cols))
###------------------------------------------------------------------------


# function to generate layout for plates
###------------------------------------------------------------------------------
gen_layout <- function(Ni_plate){
    
tab <- layout %>% filter(Keio.384.Plate.. == Ni_plate) %>% 
  filter(Column.1 == '1') %>% 
  select(Row.1, ECK..)

for (i in 2:24)
  tab<-tab %>% left_join(layout %>% filter(Keio.384.Plate.. == Ni_plate) %>% 
                   filter(Column.1 == i) %>% 
                   select(Row.1, ECK..), by = 'Row.1')

colnames(tab) <- c(Ni_plate, as.character(1:24))
return(tab)
}

#generate layout for each plate
Ni_P1 <- gen_layout("NT-1")
Ni_P2 <- gen_layout("NT-2")
Ni_P3 <- gen_layout("NT-3")
Ni_P4 <- gen_layout("NT-4")
Ni_P5 <- gen_layout("NT-5")
Ni_P6 <- gen_layout("NT-6")
Ni_P7 <- gen_layout("NT-7")
Ni_P8 <- gen_layout("NT-8")
Ni_P9 <- gen_layout("NT-9")
Ni_P10 <- gen_layout("NT-10")
Ni_P11 <- gen_layout("NT-11")
Ni_P12 <- gen_layout("NT-12")
###---------------------------------------------------------------------


# Generate interative plots for plate layout
###---------------------------------------------------------------------

# function to subset plate and 
int_plot <- function(Ni_plate){
plate <- layout %>% filter(Keio.384.Plate.. == Ni_plate) %>% 
  select(position_id, ECK.., Row.1, Column.1)

row_letter<- LETTERS[1:16]
row_num <- c()

for (i in 1:length(plate$Row.1))
  row_num <- c(row_num, match(plate$Row.1[i], row_letter))

plate <- cbind(plate, row_num) %>%  select(position_id, ECK.., row_num, Column.1)
colnames(plate) <- c("position_id", "Strain", "Row", "Column")


getPalette = colorRampPalette(brewer.pal(12, "Paired"))
colourCount = getPalette(length(unique(plate$Strain)))

layout_map = ggplot(data=plate, aes(x=Column, y=Row)) +
  geom_point(data=expand.grid(seq(1, 24), seq(1, 16)), aes(x=Var1, y=Var2),color="grey90", shape=21) +
  scale_x_continuous(breaks=seq(1, 24)) +
  scale_y_reverse(breaks=seq(1, 16))+
  theme(panel.grid.minor = element_line(size = (0.2), colour="white"), legend.position = "none")+
  labs(title=paste("Layout", Ni_plate ))+
  geom_point(aes(color=Strain),  size = 7)+
  scale_colour_manual(values= colourCount)
  
  
ggplotly(layout_map)
}

Layout1<- int_plot("NT-1")
Layout2<- int_plot("NT-2")
Layout3<- int_plot("NT-3")
Layout4<- int_plot("NT-4")
Layout5<- int_plot("NT-5")
Layout6<- int_plot("NT-6")
Layout7<- int_plot("NT-7")
Layout8<- int_plot("NT-8")
Layout9<- int_plot("NT-9")
Layout10<- int_plot("NT-10")
Layout11<- int_plot("NT-11")
Layout12<- int_plot("NT-12")lo



