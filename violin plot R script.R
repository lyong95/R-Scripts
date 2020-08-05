library(tidyverse)
library(ggplot2)



#############################################################
#####   Loading Data, blank subtraction, remove rows   ######
#############################################################


## PLATE 1 ##
########################

# retreive data files for plate 1
data_path = file.choose()

data1 = readxl::read_xlsx(data_path, 
                         skip = 20,
                         col_names=TRUE)

data1 = as.data.frame(data1[, -c(1,14)])


# get plate layout for p1
layout_path = file.choose()

layout1 = readxl::read_xlsx(layout_path)

layout1 = as.data.frame(layout1[,-1])

#create data frame with strain name and corresponding values
df_p1 = data.frame(list(Name=unlist(layout1), Value=unlist(data1)))
df_p1 = df_p1[df_p1$Value != 'OVRFLW',]
df_p1$Value = as.numeric(as.character(df_p1$Value))

#caculate average of blank and subtract blank avg from all values
blank_avg = as.data.frame(df_p1 %>% filter(Name == "Blank")) 
blank_avg$Value = as.numeric(as.character(blank_avg$Value))

blank_avg = mean(blank_avg$Value)

df_p1$Value = df_p1$Value - blank_avg

df_p1 = df_p1 %>% filter(!Name %in% c("Empty", "Blank"))

#Run the line below if you want to remove some strains
df_p1 = df_p1 %>% filter(!df_p1$Name %in%c("NT29012 UTI89", "NT29018 Nissile 1917", "NT29019 ZG17.6", "NT29004 BW25113 CsgD"))


## PLATE 2 ##
########################

# retreive data files for plate 2
data_path = file.choose()

data2 = readxl::read_xlsx(data_path, 
                          skip = 20,
                          col_names=TRUE)

data2 = as.data.frame(data2[, -c(1,14)])


# get plate layout for p2
layout_path = file.choose()

layout2 = readxl::read_xlsx(layout_path)

layout2 = as.data.frame(layout2[,-1])

#create data frame with strain name and corresponding values
df_p2 = data.frame(list(Name=unlist(layout2), Value=unlist(data2)))
df_p2 = df_p2[df_p2$Value != 'OVRFLW',]
df_p2$Value = as.numeric(as.character(df_p2$Value))

#caculate average of blank and subtract blank avg from all values
blank_avg = as.data.frame(df_p2 %>% filter(Name == "Blank")) 
blank_avg$Value = as.numeric(as.character(blank_avg$Value))

blank_avg = mean(blank_avg$Value)

df_p2$Value = df_p2$Value - blank_avg

df_p2 = df_p2 %>% filter(!Name %in% c("Empty", "Blank"))

#Run the line below if you want to remove some strains
df_p2 = df_p2 %>% filter(!df_p2$Name %in%c("NT29012 UTI89", "NT29018 Nissile 1917", "NT29019 ZG17.6", "NT29004 BW25113 CsgD"))


## PLATE 3 ##
########################

# retreive data files for plate 3
data_path = file.choose()

data3 = readxl::read_xlsx(data_path, 
                          skip = 20,
                          col_names=TRUE)

data3 = as.data.frame(data3[, -c(1,14)])


# get plate layout for p3
layout_path = file.choose()

layout3 = readxl::read_xlsx(layout_path)

layout3 = as.data.frame(layout3[,-1])

#create data frame with strain name and corresponding values
df_p3 = data.frame(list(Name=unlist(layout3), Value=unlist(data3)))
df_p3 = df_p3[df_p3$Value != 'OVRFLW',]
df_p3$Value = as.numeric(as.character(df_p3$Value))

#caculate average of blank and subtract blank avg from all values
blank_avg = as.data.frame(df_p3 %>% filter(Name == "Blank")) 
blank_avg$Value = as.numeric(as.character(blank_avg$Value))

blank_avg = mean(blank_avg$Value)

df_p3$Value = df_p3$Value - blank_avg

df_p3 = df_p3 %>% filter(!Name %in% c("Empty", "Blank"))

#Run the line below if you want to remove some strains
df_p3 = df_p3 %>% filter(!df_p3$Name %in%c("NT29012 UTI89", "NT29018 Nissile 1917", "NT29019 ZG17.6", "NT29004 BW25113 CsgD"))


###############################################
#####     Combine Data, Normalisation    ######
###############################################

combiPlate = rbind(df_p1, df_p2, df_p3)

#Gather only the wt strains
wtBW = combiPlate %>%filter(combiPlate$Name %in% "NT29001 BW25113") 

wtAR = combiPlate %>%filter(combiPlate$Name %in% "NT29007 W3110") 

#normalising to median of WT BW25113
normBW = function(x){x/median(wtBW$Value)}

BWStrains = combiPlate %>%filter(combiPlate$Name %in% c("NT29001 BW25113", "NT29003 BW25113 CsgB"))

BWStrains$Value = normBW(BWStrains$Value)

#normalising to median of WT W3110
normAR = function(x){x/median(wtAR$Value)}

ARStrains = combiPlate %>%filter(combiPlate$Name %in% c("NT29007 W3110", "NT29002 AR3110 bscQ+", "NT29006 AR3110 bscQ+ CsgA", "NT29005 AR3110 bscQ+ CsgD"))

ARStrains$Value = normAR(ARStrains$Value)

#combine all normalised values
combiPlateNorm = rbind(BWStrains, ARStrains)



#################################################
#####     Plotting with normalised Data    ######
#################################################

combiPlateNorm$Name = factor(combiPlateNorm$Name, 
                     levels=c("NT29001 BW25113",
                              "NT29003 BW25113 CsgB",
                              "NT29007 W3110",
                              "NT29002 AR3110 bscQ+",
                              "NT29006 AR3110 bscQ+ CsgA",
                              "NT29005 AR3110 bscQ+ CsgD"))


#sort the rows of the data frame according to the strain name in order

#check n for each group
#lapply(combinedPlate, count)


#change "n=" accroding to the number of replicates
combiPlateNorm = cbind(num =c(rep("n=29", nrow(combiPlateNorm))), combiPlateNorm) 

#join the columns of num and name
combiPlateNorm = cbind(unite(combiPlateNorm[,2:1], Strains, sep = " "),Value =combiPlateNorm[,3])


#plot violin plot
ggplot(combiPlateNorm, aes(x=Strains, y=Value )) + #fill = Strains
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE)+
  #scale_fill_brewer(palette = "BuPu")+
  xlab("")+
  ylab(expression('OD'["570nm"])) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme_light()+
  theme(axis.text.x = element_text(hjust = 0.5, size =12))

#plot box plot
ggplot(combiPlateNorm, aes(x=Strains, y=Value )) + #fill = Strains
  geom_boxplot(show.legend = FALSE)+
  #scale_fill_brewer(palette = "BuPu")+
  geom_point()+
  xlab("")+
  ylab(expression('OD'["570nm"])) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme_light()+
  theme(axis.text.x = element_text(hjust = 0.5, size =12))

#get file path so plots are saved in same folder as data
get_basepath = function(path_to_xlsx_file) {
  
  basepath = paste(dirname(path_to_xlsx_file), "/", sep="")
  
  return(basepath)
}

bp = get_basepath(data_path)

#save plot in above file path
ggsave(paste(bp, "CV combined plot_violin.pdf", sep=""), width = 25, height = 10, units = "cm")

