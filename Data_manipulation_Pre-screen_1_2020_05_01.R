##------ set working directory ------------
setwd("/Users/lyong/Desktop/projects/Pre-screen 1 - 17-03-2020/natural isolates 17-03-2020")

## --- Load Libraries--------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(dplyr)


## ---- message=FALSE-----------------------------------------------------------
iris_files <- list.files(path = "R1-R8 iris output", pattern = "*.iris$", recursive = TRUE, 
  full.names = TRUE)
# testfile <- iris_files[1]
# testsample <- "R1_10"

##-- Reading files and joining all data -----------------------------------------------------------------
# read all iris output files into a list and add sample information
tab <- map(iris_files, function(x) {
  tab <- read_tsv(x, skip = 6, locale = locale(decimal_mark = ","))
  # to extract info like R1_10 etc. from filename
  tab$file <- basename(x)
  # sample_id is of the form RX_YZ (plate number and condition number)
  tab$sample_id <- str_extract(tab$file, "R\\d{1,2}_\\d{1,2}")
  # get plate number and condition number
  tab$plt <- str_extract(tab$sample_id, "R\\d{1,2}")
  tab$cond_id <- str_extract(tab$sample_id, "\\d{1,2}$")
  # get position on plate from 1 to 384
  tab$pos <- seq_len(nrow(tab))
  # get "genotype" - combination of plate number and position
  tab$genotype <- paste(tab$plt, tab$pos, sep = "-")
  tab <- clean_names(tab)
  return(tab)
})


## -----------------------------------------------------------------------------
# expected number of columns
n_cols_exptd <- 24
tab[lengths(tab) != n_cols_exptd]


## -----------------------------------------------------------------------------
tab <- tab[lengths(tab) == n_cols_exptd]
tab <- do.call(bind_rows, tab)


## -----------------------------------------------------------------------------
chemicals <- c("LB", "EtOH_1.4%", "gluc_0.5%", "gluc_1%", "bipyridyl_75µM", "bipyridyl_200µM", "FeCl3_100µM")

conditions <- tibble(
  cond_id = as.character(3:16), 
  condition = c(
    chemicals, 
    paste(chemicals, "no_salt", sep = "_")
    )
)

(tab <- left_join(tab, conditions))


## -----------------------------------------------------------------------------
unique(tab$condition)


## -----------------------------------------------------------------------------
length(unique(tab$genotype))


## -----------------------------------------------------------------------------
cols_to_keep <- c("row", "column", "normalized_morphology_score", "brightness_corrected_size_normalized_color_intensity", 
  "file", "sample_id", "plt", "cond_id", "pos", "genotype", "condition")
(subtab <- select(tab, all_of(cols_to_keep)))



##------------ Attach plate layout to table ---------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
#read plate layout file
setwd("/Users/lyong/Desktop/projects/Pre-screen 1 - 17-03-2020/")

plateLayout = readxl::read_xlsx ("Natural isolates new layout.xlsx")
plateLayout = plateLayout[,-(1:3)]
colnames(plateLayout) = c("strain", "plate", "col", "row")


## -----------------------------------------------------------------------------
#replace all row values to numbers; instead of A,B,Cs, use 1,2,3s

repLetters =list("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")

newRow = vector()


for (i in 1:nrow(plateLayout)){newRow = c(newRow,(which(repLetters %in% plateLayout$row[i])))}

plateLayout = cbind(plateLayout, newRow)

## -----------------------------------------------------------------------------
# replace plt with just numbers (R1 to 1)

plate = unlist(strsplit(tab$plt, split ="R"))
plate = plate[c(which(plate != ""))]

tab$plt = plate

## -----------------------------------------------------------------------------
# attach strain names to the correct row

plateLayout$merged = paste(plateLayout$newRow, plateLayout$col, plateLayout$plate)

tab$merged = paste(tab$row, tab$column, tab$plt)

plate_strain = plateLayout %>%  
  select(strain, merged) 

index = c()

for(i in 1:nrow(tab)){index = c(index, which(plate_strain$merged %in% tab$merged[i]))}


## -----------------------------------------------------------------------------
#for checking purposes
#tab_test = tab %>% select(row, column, plt, sample_id, merged ) %>% cbind (strain = plate_strain$strain[index])


## -----------------------------------------------------------------------------
#attach strain names and filter out colony size 0
tab_remove = tab %>% 
  filter(colony_size == 0) %>% 
  select(merged)

tab_new = tab %>% 
  cbind (strain = plate_strain$strain[index]) %>% 
  arrange(sample_id, condition, row, column) %>% 
  filter(!merged %in% unlist(tab_remove))
 

## -----------------------------------------------------------------------------
#change column names for brightness_corrected_size_normalized_color_intensity, normalized_morphology_score
#iris_colnames = colnames(tab)
#new_colnames = iris_colnames
#new_colnames[c(8,13)] = c("norm_morph", "norm_color")


## -----------------------------------------------------------------------------
#Table with old iris col names and new shorter col names
#tab_colnames = cbind(iris_colnames, new_colnames) 


## -----------------------------------------------------------------------------
#Replace my data table with new shorter col names
#colnames(tab_new) = new_colnames 

## -----------------------------------------------------------------------------
#Visually check top 10% lowest colony size scores with images
#tab_colSorted[1:round(0.1*nrow(tab_colSorted)),-(4:18)]

## -----------------------------------------------------------------------------
cols_to_keep <- c("row", "column", "normalized_morphology_score", "brightness_corrected_size_normalized_color_intensity", 
                "sample_id", "pos", "strain", "genotype", "condition")
(subtab <- select(tab_new, all_of(cols_to_keep)))



## ---------- TIM's CLUSTERING -------------------------------------------------
library(RColorBrewer)
library(factoextra)

transfer_cluster_colors = function(data, from_cond, to_cond, k, n_sd) {
  
  try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
  try(dev.off(),silent=TRUE)
  
  plot.new()
  par(mfrow=c(1,2))
  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  colors = brewer.pal(n = k, name = "Set2")
  
  from_data = subtab %>% filter(condition == from_cond)
  to_data = subtab %>% filter(condition == to_cond)
  
  distances = c()
  
  for (genotype in from_data$genotype) {
    
    dist = ((from_data[from_data$genotype == genotype,]$normalized_morphology_score - 
               to_data[to_data$genotype == genotype,]$normalized_morphology_score)**2 + 
              (from_data[from_data$genotype == genotype,]$brightness_corrected_size_normalized_color_intensity - 
                 to_data[to_data$genotype == genotype,]$brightness_corrected_size_normalized_color_intensity)*2)*0.5
    
    distances = c(distances, dist)
    
  }
  
  to_data %>% add_column(dist = distances)
  
  dist_mean = mean(distances)
  dist_sd = sd(distances)
  
  res.hc = select(from_data, contains("normalized")) %>%
    scale() %>%
    eclust("hclust", k = k, graph = FALSE)
  
  from_colors = c()
  to_colors = c()
  i= 1
  
  for (dist in distances) {
    
    if (dist >= (dist_mean + n_sd * dist_sd)) {
      
      to_colors = c(to_colors, colors[res.hc$cluster[i]])
      
    } else {
      
      to_colors = c(to_colors, "#ffffff00")
      
    }
    
    from_colors = c(from_colors, colors[res.hc$cluster[i]])
    i = i + 1
    
  }
  
  plot(normalized_morphology_score ~ brightness_corrected_size_normalized_color_intensity, 
       data=from_data,
       col="white",
       main=from_cond,
       xlab="Brightness",
       ylab="Morphology")
  
  text(normalized_morphology_score ~ brightness_corrected_size_normalized_color_intensity, 
       # labels=(from_data %>% unite("pos", row:column, remove = FALSE))$pos, 
       labels=from_data$genotype,
       data=from_data, 
       col=from_colors,
       cex=0.9, 
       font=2)
  
  
  
  plot(normalized_morphology_score ~ brightness_corrected_size_normalized_color_intensity, 
       data=to_data,
       col="white",
       main=to_cond,
       xlab="Brightness",
       ylab="Morphology")
  
  text(normalized_morphology_score ~ brightness_corrected_size_normalized_color_intensity, 
       # labels=(todata %>% unite("pos", row:column, remove = FALSE))$pos, 
       labels=to_data$genotype,
       data=to_data, 
       col=to_colors,
       cex=0.9, 
       font=2)
  
}


transfer_cluster_colors(subtab, "LB","EtOH_1.4%" ,5, n_sd=3)
transfer_cluster_colors(subtab, "LB","gluc_0.5%" ,5, n_sd=3)
transfer_cluster_colors(subtab, "LB","gluc_1%",5, n_sd=3)
transfer_cluster_colors(subtab, "LB","bipyridyl_75µM",5, n_sd=3)
transfer_cluster_colors(subtab, "LB","bipyridyl_200µM",5, n_sd=3)
transfer_cluster_colors(subtab, "LB","FeCl3_100µM" ,5, n_sd=3)

transfer_cluster_colors(subtab, "LB_no_salt","EtOH_1.4%_no_salt" ,5, n_sd=3)
transfer_cluster_colors(subtab, "LB_no_salt","gluc_0.5%_no_salt" ,5, n_sd=3)
transfer_cluster_colors(subtab, "LB_no_salt","gluc_1%_no_salt",5, n_sd=3)
transfer_cluster_colors(subtab, "LB_no_salt","bipyridyl_75µM_no_salt",5, n_sd=3)
transfer_cluster_colors(subtab, "LB_no_salt","bipyridyl_200µM_no_salt",5, n_sd=3)
transfer_cluster_colors(subtab, "LB_no_salt","FeCl3_100µM_no_salt" ,5, n_sd=3)



