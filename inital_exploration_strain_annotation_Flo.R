library(tidyverse)
library(janitor)
library(readxl)



# Data import


iris_files <- list.files(path = "R1-R8 iris output", pattern = "*.iris$", recursive = TRUE, 
                         full.names = TRUE)
# testfile <- iris_files[1]
# testsample <- "R1_10"

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
  tab <- clean_names(tab)
  return(tab)
})


#Something went wrong with this table - it has fewer and different variables. Can 
#you check what happened there?
  

# expected number of columns
n_cols_exptd <- 23
tab[lengths(tab) != n_cols_exptd]

#Remove faulty table and collapse the rest into a data frame.

tab <- tab[lengths(tab) == n_cols_exptd]
tab <- do.call(bind_rows, tab)

#Add strain and condition information as specified by mapping from condition ID to condition name.


# add strain information from provided Excel file
strains <- read_xlsx("Natural isolates new layout.xlsx", range = "D1:G3073")
names(strains) <- c("strain", "plt", "column", "row")
strains$plt <- paste0("R", strains$plt)
tab$row <- LETTERS[tab$row]
tab <- left_join(tab, strains)

#filter(tab2, plt == "R1") %>% pull(strain) %>% table()
chemicals <- c("LB", "EtOH_1.4%", "gluc_0.5%", "gluc_1%", "bipyridyl_75µM", "bipyridyl_200µM", "FeCl3_100µM")

conditions <- tibble(
  cond_id = as.character(3:16), 
  condition = c(
    chemicals, 
    paste(chemicals, "no_salt", sep = "_")
  )
)

(tab <- left_join(tab, conditions))


#Some sanity checks.


unique(tab$condition)


#Some strains are there in 8 replicates? 
  

filter(tab, strain == "NT12235" & condition == "bipyridyl_200µM") %>% 
  select(row, column, file, plt, condition, pos, strain)



#You wrote that there are 600 strains in 4 replicates --> around 2400 strains. With 
#8 384-well plates I would have expected a lot of empty positions (8*384 = 3072). 
#However, there is a readout for every position - reason? 
  
 
length(unique(tab$strain))


## Saving the table


saveRDS(tab, file = "./tab.rds")




# Analysis

## Some quality control

#Focus on "normalized_morphology_score" and "brightness_corrected_size_normalized_color_intensity" 
#first as you said that these are the biofilm phenotype. 


cols_to_keep <- c("row", "column", "normalized_morphology_score", "brightness_corrected_size_normalized_color_intensity", 
                  "file", "sample_id", "plt", "cond_id", "pos", "strain", "condition")
(subtab <- select(tab, all_of(cols_to_keep)))


#Check correlation of these metrics for all conditions. The morphology score apparently needs to be log-transformed. 


ggplot(subtab, aes(x = log(normalized_morphology_score), y = brightness_corrected_size_normalized_color_intensity)) + 
  geom_point() + 
  facet_wrap( ~ condition)


#Could also be interesting to see how well + and - salt conditions correlate (and 
                                                                             later look for mutants that behave differently).


subtab$has_salt <- !grepl(pattern = "no_salt", x = subtab$condition)
unique(subtab$condition[subtab$has_salt])
unique(subtab$condition[!subtab$has_salt])

# We need a "secondary condition id" to match salt and no salt conditions 
uniq_conds <- sort(unique(subtab$condition))
helper_id <- rep(seq_len(length(unique(subtab$condition)) / 2), each = 2)
names(helper_id) <- uniq_conds

subtab$helper_id <- helper_id[subtab$condition]

subtab_salt <- subtab[subtab$has_salt, ]
subtab_no_salt <- subtab[!subtab$has_salt, ]


#The sizes of the tables differ - presumably because of the one plate that was removed 
#before?
  

dim(subtab_salt)
dim(subtab_no_salt)


#Anyway, we can merge the tables together.


join_by <- c("row", "column", "pos", "strain", "plt", "helper_id")

subtab_salt_comp <- inner_join(x = subtab_salt, subtab_no_salt, by = join_by, 
                               suffix = c(".salt", ".no_salt"))

dim(subtab_salt_comp)


#It seems like salt increases the morphology score for the low glucose conditions. For the others the opposite seems to be the case. Also, there is some irregularity in the scores for all but the glucose conditions (note the clusters around 2). 

#A lot of points have some close to 0 (?) values - probably bad colonies. 


p1 <- ggplot(subtab_salt_comp, aes(x = log(normalized_morphology_score.salt), y = log(normalized_morphology_score.no_salt))) + 
  geom_point(alpha = 0.3) + 
  geom_abline(slope = 1, colour = "blue") + 
  coord_fixed() + 
  facet_wrap( ~ condition.salt)

p1

ggsave(filename = "Salt-vs-no_salt_morph-score.pdf", plot = p1, width = 8, height = 8)


#The colony size seems to increase with salt - I guess that makes sense biologically. 


p2 <- p1 %+% aes(x = brightness_corrected_size_normalized_color_intensity.salt, y = brightness_corrected_size_normalized_color_intensity.no_salt)
p2 

ggsave(filename = "Salt-vs-no_salt_size.pdf", plot = p2, width = 8, height = 8)

#Again, we could see some irregularities - are there plate effects? Let's check. We switch back to the original table for that.


plt_eff_hist <- ggplot(tab, aes(x = log(normalized_morphology_score))) + 
  geom_histogram(aes(fill = plt), binwidth = 0.1) + 
  facet_grid(plt ~ condition)

plt_eff_hist

ggsave(filename = "Plt_effects_hist_morph.pdf", plot = plt_eff_hist, width = 30, height = 25)


#The histograms differ a bit. Is this position-dependent? 


plt_eff_pos <- ggplot(tab, aes(fill = log(normalized_morphology_score))) + 
  geom_tile(aes(x = column, y = row)) + 
  facet_grid(plt ~ condition)

plt_eff_pos

ggsave(filename = "Plt_effects_morph-score.pdf", plot = plt_eff_pos, width = 30, height = 18)

plt_eff_pos2 <- plt_eff_pos %+% aes(fill = brightness_corrected_size_normalized_color_intensity)

ggsave(filename = "Plt_effects_size.pdf", plt_eff_pos2, width = 30, height = 18)


