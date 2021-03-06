###################################
#### The aim of this script is to condensate a data.frame by the column choosed by the user
#### 
####    Example of use:
####           Rscript collapsing_table_by_id.R path/2/yourdataframe your_column_2_split /path/resultsfolder
###################################
###################################
#### 0) loading and/or installing required libraries
###################################
if (!require("BiocManager")) {
  install.packages("BiocManager", ask =FALSE)
  library("BiocManager")
}
if (!require("ggplot2")) {
  BiocManager::install("ggplot2", ask =FALSE)
  library("ggplot2")
}
if (!require("argparse")) {
  install.packages("argparse", ask =FALSE)
  library("argparse")
}
if (!require("BBmisc")) {
  install.packages("BBmisc", ask =FALSE)
  library("BBmisc")
}
if (!require("stringi")) {
  install.packages("stringi", ask =FALSE)
  library("stringi")
}
if (!require("stringr")) {
  install.packages("stringr", ask =FALSE)
  library("stringr")
}


#################################
## Data given by the user
#################################
myargs <- commandArgs(trailingOnly = TRUE)

#path_your_data_frame <- "/media/rmejia/ADATA/boba-bk-postsismo/rmejia/Documents/Familiares_y_personas_datos/Zorro/a7yowc9TmLrKqGYyuLwh5o_2021_03_05_16_41_48.csv"
path_your_data_frame <- myargs[1]

#name_of_the_column_to_condensate <- "ID"
name_of_the_column_to_condensate <- myargs[2]

#results_folder_path <- "/media/rmejia/ADATA/boba-bk-postsismo/rmejia/Documents/Familiares_y_personas_datos/Zorro/Results"
results_folder_path <- myargs[3]

################
#### Body of your script
###############
# functions needed by the user 
makeunique <- function(fu){
  f_unique <-lapply(fu,unique)
  return(f_unique)
}
collpase_vector <- function(vec){
  collapsed_vec <-lapply(vec, function(x) paste(x,sep=":",collapse=';'))
  return(collapsed_vec)
}


# Processing some of the data given by the user 
dir.create(results_folder_path)
results_folder_path <- normalizePath(results_folder_path)

#############

my_data_frame <-read.csv(path_your_data_frame) 
my_data_frame[my_data_frame == "n/a"] <- NA

my_data_frame$ID <- as.factor(my_data_frame$ID)

list_of_dfs_by_givencoumn <- split(my_data_frame ,  my_data_frame[,name_of_the_column_to_condensate] ) # splitting according the given feature

list_of_dfs_colums_desintegrated_to_lists <- 
  lapply(list_of_dfs_by_givencoumn , convertColsToList ) # desintegrating the columns

list_of_dfs_colums_desintegrated_to_lists_give_me_unique_entrances <- 
  lapply(list_of_dfs_colums_desintegrated_to_lists,makeunique) # erasing duplicated

lists_df_vectos_collapsed <- 
  lapply(list_of_dfs_colums_desintegrated_to_lists_give_me_unique_entrances , collpase_vector) # collapsing vector to rebuild the dataframe shape

myl <- lists_df_vectos_collapsed 

rows_4df <- lapply( myl, function(y) data.frame(matrix(unlist( y ), ncol=length(y), byrow=FALSE)))
collapsed_df <- do.call(rbind, rows_4df)
colnames(collapsed_df) <- colnames( my_data_frame)

path_to_save <-paste0( results_folder_path,"/",basename(path_your_data_frame),"-condensated-by_",name_of_the_column_to_condensate,".tsv")
write.table(collapsed_df, file=path_to_save, sep="\t", quote=FALSE, row.names = FALSE )

  
  
  


