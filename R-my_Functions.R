#read excel files copied from stackoverflow
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = "text"))
  #if(!tibble) 
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}


#convert to r format path own
convert_to_r_path <- function(){
  
  library(stringr)
  print("what is your window path?")
  winpath <- readline()
  r_path <- gsub("\\\\", "/", winpath)
  #print("here you go")
  return(r_path)
}

#own
append <- function(mydf, n) {
  orig <- mydf
  for (i in 1:n) {
    mydf <- rbind(mydf, orig)
  }
  return(mydf)
}

#imputing NA values using rpaart classification in multiple variables of data frame-own
{
x <- sample(c("aa", "bb", "cc", NA), 5000, replace = TRUE)
y <- sample(c("mm", "nn", "pp", NA), 5000, replace = TRUE)
z <- sample(c("uu", "vv", "ww", NA), 5000, replace = TRUE)
m <- sample(c(1:99, NA), replace = TRUE)
data <- data.frame(x, y, z, m)

for(i in seq_along(colname)) {
  i1 <- which(is.na(data[, colname[i]]))
  fm <- as.formula(paste0(colname[i], "~ ."))
  mod <- rpart(fm, data = data[-i1, ], method = "class")
  pred <- predict(mod, data[i1, ],   type = "class")
  
  data[i1, colname[i]] <- pred
  
}
}


#bar plot to explore Categorical variables with fill option-own
{
  plotBar <- function(myDf,
                      fill,
                      row = 3,
                      col = 3) {
    ls <- list()
    NumIndex <- which(sapply(myDf, is.factor))
    fill <- which(names(myDf) == fill)
    ls <- lapply(NumIndex, Bar, myDf, fill)
    
    marrangeGrob(ls, nrow = row, ncol = col)
  }
  
  Bar <- function(NumIndex, myDf,  fill) {
    myDf %>% ggplot(aes_string(x = names(myDf[, NumIndex]), fill =names(myDf[, fill]))) + geom_bar()
  }
}

#boxplots to explore numerical Vs Categorical variable-own
{
  plotbox <- function(myDf,
                      category,
                      row = 3,
                      col = 3) {
    ls <- list()
    NumIndex <- which(sapply(myDf, is.numeric))
    category <- which(names(myDf) == category)
    ls <- lapply(NumIndex, box, myDf, category)
    
    marrangeGrob(ls, nrow = row, ncol = col)
  }
  
  box <- function(NumIndex, myDf,  category) {
    myDf %>% ggplot(aes_string(y = names(myDf[, NumIndex]), x = names(myDf[, category]))) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}

#borplots to explore  Categorical variable              
{
  plotBar <- function(myDf,row = 3,
                      col = 3) {
    library(gridExtra)
    ls <- list()
    NumIndex <- which(sapply(myDf, is.factor))
    #fill <- which(names(myDf) == fill)
    ls <- lapply(NumIndex, Bar, myDf)
    
    marrangeGrob(ls, nrow = row, ncol = col)
  }
  
  Bar <- function(NumIndex, myDf) {
    myDf %>% ggplot(aes_string(x = names(myDf[, NumIndex]))) + geom_bar() + theme(axis.text.x=element_text(angle = -90, hjust = 0))
  }
}

#plot histigram              
{
  plotNum <- function(myDf,row = 3,
                      col = 3) {
    library(gridExtra)
    
    ls <- list()
    NumIndex <- which(sapply(myDf, is.numeric))
    #fill <- which(names(myDf) == fill)
    ls <- lapply(NumIndex, Hist, myDf)
    
    marrangeGrob(ls, nrow = row, ncol = col)
  }
  
  Hist <- function(NumIndex, myDf) {
    myDf %>% ggplot(aes_string(x = names(myDf[, NumIndex]))) + geom_histogram() + theme(axis.text.x=element_text(angle = -90, hjust = 0))
  }
}

#missing value summary
missing_features <- function(df){
  
  data.frame(missingVal = sapply(lapply(df, is.na), sum)) %>% rownames_to_column("feature") %>%  arrange(desc(missingVal)) %>% filter(missingVal > 0)
  
  }
              
 #plotting char or factor variable with bar along with fill option
{
temp <- loan[, which(sapply(loan, is.character))]
lapply(seq_along(temp), plot_b, temp$loan_status, names(temp), temp)

plot_b <- function(i, loan_status, name, df){
  
df = data.frame(df[, i]) 
  
df %>% 
  ggplot( aes(x = df[, 1], fill = loan_status)) + 
    geom_bar(position = "fill") +
    coord_flip() + 
    theme_light() + xlab(name[[i]])
}
}
              
#merging tables with different column names, provided a mapping table fo the column names. refer merging table excel as data source
{
  path <- "C:/Users/Rabi/Documents/merging_tables.xlsx"
  survey_files <- path %>%
    excel_sheets() %>%
    set_names() %>%
    map(read_excel, path = path)
  
  map_file <- survey_files["map"]
  
  transform_name <- function(x, source, map_file){
    
    temp <- map_file[[1]] %>% filter(Source == source)
    return(temp$mapping[which(temp$Columns %in% x)])
    
  }
  
  for (i in 1:(length(survey_files)-1)){
    colnames(survey_files[[i]]) <-
      sapply(names(survey_files[[i]]), transform_name, names(survey_files[i]), map_file)
  }
}
 
              
 #get simple corr            
 {
   split_arrange_names <- function(x){
  
  return(paste0(sort(unlist(str_split(x, "_"))), collapse = ""))
}

#get simplifeid correlation dataframe
get_corr_df_simple <- function(df, cut_off = NA) {
  
  simpl_df <-
    df %>% select_if(is.numeric) %>% correlate(use = "pairwise.complete.obs") %>%
    pivot_longer(
      -rowname,
      names_to = "features",
      values_to = "corr",
      values_drop_na = T
    ) %>%
    mutate(t1 = paste(rowname, features, sep = "_")) %>%
    mutate(t2 = sapply(t1, split_arrange_names)) %>%
    arrange(t2) %>%
    group_by(t2) %>%
    mutate(rank = row_number(t2)) %>%
    ungroup %>% filter(rank == 1) %>%
    select(rowname, features, corr) %>%
    arrange(desc(abs(corr))) 
  
  if(is.na(cut_off)){
    return(simpl_df)
  }else{
    
    return(simpl_df %>% filter(abs(corr) >= cut_off))
  }
  
}

   }

