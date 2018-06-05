#Note: all functions expect logs with just 3 columns in the format (frame, min:sec, behavior) e.g.:
#37763     20:58.77     z
#37856     21:01.87     C
#38021     21:07.37     a

#USEFUL: Statistics on overall interval distribution across all groups
#mean interval time = 5.556289 seconds
#median interval time = 2.533333 seconds
#standard deviation of interval times = 12.26823
#quantiles 
#       50%       60%       70%       80%       90%       95% 
#  2.533333  3.400000  4.600000  6.600000 11.200000 18.533333

#######################
#######################
#####Interval code#####
#######################
#######################

#################################################################
#####Function to calculate and plot inter-behavior intervals#####
#################################################################
#This function will take a file path and calculate inter-behavioral intervals
#Specific behaviors/behavioral categories can be specified by passing a vector to 'behaviors_to_include'
#'plot_density' will produce a density plot of the distribution of interval lengths
#'plot_time' will plot interval length as a function of behavioral sequence
#'seconds' specifies the amount of time to filter behavioral bouts on
#'return' will return either all intervals ("both"), just inter-bout intervals ("intervals"), or just intervals during bouts ("bouts")
intervals_from_file = function(path_to_file, filter = FALSE, 
                               behaviors_to_include = NULL, 
                               plot_density = FALSE, 
                               plot_time = FALSE,
                               seconds, 
                               return = c("bouts", "intervals", "both")){
  
  log = read.delim(path_to_file, sep = "", header=FALSE)
  #log = as.data.frame(cbind(log[,1], log[,6], log[,11]))
  
  #####Convert frames to seconds#####
  log[,1] = as.numeric(log[,1])/30
  
  #####Remove NAs#####
  log = na.omit(log)
  
  #####Remove bad behaviors#####
  toRemove = c("A", "F", "c", "h", "n", "w", "q", "y", "z")
  log = log[!log[,3]%in%toRemove,]
  
  #####Filter if desired#####
  if(filter == TRUE){
    log = log[log[,3]%in%behaviors_to_include,]
  }else{
    log = log
  }
  
  #####Get intervals#####
  intervals = diff(log[,1])
  
  #####Get bouts and interbout intervals
  bouts = intervals[intervals<seconds]
  interbout_intervals = intervals[intervals>seconds]
  
  #####Plot intervals#####
  if(plot_density == TRUE){
    d = density(intervals)
    plot(d, col = NULL, bty = 'n', main = "", xlim = c(0,100))
    polygon(d, col = "grey80", border = "grey80")
  }
  
  #####Plot time#####
  if(plot_time == TRUE){
    plot(c(1:length(intervals)), intervals, type = 'b', ylab = "Interval length (seconds)",
         xlab = "Behavior", pch = 20, bg = "black", col = "black", bty = 'n')
  }
  
  #####Results#####
  if(return == "bouts"){
    results = list(bouts, mean(bouts), length(bouts))
    names(results) = c("bouts", "mean", "n_behaviors")
  }
  if(return == "intervals"){
    results = list(interbout_intervals, mean(interbout_intervals), length(interbout_intervals))
    names(results) = c("interbout_intervals", "mean", "n_behaviors")
  }
  
  if(return == "both"){
    results = list(intervals, mean(intervals), length(intervals))
    names(results) = c("intervals", "mean", "n_behaviors")
  }
  
  return(results)
}

#############################################################################################
#####Function to calculate and plot inter-behavior intervals for directional transitions#####
#############################################################################################
#This function will take a file path and calculate inter-behavioral intervals for transitions FROM a category of behaviors (e.g. courtship) TO another group (e.g. aggression)
intervals_from_file_directional_behaviors = function(path_to_file, filter = FALSE, 
                                                     behaviors1 = NULL, 
                                                     behaviors2 = NULL,
                                                     plot = FALSE, 
                                                     seconds, 
                                                     return = c("bouts", "intervals", "both")){
  
  log = read.delim(path_to_file, sep = "", header=FALSE)
  #log = as.data.frame(cbind(log[,1], log[,6], log[,11]))
  
  #####Convert frames to seconds#####
  log[,1] = as.numeric(log[,1])/30
  
  #####Remove NAs#####
  log = na.omit(log)
  
  #####Remove bad behaviors#####
  toRemove = c("A", "F", "c", "h", "n", "w", "q", "y", "z")
  log = log[!log[,3]%in%toRemove,]
  
  #####Get all combos of directional behaviors#####
  poss = expand.grid(behaviors1, behaviors2)
  
  #####Identify transitions that match directions in poss#####
  new_log = as.data.frame(matrix(nrow=0, ncol=3))
  for(i in 1:nrow(poss)){
    x = grep(as.character(poss[i,1]), log[,3])
    y = x+1
    tmp = log[y,]
    tmp = tmp[tmp[,3]%in%behaviors2,]
    tmp = log[c((as.numeric(rownames(tmp))-1),
                as.numeric(rownames(tmp))),]
    tmp = tmp[order(as.numeric(rownames(tmp)), decreasing = FALSE),]
    new_log = rbind(new_log, tmp)
    rm(tmp)
  }
  new_log = new_log[order(as.numeric(rownames(new_log))),]
  new_log = new_log[!duplicated(new_log),]
  
  #####Get intervals#####
  intervals = diff(new_log[,1])
  intervals = intervals[!is.na(intervals)]
  
  #####Get bouts and interbout intervals
  bouts = intervals[intervals<seconds]
  interbout_intervals = intervals[intervals>seconds]
  
  #####Plot intervals#####
  if(plot == TRUE){
    d = density(intervals)
    plot(d, col = NULL, bty = 'n', main = "", xlim = c(0,100))
    polygon(d, col = "grey80", border = "grey80")
  }
  
  #####Results#####
  if(return == "bouts"){
    results = list(bouts, mean(bouts), length(bouts))
    names(results) = c("bouts", "mean", "n_behaviors")
  }
  if(return == "intervals"){
    results = list(interbout_intervals, mean(interbout_intervals), length(interbout_intervals))
    names(results) = c("interbout_intervals", "mean", "n_behaviors")
  }
  
  if(return == "both"){
    results = list(intervals, mean(intervals), length(intervals))
    names(results) = c("intervals", "mean", "n_behaviors")
  }
  return(results)
}

################################################################################
#####Function to calculate and plot inter-behavior intervals from directory#####
################################################################################
#Wrapper function for 'intervals_from_file' that calculates intervals from a directory of log files
intervals_from_directory = function(dir,...){
  files = list.files(dir) 
  subjects = gsub(".txt", "", files)
  subjects = gsub("log", "", subjects)
  
  interval_list = list()
  mean_list = list()
  median_list = list()
  n_behaviors = list()
  
  for(i in 1:length(files)){
    print(subjects[i])
    
    n = intervals_from_file(paste(dir, files[i], sep=""),...)[[5]]
    
    if(n>0){
      interval_list[[i]] = intervals_from_file(paste(dir, files[i], sep=""),...)[[1]]
      mean_list[[i]] = intervals_from_file(paste(dir, files[i], sep=""),...)[[2]]
      median_list[[i]] = intervals_from_file(paste(dir, files[i], sep=""),...)[[3]]
      n_behaviors[[i]] = intervals_from_file(paste(dir, files[i], sep=""),...)[[5]]
    }else{
      interval_list[[i]] = 0
      mean_list[[i]] = 0
      median_list[[i]] = 0
      n_behaviors[[i]] = 0
    }
  }
  names(interval_list) = subjects
  names(mean_list) = subjects
  names(median_list) = subjects
  names(n_behaviors) = subjects
  
  return(list(interval_list, mean_list, median_list, n_behaviors))
}

############################################################################################################
#####Function to calculate and plot inter-behavior intervals from directory for directional transitions#####
############################################################################################################
#Wrapper function for 'intervals_from_file_directional_behaviors' that calculates intervals from a directory of log file for directional transitions between behavioral categories
intervals_from_directory_directional_behaviors = function(dir,...){
  files = list.files(dir) 
  subjects = gsub(".txt", "", files)
  subjects = gsub("log", "", subjects)
  
  interval_list = list()
  mean_list = list()
  median_list = list()
  
  for(i in 1:length(files)){
    print(subjects[i])
    
    interval_list[[i]] = intervals_from_file_directional_behaviors(paste(dir, files[i], sep=""),...)[[1]]
    mean_list[[i]] = intervals_from_file_directional_behaviors(paste(dir, files[i], sep=""),...)[[2]]
    median_list[[i]] = intervals_from_file_directional_behaviors(paste(dir, files[i], sep=""),...)[[3]]
  }
  names(interval_list) = subjects
  names(mean_list) = subjects
  return(list(interval_list, mean_list, median_list))
}

##########################################################
#####Function to compare intervals between two groups#####
##########################################################
#Function for calculating statistics from lists of behavioral intervals between two groups
#Interval means per subject are used by default, medians can used instead by specifying 'median = TRUE'
#Default test is Kruskal-wallis; wilcoxon can be used instead by specifying 'wilcoxon = TRUE'
interval_test = function(group1, 
                         group2, 
                         median = FALSE,
                         wilcoxon = FALSE){
  if(median == TRUE){
    g1 = unlist(group1[[3]])
    g2 = unlist(group2[[3]])
  }else{
    g1 = unlist(group1[[2]])
    g2 = unlist(group2[[2]])
  }
  
  if(wilcoxon == TRUE){
    test = wilcox.test(g1, g2)
    return(test)
  }else{
    verboseBoxplot(c(g1, g2), c(rep("group1", length(g1)), rep("group2", length(g2))),
                   notch=FALSE, xlab = "", ylab  = "", bty = 'n')
  }
}

#############################################################################
#####Function to compare intervals from directories with groups provided#####
#############################################################################
#Wrapper function for 'interval_test' to perform all possible comparisons of groups with a list of directories ('dir_list')
#'names' specifies the names of the groups compared
interval_test_directional_from_directory = function(dir_list, names, ...){
  int_list = list()
  
  for(i in 1:length(dir_list)){
    int_list[[i]] = intervals_from_directory_directional_behaviors(dir_list[i],
                                                                   seconds = seconds,
                                                                   behaviors1 = behaviors1,
                                                                   behaviors2 = behaviors2,
                                                                   return = return)
    names(int_list)[[i]] = names[[i]]
  }
  
  combos = combn(names,2)
  
  #Kruskal wallis
  kw_results = as.data.frame(matrix(ncol = 6, nrow = ncol(combos)))
  colnames(kw_results) = c("mean_s1", "mean_s2", "p", "stat", "group1", "group2")
  
  print("performing kruskal wallis test")
  for(i in 1:ncol(combos)){
    test = kruskal.test(list(as.numeric(int_list[names(int_list)%in%combos[1,i]][[1]][[2]]), 
                             as.numeric(int_list[names(int_list)%in%combos[2,i]][[1]][[2]])))
    
    kw_results[i,1] = mean(as.numeric(int_list[names(int_list)%in%combos[1,i]][[1]][[2]]), na.rm = TRUE)
    kw_results[i,2] = mean(as.numeric(int_list[names(int_list)%in%combos[2,i]][[1]][[2]]), na.rm = TRUE)
    kw_results[i,3] = test$p.value
    kw_results[i,4] = test$statistic
    kw_results[i,5] = combos[1,i]
    kw_results[i,6] = combos[2,i]
  }
  
  #Wilcoxon
  wilc_results = as.data.frame(matrix(ncol = 6, nrow = ncol(combos)))
  colnames(wilc_results) = c("mean_s1", "mean_s2", "p", "stat", "group1", "group2")
  
  print("performing kruskal wallis test")
  for(i in 1:ncol(combos)){
    test = wilcox.test(as.numeric(int_list[names(int_list)%in%combos[1,i]][[1]][[2]]), 
                       as.numeric(int_list[names(int_list)%in%combos[2,i]][[1]][[2]]))
    
    wilc_results[i,1] = mean(as.numeric(int_list[names(int_list)%in%combos[1,i]][[1]][[2]]), na.rm = TRUE)
    wilc_results[i,2] = mean(as.numeric(int_list[names(int_list)%in%combos[2,i]][[1]][[2]]), na.rm = TRUE)
    wilc_results[i,3] = test$p.value
    wilc_results[i,4] = test$statistic
    wilc_results[i,5] = combos[1,i]
    wilc_results[i,6] = combos[2,i]
  }
  
  results = list(kw_results, wilc_results)
  names(results) = c("kruskal-wallis", "wilcoxon")
  
  return(results)
}

###################
###################
#####Kmer code#####
###################
###################

################################
#####Behavior kmer function#####
################################
#This function is called by 'kmers_from_file' and 'kmers_from_directory'
behavioralKmers=function(data, Kmer){
  seq=c()
  for (i in 1:length(data)){
    if(i==length(data)){
      break
    }
    temp=paste(data[i:(i+Kmer-1)],collapse="")
    seq=c(seq,temp)
  }
  return(seq[-length(seq)])
}

###############################################
#####Function to calculate kmers from file#####
###############################################
#'seconds' corresponds to interval to split bouts by 
#'kmer' dictates the length of behavioral sequences to look at (integer; e.g. 2,3,4,...)
kmers_from_file = function(path_to_file, seconds, kmer){
  log = read.delim(path_to_file, sep = "", header=FALSE)

  #####Convert frames to seconds#####
  log[,1] = as.numeric(log[,1])/30
  
  #####Remove NAs#####
  log = na.omit(log)
  
  #####Remove bad behaviors#####
  toRemove = c("A", "F", "c", "h", "n", "w", "q", "y", "z")
  log = log[!log[,3]%in%toRemove,]
  
  #####Create an index for time differences > n seconds#####
  idx <- c(0, cumsum(abs(diff(log[,1])) > seconds))
  
  #####Split the data frame#####
  bouts = split(log, idx)
  
  #####Keep just dfs with >kmer rows#####
  bouts = Filter(function(x) dim(x)[1] > kmer-1, bouts)
  
  #####Loop through and count kmers#####
  if(length(bouts)>0){
    bout_strings = c()
    for(i in 1:length(bouts)){
      temp = behavioralKmers(bouts[[i]][,3],kmer)
      bout_strings=c(bout_strings,temp)
    }
    kmer_table = sort(table(bout_strings))
    return(kmer_table)
  }
}

####################################################
#####Function to calculate kmers from directory#####
####################################################
#This function will output a list of kmers of desired length, each element corresponding to a subject
#'dir' specifies directory of log files to process
#'seconds' corresponds to interval to split bouts by 
#'kmer' dictates the length of behavioral sequences to look at (integer; e.g. 2,3,4,...)
kmers_from_directory = function(dir, seconds, kmer){
  files = list.files(dir) 
  subjects = gsub(".txt", "", files)
  subjects = gsub("log", "", subjects)
  
  kmer_list = list()
  for(i in 1:length(files)){
    print(subjects[i])
    #kmer_list = list(kmer_list, kmers_from_file(paste(dir, files[i], sep=""), seconds, kmer))
    test = kmers_from_file(paste(dir, files[i], sep=""), seconds, kmer)
    if(length(test>0)){
      kmer_list[[i]] = test
    }
  }
  names(kmer_list) = subjects
  return(kmer_list)
}

##############################################################
#####Function to create dataframe of kmers for each group#####
##############################################################
#This function will output a dataframe of all observed kmers within the group (rows)
#and the amount of each performed by the subjects (columns)
#'kmer_list' is the output from 'kmers_from_directory'
kmer_df = function(kmer_list){
  kmers = c()
  for(i in 1:length(kmer_list)){
    kmers = c(kmers, names(kmer_list[[i]]))
  }
  kmers = unique(kmers)
  
  kmer_df = as.data.frame(matrix(nrow = length(kmers), ncol = length(kmer_list)))
  rownames(kmer_df) = kmers
  colnames(kmer_df) = names(kmer_list)
  
  for(i in 1:length(kmer_list)){
    tmp = kmer_list[[i]]
    for(j in 1:length(tmp)){
      kmer_df[match(names(tmp[j]), rownames(kmer_df)),i] = tmp[j]
    }
  }
  return(kmer_df)
}

################################################################
#####Kruskal wallis test of kmer amount between two groups######
################################################################
kw_test_for_kmer = function(df1, df2, sig_filter = FALSE){
  common = intersect(rownames(df1), rownames(df2))
  kw_df = as.data.frame(matrix(nrow = length(common), ncol = 5))
  rownames(kw_df) = common
  colnames(kw_df) = c("mean_s1", "mean_s2", "p", "chi-stat", "sig")
  
  for(i in 1:length(common)){
    s1 = as.numeric(df1[match(common[i], rownames(df1)),])
    s2 = as.numeric(df2[match(common[i], rownames(df2)),])
    test = kruskal.test(list(s1, s2))
    
    kw_df[i,1] = mean(s1)
    kw_df[i,2] = mean(s2)
    kw_df[i,3] = test$p.value
    kw_df[i,4] = test$statistic
  }
  kw_df[,5] = rep(NA, nrow(kw_df))
  kw_df[kw_df[,3]<0.05,5] = "sig"
  kw_df = kw_df[order(kw_df[,3]),]
  
  if(sig_filter == TRUE){
    kw_df = na.omit(kw_df[kw_df[,5] == "sig",])
  }else{
    kw_df = kw_df
  }
  return(kw_df)
}

###############################################################################
#####Wilcoxon test for kmers within groups (between morning and afternoon)#####
###############################################################################
wilcoxon_test_for_kmer = function(df1, df2, sig_filter = FALSE){
  common = intersect(rownames(df1), rownames(df2))
  df = as.data.frame(matrix(nrow = length(common), ncol = 5))
  rownames(df) = common
  colnames(df) = c("mean_s1", "mean_s2", "p", "W", "sig")
  
  for(i in 1:length(common)){
    s1 = as.numeric(df1[match(common[i], rownames(df1)),])
    s2 = as.numeric(df2[match(common[i], rownames(df2)),])
    test = wilcox.test(s1, s2)
    
    df[i,1] = mean(s1)
    df[i,2] = mean(s2)
    df[i,3] = test$p.value
    df[i,4] = test$statistic
  }
  df[,5] = rep(NA, nrow(df))
  df[df[,3]<0.05,5] = "sig"
  df = df[order(df[,3]),]
  
  if(sig_filter == TRUE){
    df = na.omit(df[df[,5] == "sig",])
  }else{
    df = df
  }
  return(df)
}

##################################################################################
#####Wrapper for testing all possible combos of groups and outputting into df#####
##################################################################################
#This function will perform Kruskal-wallis and wilcoxon tests on all possible comparisons of groups from a list of directories ('dir_list')
#Example usage:
#dir_list = c("~/Desktop/logfilesBeau/ASCAFT/", "~/Desktop/logfilesBeau/ASCMOR/",
#             "~/Desktop/logfilesBeau/DAFT/", "~/Desktop/logfilesBeau/DMOR/")
#names = c("ASCAFT", "ASCMOR", "DAFT", "DMOR")

#threemer_5.5 = kmer_test_from_directory(dir_list, names, seconds = 5.5, kmer = 3)

kmer_test_from_directory = function(dir_list, names, ...){
  df_list = list()
  
  for(i in 1:length(dir_list)){
    df_list[[i]] = kmer_df(kmers_from_directory(dir_list[[i]], seconds = seconds, kmer = kmer))
    names(df_list)[[i]] = names[[i]]
    df_list[[i]][is.na(df_list[[i]])] = 0
  }

  combos = combn(names(df_list),2)
  
  #Kruskal wallis
  kw_results = as.data.frame(matrix(ncol = 7, nrow = 0))
  colnames(kw_results) = c("mean_s1", "mean_s2", "p", "stat", "sig", "group1", "group2")
  
  print("performing kruskal wallis test")
  for(i in 1:ncol(combos)){
    test = kw_test_for_kmer(df_list[names(df_list)%in%combos[1,i]][[1]], 
                            df_list[names(df_list)%in%combos[2,i]][[1]], 
                            sig_filter = TRUE)
    test[,6] = rep(combos[1,i], nrow(test))
    test[,7] = rep(combos[2,i], nrow(test))
    
    kw_results = rbind(kw_results, test)
  }
  
  #Wilcoxon
  wilc_results = as.data.frame(matrix(ncol = 7, nrow = 0))
  colnames(wilc_results) = c("mean_s1", "mean_s2", "p", "stat", "sig", "group1", "group2")
  
  print("performing wilcoxon test")
  for(i in 1:ncol(combos)){
    test = wilcoxon_test_for_kmer(df_list[names(df_list)%in%combos[1,i]][[1]], 
                                  df_list[names(df_list)%in%combos[2,i]][[1]], 
                                  sig_filter = TRUE)
    test[,6] = rep(combos[1,i], nrow(test))
    test[,7] = rep(combos[2,i], nrow(test))
    
    wilc_results = rbind(wilc_results, test)
  }
  
  results = list(kw_results, wilc_results)
  names(results) = c("kruskal-wallis", "wilcoxon")
  
  return(results)
}


