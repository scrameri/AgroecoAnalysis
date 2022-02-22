## Download / Load the complete dataset once (on startup)
if (downloadall) {
  LOCATION = paste(locidx$idx, collapse = ",")
  PARAMETER = paste(sort(paramidx$idx), collapse = ",")
  
  httprequest_all = paste0(datapath, "?l=", LOCATION, "&m1=", first.month, "&y1=", first.year,
                           "&m2=", last.month, "&y2=", current.year, "&p=", PARAMETER)
  
  # download data
  message("downloading data...")
  suppressPackageStartupMessages(library(data.table))
  df <- df.orig2 <- df.orig <- data.frame(fread(httprequest_all, fill = TRUE, sep = "", header = F))[,1]
  
  # check if new data was added
  load("ddinput_all.rda") # ddinput.all, df.orig
  if (all(df.orig %in% df.orig2)) {
    rm(list = c("df.orig2","df.orig"))
    message("locally downloaded database is up to date!")
  } else {
    # get data list
    df <- gsub("R1,R2,R3","R1_R2_R3", gsub("\"","",df)) # 'R1,R2,R3' should not be split by ','
    df[grep("Parameter: ", df)] <- gsub(",", "_", df[grep("Parameter: ", df)]) # 'Maize, mm' etc. should not be split by ','
    dfl <- strsplit(df, split = ",")
    
    # binding data (takes some time)
    message("binding data...")
    d.wide <- array(NA, dim = c(0, max(sapply(dfl, length))))
    for (i in seq(length(dfl))) {
      d.wide <- rbind(d.wide, c(dfl[[i]], rep(NA, ncol(d.wide) - length(dfl[[i]]))))
    }
    d.wide <- data.frame(apply(d.wide, 2, function(x) gsub("_", ",", x)))
    
    starts.section <- grep(start.section, d.wide[,1])
    starts.indbased <- grep(start.indbased, d.wide[,1]) ###
    ends.indbased <- grep(end.indbased, d.wide[,1])
    starts.plotbased <- grep(start.plotbased, d.wide[,1])
    ends.plotbased <- grep(end.plotbased, d.wide[,1])
    
    # binding indbased data (takes some time)
    ddinput_all <- list(wide = NA, long = NA, agg = NA, sd = NA, agg_long = NA)
    for (i in seq(length(starts.indbased))) {
      ddinput_i <- do.reshape(d.wide = d.wide[(max(starts.section[starts.section < starts.indbased[i]])):ends.indbased[i],],
                              paramidx = paramidx,
                              start.indbased = start.indbased, end.indbased = end.indbased,
                              start.plotbased = start.plotbased, end.plotbased = end.plotbased)
      for (j in seq(length(ddinput_all))) {
        ddinput_all[[j]] <- rbind(ddinput_all[[j]], ddinput_i[[j]])
      }
      rm(ddinput_i)
    }
    
    # binding plotbased data (takes some time)
    for (i in seq(length(starts.plotbased))) {
      ddinput_i <- do.reshape(d.wide = d.wide[(max(starts.section[starts.section < starts.plotbased[i]])):(min(ends.plotbased[ends.plotbased > (max(starts.section[starts.section < starts.plotbased[i]]))])),],
                              paramidx = paramidx,
                              start.indbased = start.indbased, end.indbased = end.indbased,
                              start.plotbased = start.plotbased, end.plotbased = end.plotbased)
      for (j in seq(length(ddinput_all))) {
        ddinput_all[[j]] <- rbind(ddinput_all[[j]], ddinput_i[[j]])
      }
      rm(ddinput_i)
    }
    
    # saving data
    df.orig <- df.orig2 
    save(ddinput_all, df.orig, file = "ddinput_all.rda")
    message("Saved complete dataset to <ddinput_all.rda>")
    rm(list = c("df.orig","df.orig2"))
  }
  
} else {
  
  # # download partial dataset
  # ddinput_all <- read.delim(httprequest(), header = F, sep = ",")
  
  # load
  message("Loading <ddinput_all.rda>") 
  load("ddinput_all.rda") # ddinput.all, df.orig
  rm(df.orig)
} # ddinput_all now ready

# be verbose
message("\nLoaded data (wide format) complete - The dimensions are ", paste(dim(ddinput_all$wide), collapse = ","), "\n\n",
        locname, " levels: ",   paste(levels(droplevels(ddinput_all$wide[,locname])), collapse = ","), "\n",
        pername, " range (first and last): ",   paste(sort(unique(ddinput_all$wide[,pername]))[c(1, length(sort(unique(ddinput_all$wide[,pername]))))], collapse = ","), "\n",
        paramname, " levels:\n  ", paste(sort(levels(droplevels(ddinput_all$wide[,paramname]))), collapse = "\n  "), "\n",
        repname, " levels: ",   paste(levels(droplevels(ddinput_all$wide[,repname])), collapse = ","), "\n")


# set UI parameters
periods.all <- sort(unique(ddinput_all$wide[,pername]))
