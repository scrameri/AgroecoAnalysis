########################################################################
### Helperfunctions for Analysis of Tanzania Agricultural Experiment ###
########################################################################

## Script Author: 
# Simon Crameri, simon.crameri@usys.ethz.ch, sfcrameri@gmail.com

## Install dependencies
# need.pck <- c("shiny","shinythemes","shinyWidgets","reshape2","ggplot2","ggpubr",
#               "lmerTest","multcomp","plyr","data.table","plotly")# if (any(! need.pck %in% installed.packages())) {
#   for (pckg in need.pck[which(! need.pck %in% installed.packages())]) {
#     cat("installing", pckg, "package...\n")
#     install.packages(pckg)
#   }
# }
# for (pckg in need.pck) {
#   suppressPackageStartupMessages(library(pckg, character.only = TRUE))
#   message("Loading package: ", pckg, " v", packageVersion(pckg))
# }

## FUNCTION to create Boxplots
do.boxplot <- function(d.long, mode = "group", aggfun = mean,
                       wp.levels = NULL, sp.levels = NULL, levelorder = NULL, 
                       ylim = NULL, ytrans = "identity", treatcols, lines = "first", 
                       face.strip.text.x = "plain", face.title = "plain", size.title = 15,
                       face.axis.title = "plain", face.axis.text = "bold",
                       size.strip.text.x = 15, size.axis.title = 15, size.axis.text = 15,
                       stars = NULL) {
  if (nrow(d.long) == 0) stop(errormessage)
  stopifnot(mode %in% c("group", "facet", "perfac"),
            ytrans %in% c("identity", "log", "logst", "sqrt", "asin-sqrt"),
            lines %in% c("first", "connect"))
  ylab <- ifelse(ytrans == "identity", sub("\\(", "\n(", paste0(unique(d.long[,paramname]))),
                 paste0(ytrans, "(", sub("\\(", "\n(", unique(d.long[,paramname])), ")"))
  
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(ggpubr))
  if (is.null(ylim)) ylim <- range(d.long[,valname])
  if (ytrans == "asin-sqrt") {
    d.long[,"ytrans"] <- asin(sqrt(d.long[,valname]))
    ylim <- asin(sqrt(ylim))
  } else {
    d.long[,"ytrans"] <- do.call(ytrans, list(d.long[,valname]))
    if (ytrans == "logst") {
      ylim <- do.call(ytrans, list(ylim, threshold = attr(d.long[,"ytrans"], "threshold")))
    } else {
      ylim <- do.call(ytrans, list(ylim))
    }
  }
  d.long[,"Date"] <- as.Date(paste(d.long[,pername_y], d.long[,pername_m], d.long[,pername_d], sep = "/"), "%Y/%m/%d")
  d.long[,"Date2"] <- as.factor(d.long[,"Date"])
  
  # Create a plot for each season
  plotlist <- list()
  for (season in sort(unique(d.long[,pername_s]))) {
    
    # Get by-season data
    dseason <- d.long[d.long[,pername_s] == season,]
    
    # Averaged over period / months within season (for group mode)
    dmonthavg <- aggregate(dseason[,"ytrans"],
                           by = dseason[,c(locname, paramname, pername, treatname)],
                           FUN = aggfun, na.rm = TRUE)
    names(dmonthavg)[ncol(dmonthavg)] <- "ytrans"
    
    # Averaged over season (for facet mode)
    dseasonavg <- aggregate(dseason[,"ytrans"], 
                            by = dseason[,c(locname, paramname, treatname)],
                            FUN = aggfun, na.rm = TRUE)
    names(dseasonavg)[ncol(dseasonavg)] <- "ytrans"
    
    # Averaged over main factors (for perfac mode)
    dperrep <- aggregate(dseason[,"ytrans"],
                         by = dseason[,c(locname, paramname, pername, pername_y, repname, 
                                         treatname, wp.levels, sp.levels)],
                         FUN = aggfun, na.rm = TRUE)
    names(dperrep)[ncol(dperrep)] <- "ytrans"
    
    dmf <- data.frame(array(data = NA, dim = c(0, ncol(dperrep) + 2),
                            dimnames = list(NULL, c(names(dperrep), "MF", "MFlevel"))))
    for (i in 1:length(c(wp.levels, sp.levels))) {
      dmf <- rbind(dmf, cbind(dperrep,
                              data.frame(MF = rep(c(wp.levels, sp.levels)[i], 
                                                  times = nrow(dperrep)),
                                         MFlevel = dperrep[,c(wp.levels, sp.levels)[i]])))
    }
    
    # Plot
    switch(mode,
      perfac = {
        
        # basic plot (Factorplot)
        p <- ggplot(dmf, aes_string(x = "MF", y = "ytrans", fill = "MFlevel")) +
          geom_boxplot() +
          scale_fill_manual(name = as.character(season), values = c("white", "gray75"), 
                            labels = c("Control","Treatment")) +
          labs(x = NULL, y = ylab) +
          scale_y_continuous(limits = c(ylim[1], ylim[2] + diff(range(ylim))/100)) +
          theme_bw()
        
        # add stars (not facetted over periods within season)
        addedstars <- stars[c(wp.levels, sp.levels),"sign",drop = T]
        addedstarlabs <- character()
        for (j in 1:length(addedstars)) {addedstarlabs <- c(addedstarlabs, "", addedstars[j])}
        p <- p + 
          geom_text(aes_string(x = "MF", y = "y", label = "z"), size = size.axis.text,
                    data = data.frame(MF = rep(c(wp.levels, sp.levels), each = 2), 
                                      y = ylim[2]-diff(range(ylim))/20,
                                      MFlevel = factor(rep(c(0,1), times = length(c(wp.levels, sp.levels)))),
                                      z = addedstarlabs))
      },
      group = {
        
        # basic plot (Meanplot)
        p <- ggplot(dseason, aes_string(treatname, "ytrans")) +
          scale_colour_manual(name = "Date", 
                              values = rev(1:nlevels(droplevels(dseason[,"Date2"])))) +
          scale_fill_manual(guide = "none", values = treatcols) +
          labs(x = NULL, y = ylab) +
          scale_y_continuous(limits = c(ylim[1], ylim[2] + diff(range(ylim))/100)) +
          theme_bw()
        
        # add lines for each date
        if (lines == "connect") {
          for (date in levels(droplevels(dseason[,"Date2"]))) {
            p <- p +
              stat_summary(fun.y = aggfun, geom = "line", group = 1,
                           colour = "darkblue", linetype = 2, lwd = 0.5, 
                           data = subset(dseason, Date2 == date))
          }
        }
        if (lines == "first") {
          datemeans <- dmonthavg[dmonthavg[,treatname] == levelorder[1], "ytrans"]
          p <- p +
            geom_hline(yintercept = datemeans,
                       colour = rev(1:length(datemeans)), 
                       linetype = 2, lwd = 0.5)
        }
        
        # add line averaged across dates (gets messy)
        # if (lines == "connect") {
        #   p <- p +
        #     stat_summary(fun.y = aggfun, geom = "line", group = 1,
        #                  colour = "darkred", lwd = 1.5)
        # }
        # if (lines == "first") {
        #   seasonmeans <- dseasonavg[dseasonavg[,treatname] == levelorder[1], valname]
        #   p <- p +
        #     geom_hline(yintercept = seasonmeans,
        #                colour = "darkred", lwd = 0.5)
        # }
        
        # add boxes
        p <- p +
          geom_boxplot(aes_string(fill = treatname, colour = "Date2"), alpha = 0.75)
        
        # add stars (not facetted over periods within season)
        p <- p + 
          geom_text(aes_string(x = "x", y = "y", label = "z"), size = size.axis.text,
                     data = data.frame(x = levelorder, y = ylim[2]-diff(range(ylim))/10,
                                       z = c("", stars[,"sign",drop = T])))
      },
      facet = {
        
        # basic plot (Boxplot) with lines
        if (lines == "connect") {
          p <- ggplot(dseason, aes_string(treatname, "ytrans")) +
            stat_summary(fun.y = aggfun, geom = "line", group = 1,
                         colour = "darkblue", linetype = 2, lwd = 0.5) + 
            scale_fill_manual(guide = "none", values = treatcols) +
            labs(x = NULL, y = ylab) +
            facet_wrap("Date", nrow = 1) +
            scale_y_continuous(limits = c(ylim[1], ylim[2] + diff(range(ylim))/100)) +
            theme_bw()
        }
        if (lines == "first") {

          p <- ggplot(dseason, aes_string(treatname, "ytrans")) +
            scale_fill_manual(guide = "none", values = treatcols) +
            labs(x = NULL, y = ylab) +
            facet_wrap("Date2", nrow = 1) +
            scale_y_continuous(limits = c(ylim[1], ylim[2] + diff(range(ylim))/100)) +
            theme_bw()
          
          for (date in levels(droplevels(dseason[,"Date2"]))) {
            datemean <- dmonthavg[dmonthavg[,treatname] == levelorder[1] & 
                                    dmonthavg[,pername] == date, "ytrans"]
            if (length(datemean) == 0) datemean <- as.numeric(NA)
            # print(datemean)

            p <- p +
              geom_hline(aes_string(yintercept = "ytrans"),
                         colour = "darkblue", linetype = 2, lwd = 0.5,
                         data = data.frame("Date2" = date,
                                           "ytrans" = datemean))
          }
        }
        
        # add boxes
        p <- p + geom_boxplot(aes_string(fill = treatname), alpha = 0.75)
        
        # add stars (only if there is just one period per season)
        if (length(unique(dseason[,pername])) == 1) {
          p <- p + 
            geom_text(aes_string(x = "x", y = "y", label = "z"), size = size.axis.text,
                      data = data.frame(x = levelorder, y = ylim[2]-diff(range(ylim))/10,
                                        z = c("", stars[,"sign",drop = T])))
        }
      }
    )
    
    # make larger text
    p <- p +
      theme(strip.text.x = element_text(size = size.strip.text.x, face = face.strip.text.x),
            axis.text=element_text(size = size.axis.text, face = face.axis.text),
            axis.title=element_text(size = size.axis.title, face = face.axis.title))
    
    plotlist[[as.character(season)]] <- p
  }
  p <- ggarrange(plotlist = plotlist, nrow = length(unique(d.long[,pername_s])))
  p <- annotate_figure(p = p,
                      top = text_grob(
                        paste0("Location: ", unique(d.long[,locname]),
                               " - Period: ", paste(range(d.long[,pername]), collapse = " - "),
                               " - Season: ", paste(range(d.long[,pername_s]), collapse = " - "),
                               "\nParameter: ", unique(d.long[,paramname]),
                               " - Parameter range: ", paste0(round(range(d.long[,"ytrans"], na.rm = TRUE), 2), collapse = "-"),
                               " (y axis range: ", paste0(round(ylim, 2), collapse = "-"), ")",
                               ifelse(ytrans != "identity", paste0(" (", ytrans, " transformation)"), ""),
                               "\n", attr(stars, "model")),
                        face = face.strip.text.x, size = size.strip.text.x))
  invisible(p)
}

## FUNCTION to create Timeplot
do.timeplot <- function(d.long, ylim = NULL, ytrans = "identity", treatcols) {
  if (nrow(d.long) == 0) stop(errormessage)
  stopifnot(ytrans %in% c("identity", "log", "logst", "sqrt", "asin-sqrt"))
  ylab <- ifelse(ytrans == "identity", paste0(unique(d.long[,paramname])),
                 paste0(ytrans, "(", unique(d.long[,paramname]), ")"))
  
  # aggregate over repetitions
  d.long.agg <- aggregate(d.long[,valname], 
                          by = d.long[,c(locname, pername, pername_y, pername_m, 
                                         pername_d, paramname, treatname)],
                          FUN = aggfun)
  names(d.long.agg)[ncol(d.long.agg)] <- valname
  suppressPackageStartupMessages(library(ggplot2))
  if (ytrans == "asin-sqrt"){
    d.long[,"ytrans"] <- asin(sqrt(d.long[,valname]))
    d.long.agg[,"ytrans"] <- asin(sqrt(d.long.agg[,valname]))
    ylim <- asin(sqrt(ylim))
  } else {
    d.long[,"ytrans"] <- do.call(ytrans, list(d.long[,valname]))
    d.long.agg[,"ytrans"] <- do.call(ytrans, list(d.long.agg[,valname]))
    if (ytrans == "logst") {
      ylim <- do.call(ytrans, list(ylim, threshold = attr(d.long[,"ytrans"], "threshold")))
    } else {
      ylim <- do.call(ytrans, list(ylim))
    }
  }
  d.long[,"Date"] <- as.Date(paste(d.long[,pername_y], d.long[,pername_m], d.long[,pername_d], sep = "/"), "%Y/%m/%d")
  d.long[,"Date2"] <- as.Date(paste(current.year, d.long[,pername_m], d.long[,pername_d], sep = "/"), "%Y/%m/%d")
  d.long.agg[,"Date"] <- as.Date(paste(d.long.agg[,pername_y], d.long.agg[,pername_m], d.long.agg[,pername_d], sep = "/"), "%Y/%m/%d")
  d.long.agg[,"Date2"] <- as.Date(paste(current.year, d.long.agg[,pername_m], d.long.agg[,pername_d], sep = "/"), "%Y/%m/%d")
  
  if (is.null(ylim)) ylim <-range(d.long[,valname], na.rm = TRUE)

  p <- ggplot(d.long, aes_string(x = "Date2", y = "ytrans")) +
    geom_point(aes_string(group = treatname, colour = treatname), alpha = 0.75) +
    # geom_smooth(aes_string(group = treatname, colour = treatname), # gives many loess warnings
    #             method = "loess", formula = "y ~ x", se = FALSE) +
    # stat_summary(fun.y = aggfun, geom = "line", group = treatname, # does not work
    #              colour = treatname, linetype = 2, lwd = 0.5) +
    geom_line(aes_string(group = treatname, colour = treatname),
              lwd = 0.5, data = d.long.agg) +
    scale_y_continuous(limits = ylim) +
    scale_x_date(date_labels = "%B", name = NULL) +
    scale_colour_manual(name = treatname, values = treatcols) +
    labs(x = "", y = ylab) +
    facet_wrap(as.formula(paste("~", pername_s)), ncol = 1) +
    ggtitle(paste0("Location: ", unique(d.long[,locname]), 
                   "\nPeriod: ", paste(range(d.long[,pername]), collapse = " - "),
                   "\nSeason: ", paste(range(d.long[,pername_s]), collapse = " - "),
                   "\nParameter: ", unique(d.long[,paramname]),
                   "\nParameter range: ", paste0(round(range(d.long[,valname], na.rm = TRUE), 2), collapse = "-"),
                   " (y axis range: ", paste0(round(ylim, 2), collapse = "-"), ")",
                   ifelse(ytrans != "identity", paste0(" (", ytrans, " transformation)"), ""))) +
    theme_bw()
  
  # print(p)
  invisible(p)
}

## FUNCTION to carry out ANOVA
do.anova <- function(d.long, mode = "block", 
                     wp.levels = NULL, sp.levels = NULL, levelorder = NULL,
                     modelop = "additive only", ytrans = "identity",
                     p.adjust.method = "single-step",
                     plot = FALSE,
                     print = FALSE) {
  if (nrow(d.long) == 0) stop(errormessage)
  stopifnot(mode %in% c("block", "split-plot"),
            modelop %in% c("additive only", "2-way interactions"),
            ytrans %in% c("identity", "log", "logst", "sqrt", "asin-sqrt"))

  ## Aggregate data (aggfun over months within Season, Season as a block factor)
  d.long[,"Date"] <- as.Date(paste(d.long[,pername_y], d.long[,pername_m], d.long[,pername_d], sep = "/"), "%Y/%m/%d")
  d.long[,"Date2"] <- as.Date(paste(current.year, d.long[,pername_m], d.long[,pername_d], sep = "/"), "%Y/%m/%d")
  d.aov <- aggregate(x = d.long[,valname], 
                     by = d.long[,c(locname, pername, pername_s, paramname, repname, treatname)], ###
                     FUN = aggfun, na.rm = TRUE)
  names(d.aov)[ncol(d.aov)] <- valname
  d.aov <- d.aov[order(d.aov[,locname], d.aov[,pername_s], d.aov[,paramname], d.aov[,treatname], d.aov[,repname]),]
  d.aov[,treatname] <- droplevels(d.aov[,treatname])
  # d.aov[,pername_y] <- factor(as.character(d.aov[,pername_y]))
  d.aov[,pername_s] <- factor(as.character(d.aov[,pername_s]))
  
  ## Prepare results
  res <- list()
  res[["args"]]<- list(as.character(substitute(d.long)), mode, wp.levels, sp.levels,
                       modelop, ytrans, p.adjust.method, plot, print)
  names(res[["args"]]) <- c("d.long", "mode", "wp.levels", "sp.levels", 
                            "modelop", "ytrans", "p.adjust.method",
                            "plot", "print")
  
  ## Carry out ANOVA
  switch(mode,
          
    ### RANDOMIZED COMPLETE BLOCK DESIGN ###
    block = {
      ## Construct model (formula)
      if (ytrans == "asin-sqrt") {
        formula_right <- paste0("asin(sqrt(", valname, ")) ~ ")
      } else {
        formula_right <- paste0(ytrans, "(", valname, ") ~ ")
      }
      modelsign <- switch(modelop,
                          "2-way interactions" = {"*"},
                          "additive only" = {"+"})
      oneperiodperseason <- nlevels(droplevels(interaction(d.aov[,pername], d.aov[,pername_s]))) <= nlevels(d.aov[,pername_s])
      if (nlevels(d.aov[,pername_s]) <= 1) {
        aov <- aov(as.formula(paste0(formula_right, repname, " ", modelsign, " ", treatname)), data = d.aov)
      } else {
        aov <- aov(as.formula(paste0(formula_right, pername_s, "+", " ", repname, " ", modelsign, " ", treatname)), data = d.aov)
      }
     
      ## Post-hoc comparison of means
      # contrast matrix: contrast with control (each line i is levelorder[-1][i] vs. levelorder[1])
      suppressPackageStartupMessages(library(multcomp))
      if (!identical(levels(d.aov[,treatname]), levelorder)) {
        warning("This dataset does not contain all treatment levels in ", paste(levelorder, collapse = ","))
      }
      K <- matrix(rep(c(-1, rep(0, length(levelorder)-1)), times = length(levelorder)-1), 
                  ncol = length(levelorder), byrow = TRUE)
      for (i in seq(nrow(K))) K[i,i+1] <- 1
      rownames(K) <- paste(levelorder[!levelorder %in% control.level], "-", control.level)
      
      glht <- glht(aov, linfct = mcp(Treatment = K))
      
      ## Compile results
      res[["d.aov"]] <- d.aov
      res[["aov"]] <- aov
      res[["K"]] <- K
      res[["glht"]] <- glht
      res[["sum.aov"]] <- summary(aov)
      res[["sum.glht"]] <- summary(glht, test = adjusted(p.adjust.method))
      res[["confint"]] <- confint(glht)$confint # confint(aov)
      
      ## Print results
      if (print) {
        print(res[["sum.aov"]])
        print(res[["sum.glht"]])
      }
    },
    
    ### SPLIT-PLOT DESIGN ###
    "split-plot" = {
      library(lmerTest)
      
      # ## Set contrasts
      # options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")) # default
      # options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
      
      ## Extract the main effects (also done in do.reshape())
      d.aov <- add.mainfac(dd = d.aov, control.level, wp.levels, sp.levels)
      
      ## Construct model (formula)
      if (ytrans == "asin-sqrt") {
        formula_right <- paste0("asin(sqrt(", valname, ")) ~ ")
      } else {
        formula_right <- paste0(ytrans, "(", valname, ") ~ ")
      }
      modelsign <- switch(modelop,
                          "2-way interactions" = {c('(', ')^2 + ')},
                          "additive only" = {c('', ' + ')})
      oneperiodperseason <- nlevels(droplevels(interaction(d.aov[,pername], d.aov[,pername_s]))) <= nlevels(d.aov[,pername_s])
      if (nlevels(d.aov[,pername_s]) <= 1) {
        aov <- lmer(as.formula(paste0(formula_right,
                                      repname, " + ", modelsign[1],
                                      paste(c(wp.levels, sp.levels), collapse = " + "), modelsign[2],
                                      paste(paste0("(1 | ", repname, ":", wp.levels, ")"),
                                            collapse = " + "))),
                    data = d.aov,
                    control = lmerControl(optimizer ="Nelder_Mead")) # https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer
      } else {
        aov <- lmer(as.formula(paste0(formula_right,
                                      pername_s, " + ",
                                      repname, " + ", modelsign[1],
                                      paste(c(wp.levels, sp.levels), collapse = " + "), modelsign[2],
                                      paste(paste0("(1 | ", repname, ":", wp.levels, ")"),
                                            collapse = " + "))), 
                    data = d.aov,
                    control = lmerControl(optimizer ="Nelder_Mead"))
      }
      
      ## Compile results
      res[["d.aov"]] <- d.aov
      res[["aov"]] <- aov
      res[["sum.aov"]] <- summary(aov)
      res[["sum.anova"]] <- anova(aov)
      res[["confint"]] <- confint(aov, oldNames = F)
      
      ## Print results
      if (print) {
        print(res[["sum.anova"]])
      }
    }
  )
  
  ## ANOVA residual analysis
  res[["model"]] <- gsub("  ", "", paste(format(formula(aov)), collapse = ""))
  if (plot) do.resanal(aov = res, 
                       sp.levels = sp.levels, 
                       wp.levels = wp.levels)
  
  ## Return results
  invisible(res)
}

## FUNCTION to create a results table
do.results <- function(aov) {

  mod <- aov$aov
  
  ## Check input
  stopifnot(inherits(mod, c("lmerModLmerTest","aov","lm")))
  modclass <- class(mod)[1]
  
  ## Create results table
  switch(modclass, 
         lmerModLmerTest = {
           dcoef <- data.frame(
             cbind(aov$sum.aov$coefficients[,c(1,2,5)],
                   aov$confint[(grep("sigma", rownames(aov$confint))+1):nrow(aov$confint),])
           )
           names(dcoef) <- c("Estimate", "StdError", "pT",  # "df", "t.value",
                             "ci2.5", "ci97.5")
           dcoef$sign <- ifelse(dcoef[,"pT"] < 0.001, "***",
                                ifelse(dcoef[,"pT"] < 0.01, "**",
                                       ifelse(dcoef[,"pT"] < 0.05, "*",
                                              ifelse(dcoef[,"pT"] < 0.1, ".", ""))))
           dcoef <- data.frame("Level" = rownames(dcoef), dcoef[,c("Estimate","ci2.5","ci97.5","StdError","pT","sign")])
           
           dpval <- data.frame("Factor" = rownames(aov$sum.anova),
                               "pF" = aov$sum.anova[["Pr(>F)"]],
                               row.names = rownames(aov$sum.anova))
           dpval$sign <- ifelse(dpval[,"pF"] < 0.001, "***",
                           ifelse(dpval[,"pF"] < 0.01, "**",
                             ifelse(dpval[,"pF"] < 0.05, "*",
                              ifelse(dpval[,"pF"] < 0.1, ".", ""))))
           dres <- list(PairwiseTtest = dcoef, Ftest = dpval)
         },
         aov = {
           dconf <- confint(aov$glht)$confint
           dcoef <- data.frame(Estimate = dconf[,1],
                               StdError = aov$sum.glht$test$sigma,
                               # df = rep(NA, nrow(dconf)),
                               # t.value = aov$sum.glht$test$tstat,
                               # "pF"= rep(NA, nrow(dconf)),
                               ci2.5 = dconf[,"lwr"],
                               ci97.5 = dconf[,"upr"],
                              "pT" = aov$sum.glht$test$pvalues)
           dcoef$sign <- ifelse(dcoef[,"pT"] < 0.001, "***",
                                ifelse(dcoef[,"pT"] < 0.01, "**",
                                       ifelse(dcoef[,"pT"] < 0.05, "*",
                                              ifelse(dcoef[,"pT"] < 0.1, ".", ""))))
           dcoef <- dcoef[!rownames(dcoef) %in% "(Intercept)",]
           dcoef <- cbind("Contrast" = rownames(dcoef), dcoef[,c("Estimate","ci2.5","ci97.5","StdError","pT","sign")])
           
           dpval <- data.frame("Factor" = rownames(aov$sum.aov[[1]]),
                               "pF" = aov$sum.aov[[1]][,"Pr(>F)"],
                               row.names = rownames(aov$sum.aov[[1]]))
           dpval$sign <- ifelse(dpval[,"pF"] < 0.001, "***",
                                ifelse(dpval[,"pF"] < 0.01, "**",
                                       ifelse(dpval[,"pF"] < 0.05, "*",
                                              ifelse(dpval[,"pF"] < 0.1, ".", ""))))
           dres <- list(PairwiseTtest = dcoef, Ftest = dpval)
         })
  
  ## Return results
  attr(dres, "model") <- aov$model
  return(dres)
}

## FUNCTION to do residual analysis
do.resanal <- function(aov, sp.levels = NULL, wp.levels = NULL) {
  
  mod <- aov$aov
  
  ## Check input
  stopifnot(any(inherits(mod, "lmerModLmerTest"),
                inherits(mod, "aov"),
                inherits(mod, "lm")))
  modclass <- class(mod)[1]
  
  ## ANOVA residual analysis
  par(mfrow=c(2,2), pty = "s")
  
  switch(modclass, 
         lmerModLmerTest = {
           # split plot error
           scatter.smooth(x = fitted(mod), y = resid(mod),
                          lpars = list(col = 2, lwd = 1.5),
                          ylab = "Residuals", xlab = "Fitted values")
           title(main = paste0("Residuals vs Fitted\nsplit-plot factor: ", 
                               paste(sp.levels, collapse = ", ")),
                 font.main = 1, line = 0.5)
           abline(h = 0, lty = 3, col = "gray65")
           
           qqnorm(resid(mod)/sd(resid(mod)), 
                  ylab = "Standardized residuals",
                  main = "")
           title(main = paste0("Normal Q-Q\nsplit-plot factor: ", 
                               paste(sp.levels, collapse = ", ")),
                 font.main = 1, line = 0.5)
           qqline(resid(mod)/sd(resid(mod)), lty = 3, col = "gray65")
           
           scatter.smooth(x = fitted(mod), 
                          y = sqrt(abs(resid(mod)/sd(resid(mod)))),
                          lpars = list(col = 2, lwd = 1.5),
                          ylab = "sqrt(|Standardized residuals|)", xlab = "Fitted values")
           title(main = paste0("Scale-Location\nsplit-plot factor: ", 
                               paste(sp.levels, collapse = ", ")),
                 font.main = 1, line = 0.5)
           
           # whole plot error
           qqnorm(ranef(mod)[[1]][,1],
                  ylab = "Random errors",
                  main = "")
           title(main = paste0("Normal Q-Q\nwhole-plot factor: ", 
                               paste(wp.levels, collapse = ", ")),
                 font.main = 1, line = 0.5)
           qqline(ranef(mod)[[1]][,1], lty = 3, col = "gray65")
         },
         aov = {
           plot(mod)
         })
  par(mfrow=c(1,1), pty = "m")
  mtext(aov$model, line = 3)
}

## FUNCTION  to get summary statistics
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  
  ## Funciton retrieved from: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
  ## Function slightly modified by simon.crameri@env.ethz.ch
  
  ## Summarizes data.
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
  suppressPackageStartupMessages(library(plyr))
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, median, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N      = length2(xx[[col]], na.rm=na.rm),
                     mean   = mean   (xx[[col]], na.rm=na.rm),
                     median = median (xx[[col]], na.rm=na.rm),
                     sd     = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  datac$mean_ciLow <- datac$mean - datac$ci
  datac$mean_ciHigh <- datac$mean + datac$ci
  
  return(datac)
}

## FUNCTION to stop quietly (if there is no data)
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

## FUNCTION to turn character variables to factor variables for data frames
factorize <- function(data, factors) {
  for (i in factors) {
    data[,i] <- as.factor(as.character(data[,i]))
  }
  return(data)
}

## FUNCTION to turn factor or character variables to numerics for data frames
numerize <- function(data, numerics) {
  for (i in numerics) {
    data[,i] <- as.numeric(as.character(data[,i]))
  }
  return(data)
}

## FUNCTION to disentangle treatment combination
disentangle <- function(x, control, levels) {
  x <- as.character(x)
  if (x == control) x <- "C"
  if (nchar(x) > 1) {
    x <- unlist(strsplit(x, split = ""))
  }
  res <- as.numeric(levels %in% x)
  names(res) <- levels
  return(res)
}

## FUNCTION to add disentangled main effects
add.mainfac <- function(dd, control.level, wp.levels, sp.levels) {
  dd <- cbind(dd, 
              t(sapply(X = dd[,treatname], 
                       FUN = disentangle, 
                       control = control.level, levels = c(wp.levels, sp.levels))))
  dd <- factorize(data = dd, factors = c(wp.levels, sp.levels))
  # dd$WP <- as.factor(apply(X = dd[,wp.levels], MARGIN = 1, 
  #                          FUN = function(x) {
  #                            wp <- sum(as.numeric(as.character(x)))
  #                            ifelse(wp > 0, 1, 0)
  #                          }))
  return(dd)
}

## FUNCTION to subset data frames by variable and level(s)
dd.subset <- function(data, variable, levels) {
  dsub <- data[data[,variable] %in% levels, ]
  dsub[] <- lapply(dsub, function(x) if(is.factor(x)) factor(x) else x)
  dsub
}

## FUNCTION to generate a ggplot2 color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## FUNCTION to capitalize the first letter in a string
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## FUNCTION to retrieve Variable index
get.var.level <- function(var, data, start, i) {
  dstart <- grep(start, data[,1])
  var.i <- grep(var, data[,1])
  idx <- max(var.i[var.i < dstart[i]])
  string <- data[idx, 1]
  level <- gsub(paste0(var, ": "), "", string)
  level
}

## FUNCTION to write results
write.results <- function(mod, filename) {
  
  ## Check input
  stopifnot(any(inherits(mod[["aov"]], "lmerModLmerTest"),
                inherits(mod[["aov"]], "aov"),
                inherits(mod[["aov"]], "lm")))
  modclass <- class(mod[["aov"]])[1]
  tit <- paste0("### ANOVA Results ####\n\n", 
                "# Location: ", paste(unique(mod[["d.aov"]][,locname]), collapse = ", "), 
                # " ; Period: ", paste(range(as.numeric(as.character(mod[["d.aov"]][,pername_y]))), collapse = " - "),
                " ; Season: ", paste(unique(mod[["d.aov"]][,pername_s]), collapse = " - "),
                " ; Parameter: ", unique(mod[["d.aov"]][,paramname]))
  blank <- c("\n")
  
  ## Write file
  cat(tit, file = filename)
  cat(blank, file = filename, append = TRUE)
  cat(paste0("# ", mod[["model"]]), file = filename, append = TRUE)
  cat(blank, file = filename, append = TRUE)
  cat(blank, file = filename, append = TRUE)
  switch(modclass,
         "lmerModLmerTest" = {
           capture.output(mod[["sum.anova"]], file = filename, append = TRUE)
           # capture.output(mod[["sum.anova"]], file = filename, append = TRUE)
           cat("\n\n# confidence intervals for effect sizes:\n\n", file = filename, append = TRUE)
           capture.output(mod[["confint"]], file = filename, append = TRUE)
         },
         "aov" = {
           capture.output(mod[["sum.aov"]], file = filename, append = TRUE)
           capture.output(mod[["sum.glht"]], file = filename, append = TRUE)
           cat("\n\n# confidence intervals for effect sizes:\n\n", file = filename, append = TRUE)
           capture.output(mod[["confint"]], file = filename, append = TRUE)
         })
}

## FUNCTION to reshape data
do.reshape <- function(d.wide, paramidx, start.indbased, end.indbased, start.plotbased, end.plotbased) {
  
  # check Parameter
  param <- unique(as.character(d.wide[grep("^Parameter", d.wide[,1]),1]))
  paramstr <- sapply(strsplit(gsub("[(]", "- ", gsub("Parameter: ", "", param)), split = ","), "[", 1)
  paramdat <- subset(paramidx, param == paramstr)
  if (nrow(paramdat) > 1) warning("Parameter is ambiguous!")
  if (nrow(paramdat) < 1) {warning(errormessage) ; stop_quietly()}
  
  # bind data depending on Parameter
  # if (debug) print(paramdat)
  switch(EXPR = as.character(paramdat[,"unit"]),
         "per plant" = {
           dd <- do.bind(dd = d.wide, start = start.indbased, end = end.indbased)
         },
         
         "per plot" = {
           dd <- do.bind(dd = d.wide, start = start.plotbased, end = end.plotbased)
         }
  )
  
  # fix Location
  dd[,locname] <- gsub(" R[1-9].+", "", dd[,locname])
  
  # add Date
  dd <- cbind(dd, array(data = NA, dim = c(nrow(dd), 3), dimnames = list(NULL, c(pername_y, pername_m, pername_d))))
  dd[,pername_y] <- as.numeric(sapply(strsplit(as.character(dd[,pername]), split = "-"), "[", 1))
  dd[,pername_m] <- as.numeric(sapply(strsplit(as.character(dd[,pername]), split = "-"), "[", 2))
  dd[,pername_d] <- as.numeric(sapply(strsplit(as.character(dd[,pername]), split = "-"), "[", 3))
  dd[,pername] <- as.Date(paste(dd[,pername_y], dd[,pername_m], dd[,pername_d], sep = "/"),"%Y/%m/%d")
  
  # factorize factors
  dd <- factorize(dd, factors = c(locname, paramname, repname, idname))
  
  # reorder columns
  dd <- dd[,c(locname, pername, pername_y, pername_m, pername_d, paramname, repname, idname, levelorder)]
  
  # aggregate and sort data
  ppnames <- c(locname, pername, pername_y, pername_m, pername_d, paramname, repname, idname)
  pnames <- ppnames[-which(ppnames == idname)]
  # vnames <- sort(names(dd)[-which(names(dd) %in% ppnames)]) # use levelorder instead of vnames
  
  if (nrow(dd) > 0) {
    dd.agg <- aggregate(dd[,levelorder], by = as.list(dd[,pnames]), FUN = aggfun, na.rm = TRUE)
    dd.sd <- aggregate(dd[,levelorder], by = as.list(dd[,pnames]), FUN = sd, na.rm = TRUE) # take sample sd (n-1 divisor)
    
    # reshape data to long format (for plotting and ANOVA)
    suppressPackageStartupMessages(library(reshape2))
    dd.long <- reshape2::melt(dd, id.vars = pnames, measure.vars = levelorder, 
                              variable.name = treatname, value.name = valname, na.rm = TRUE) # removes empty combinations
    dd.long[,treatname] <- factor(dd.long[,treatname], levels = levelorder)
    
    dd.agg.long <- reshape2::melt(dd.agg, id.vars = pnames, measure.vars = levelorder, 
                                  variable.name = treatname, value.name = valname, na.rm = TRUE)
    dd.agg.long[,treatname] <- factor(dd.agg.long[,treatname], levels = levelorder)
    
    # add main factors to long format (for split-plot ANOVA)
    dd.long <- add.mainfac(dd.long, control.level = control.level, 
                           wp.levels = wp.levels, sp.levels = sp.levels)
    dd.agg.long <- add.mainfac(dd.agg.long, control.level = control.level, 
                               wp.levels = wp.levels, sp.levels = sp.levels)
    
    # reorder data
    dd <- dd[order(dd[,paramname], dd[,locname], dd[,pername], dd[,repname]),]
    rownames(dd) <- seq(nrow(dd))
    
    dd.long <- dd.long[order(dd.long[,paramname], dd.long[,locname], dd.long[,pername], 
                             dd.long[,treatname], dd.long[,repname]),]
    rownames(dd.long) <- seq(nrow(dd.long))
    
    dd.agg <- dd.agg[order(dd.agg[,paramname], dd.agg[,locname], dd.agg[,pername], dd.agg[,repname]),]
    rownames(dd.agg) <- seq(nrow(dd.agg))
    
    dd.sd <- dd.sd[order(dd.sd[,paramname], dd.sd[,locname], dd.sd[,pername], dd.sd[,repname]),]
    rownames(dd.sd) <- seq(nrow(dd.sd))
    
    dd.agg.long <- dd.agg.long[order(dd.agg.long[,paramname], dd.agg.long[,locname],dd.agg.long[,pername],
                                     dd.agg.long[,treatname], dd.agg.long[,repname]),]
    rownames(dd.agg.long) <- seq(nrow(dd.agg.long))
  } else {
    dd.long = data.frame(array(NA, dim = c(0, 13), dimnames = list(NULL, c("Location","Period","Year","Month","Day","Parameter","","Rep","Treatment","Value","S","P","L"))))
    dd.agg = data.frame(array(NA, dim = c(0, 15), dimnames = list(NULL, c("Location","Period","Year","Month","Day","Parameter","Rep","Control","L","P","S","PL","PS","SL","PSL"))))
    dd.sd = data.frame(array(NA, dim = c(0, 13), dimnames = list(NULL, c("Location","Period","Year","Month","Day","Parameter","","Rep","Treatment","Value","S","P","L"))))
    dd.agg.long = data.frame(array(NA, dim = c(0, 15), dimnames = list(NULL, c("Location","Period","Year","Month","Day","Parameter","Rep","Control","L","P","S","PL","PS","SL","PSL"))))
  }

  # return data
  res <- list(wide = dd, long = dd.long, agg = dd.agg, sd = dd.sd, agg_long = dd.agg.long)
  return(res)
}

## FUNCTION to reshape data to long format
do.bind <- function(dd, start, end) {
  
  ## Get starting and ending indices
  dstart <- grep(start, dd[,1])
  dend.all <- grep(end, dd[,1])
  dend <- numeric(length(dstart))
  for (z in dstart) {
    dend[which(dstart==z)] <- min(dend.all[dend.all > z]) #
  }
  
  ## Get Treatment names and create empty data.frame
  treats <- na.omit(unique(as.character(t(dd[dstart[1],-1]))))
  varnames <- c(locname, pername, paramname, repname, treats, idname)
  dd2 <- array(data = NA, dim = c(0, length(varnames)), dimnames = list(NULL, varnames))
  
  ## Loop over concatenated data chunks and fill up empty data.frame
  for (i in seq(length(dstart))) {
    
    ## Check conformity of columns
    # d.check <- ddply(dd[dstart,],names(dd),nrow)
    d.check.test <- as.character(t(dd[dstart,][i,-1]))
    d.check.tab <- unique(table(d.check.test))
    check1 <- all(levelorder %in% d.check.test) # checks if all treatment levels are in data
    if (length(d.check.tab) > 1) {
      check2 <- length(d.check.tab[!d.check.tab %in% 1]) == 1 # checks that all levels have the same nb of repetitions
    } else {
      check2 <- TRUE
    }
    if (all(c(check1, check2))) {
      
      ## Get split-plot data and remove empty lines and columns
      dd.i <- dd[dstart[i]:dend[i],] ; dd.i <- dd.i[which(apply(dd.i, 1, function(x) !all(is.na(x)))),]
      for (col in seq(ncol(dd.i))) {dd.i[dd.i[,col] %in% c(""," "),col] <- NA}
      
      ## Reshape
      colnames(dd.i) <- as.character(t(dd.i[1,])) ; dd.i <- dd.i[-1,]
      rownames(dd.i) <- dd.i[,start] ; dd.i <- dd.i[, -1]
      dd.i <- dd.i[-which(rownames(dd.i) == end),]
      i.empty <- which(apply(dd.i, 2, function(x) sum(is.na(x))) == nrow(dd.i))
      i.torm <- i.empty[c(which(is.na(names(i.empty))), grep("^NA", names(i.empty)))] #
      if (length(i.torm) > 0) dd.i <- dd.i[,-i.torm] #
      treatnames <- colnames(dd.i)
      
      ## Numerize
      dd.i <- numerize(dd.i, numerics = colnames(dd.i))
      
      ## Get whole-plot data 
      dd.i[,locname] <- get.var.level(locname, dd, start, i)
      dd.i[,pername] <- get.var.level(pername, dd, start, i)
      dd.i[,paramname] <- get.var.level(paramname, dd, start, i)
      
      ## Reshape data such that repetitions are in long format
      varying.list <- vector("list", length(treats))
      treatnames.rep <- na.omit(as.character(t(dd[dstart[i],-1])))
      treatnames.rep <- treatnames.rep[!treatnames.rep %in% ""]
      for (j in 1:length(treats)) {
        varying.list[[j]] <- treatnames[treatnames.rep == treats[j][1]] # takes first if ambiguous
      }
      dd.i.long <- reshape(dd.i, direction = "long", varying = varying.list, 
                           timevar = repname, idvar = idname, ids = rownames(dd.i))
      
      # plot-based repetitions are already in long format
      if (start == start.plotbased) dd.i.long[,repname] <- 1:nrow(dd.i.long)
      
      ## Combine data
      dd2 <- rbind(dd2, dd.i.long)
      rownames(dd2) <- 1:nrow(dd2)
      
      ## Remove empty lines (where different replicates have different sample numbers)
      check <- numeric()
      for (i in seq(nrow(dd2))) {
        check <- c(check, all(is.na(dd2[i,levelorder])))
      }
      dd3 <- dd2[check == 0,]
      
    } else {
      if (debug) {
        cat("check1 (all treatment levels in data):", check1, "\n")
        cat("check2 (all treatment levels repeated n times):", check2, "\n")
      }
      warning(paste("SKIPPED DUE TO ISSUE: ", 
                    paste(as.character(t(dd[(dstart[i]-4):(dstart[i]-2),1])), 
                          collapse = " - ")))
      
      dd3 <- dd2
    }
  }

  ## Return combined data
  return(dd3)
}

## FUNCTION to merge treatment levels
mergetreatments <- function(list, merge, mergewith = "Control", fac = "Treatment") {
  ## check input
  stopifnot(is.list(list), names(list) == c("wide","long","agg","sd","agg_long"),
            fac %in% names(list[["long"]]))
  
  if (!merge %in% list[["long"]][,fac]) warning("merge level <", merge, "> not in <", fac, ">")
  if (!mergewith %in% list[["long"]][,fac]) warning("mergewith level <", mergewith, "> not in <", fac, ">")
  
  ## merge treatment levels in long format
  dd.long <- list[["long"]]
  lev <- levels(dd.long[,fac])
  newlevelorder <- unique(sub(merge, "", lev[!lev %in% merge]))
  dd.long[,fac] <- factor(sub(merge, "", gsub(paste0("^", merge, "$"), mergewith, as.character(dd.long[,fac]))),
                          levels = newlevelorder)
  
  # delete column <merge>
  dd.long[,merge] <- NULL
  
  ## wide
  dd.long$iduniq <- apply(dd.long[,!names(dd.long) %in% c(newlevelorder, "Value", "id", "iduniq")], 1, function(x) {paste(x, collapse = "_")})
  dd.long$id <- NA
  dd.long <- dd.long[order(dd.long[,locname], dd.long[,pername], dd.long[,paramname], dd.long[,treatname], dd.long[,repname]),]
  for (i in unique(dd.long$iduniq)) dd.long[dd.long$iduniq == i,"id"] <- 1:nrow(dd.long[dd.long$iduniq == i,])
  dd.long$iduniq <- apply(dd.long[,!names(dd.long) %in% c(newlevelorder, "Treatment", "Value", "iduniq")], 1, function(x) {paste(x, collapse = "_")})
  
  dd.wide <- reshape(dd.long[,!names(dd.long) %in% newlevelorder], v.names = "Value",
                     idvar = "iduniq", timevar = c("Treatment"), direction = "wide")
  dd.wide$iduniq <- NULL
  names(dd.wide) <- gsub("^Value[.]", "", names(dd.wide))
  rownames(dd.wide) <- NULL
  
  ## agg
  pnames <- names(dd.wide)[!names(dd.wide) %in% c(newlevelorder, "id")]
  dd.agg <- aggregate(dd.wide[,newlevelorder], by = as.list(dd.wide[,pnames]), FUN = aggfun, na.rm = TRUE)
  dd.agg <- dd.agg[order(dd.agg[,locname], dd.agg[,pername], dd.agg[,paramname], dd.agg[,repname]),]
  rownames(dd.agg) <- NULL
  
  ## sd
  dd.sd <- aggregate(dd.wide[,newlevelorder], by = as.list(dd.wide[,pnames]), FUN = sd, na.rm = TRUE) # take sample sd (n-1 divisor)
  rownames(dd.sd) <- NULL
  
  ## agg_long
  dd.agg.long <- reshape2::melt(dd.agg, id.vars = pnames, measure.vars = newlevelorder, 
                                variable.name = treatname, value.name = valname, na.rm = TRUE)
  dd.agg.long[,treatname] <- factor(dd.agg.long[,treatname], levels = newlevelorder)
  rownames(dd.agg.long) <- NULL
  
  ## return results
  return(list(wide = dd.wide, long = dd.long, agg = dd.agg, sd = dd.sd, agg_long = dd.agg.long, levelorder = newlevelorder))
}

## FUNCTION to take the logarithm of variables including some zero values
# taken from regr0 package (W. Stahel, ETH Zurich)
logst <- function (data, calib = data, threshold = NULL, mult = 1) 
{
  data <- cbind(data)
  calib <- cbind(calib)
  lncol <- ncol(calib)
  ljthr <- length(threshold) > 0
  if (ljthr) {
    if (is.logical(threshold) && threshold) 
      threshold <- attr(data, "threshold")
    if (!length(threshold) %in% c(1, lncol)) 
      stop("!logst! argument `threshold` is inadequate")
    lthr <- rep(threshold, length = lncol)
    ljdt <- !is.na(lthr)
  }
  else {
    ljdt <- rep(TRUE, lncol)
    lthr <- rep(NA, lncol)
    for (lj in 1:lncol) {
      lcal <- calib[, lj]
      ldp <- lcal[lcal > 0 & !is.na(lcal)]
      if (length(ldp) == 0) 
        ljdt[lj] <- FALSE
      else {
        lq <- quantile(ldp, probs = c(0.25, 0.75), na.rm = TRUE)
        if (lq[1] == lq[2]) 
          lq[1] <- lq[2]/2
        lthr[lj] <- lc <- lq[1]^(1 + mult)/lq[2]^mult
      }
    }
  }
  for (lj in 1:lncol) {
    if (ljdt[lj]) {
      ldt <- data[, lj]
      lc <- lthr[lj]
      li <- which(ldt < lc)
      if (length(li)) 
        ldt[li] <- lc * 10^((ldt[li] - lc)/(lc * log(10)))
      data[, lj] <- log10(ldt)
    }
  }
  if (length(colnames(data))) 
    lnmpd <- names(ljdt) <- names(lthr) <- colnames(data)
  else lnmpd <- as.character(1:lncol)
  if (ncol(data) == 1) 
    data <- data[, 1]
  attr(data, "threshold") <- unname(lthr)
  if (any(!ljdt)) {
    warning(":logst: no positive data", if (lncol > 1) 
      paste(" for variables ", lnmpd[!ljdt], ". These are not transformed")
      else ". No transformation")
    attr(data, "transformed") <- unname(ljdt)
  }
  data
}

### FUNCTION to subset ddinput list
do.subset <- function(list, LOCATION., SUBJECT., PARAMETER.,t1. = NULL,t2. = NULL) {
  stopifnot(names(list) == c("wide","long","agg","sd","agg_long"))
  
  subset. <- function(x, LOCATION, SUBJECT, PARAMETER, t1 = NULL, t2 = NULL) {
    stopifnot(all(c("Location","Parameter","Period") %in% names(x)))
    dsub <- subset(x, Location %in% LOCATION)
    if (!is.null(t1)) dsub <- subset(dsub, Period >= t1)
    if (!is.null(t2)) dsub <- subset(dsub, Period <= t2)
    dsub.p <- dsub[grep(paste0("^",PARAMETER), dsub$Parameter),]
    dsub.s <- dsub.p[grep(paste0("\\(", SUBJECT, ","), dsub.p$Parameter),]
    return(dsub.s)
  }
  
  newlist <- list
  for (i in names(newlist)) {
    newlist[[i]] <- subset.(x = newlist[[i]], LOCATION = LOCATION., 
                            PARAMETER = PARAMETER., SUBJECT = SUBJECT., 
                            t1 = t1., t2 = t2.)
  }
  return(newlist)
}

### FUNCTION to guess period ends
guess.period <- function(x) {
  x <- sort(x)
  l <- strsplit(as.character(x), split = "-")
  d <- data.frame(d = as.numeric(sapply(l, "[", 3)), 
                  m = as.numeric(sapply(l, "[", 2)),
                  y = as.numeric(sapply(l, "[", 1)))
  d$date <- x
  unname(apply(d[diff(d$y) == 1,], 1, function(x) {x[4]}))
}

### FUNCTION to assign periods to seasons
period2season <- function(x, periodbreaks) {
  s <- factor(NA, levels = 1:(length(periodbreaks)+1))
  for (i in seq(length(x))) {
    t <- !c(FALSE, x[i] <= periodbreaks)
    s <- c(s, sum(t))
  }
  return(s[-1])
}

### FUNCTION to transform yields to kg/hectare
div <- function(df1, df2, prefix = "var_") {
  vars <- c("Control","L","P","S","PL","PS","SL","PSL")
  for (i in vars) {
    df1$NEW <- df1[,i] / df2[,i]
    names(df1)[ncol(df1)] <- paste0(prefix, i)
  }
  return(df1)
}

### Functions for colour palettes
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
funky <- function(n) {
  ramp <- grDevices::colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                                        "#33A02C","#FB9A99","#E31A1C",
                                        "#FDBF6F","#FF7F00","#CAB2D6",
                                        "#6A3D9A","#FFFF99","#B15928"))
  ramp(n)
}
angelika <- function(n) {
  ramp <- grDevices::colorRampPalette(c("#FFFFFF","#4F9F3B","#E93E3F",
                                        "#B15928","#FDAC4F","#D1AAB7",
                                        "#72B29C","#A6CEE3"))
  ramp(n)
}