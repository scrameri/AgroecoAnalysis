##############################################################
### User Interface for Tanzania Experiment Web Application ###
##############################################################

## App version
# version 1.1

## Script Author: 
# Simon Crameri, simon.crameri@usys.ethz.ch, sfcrameri@gmail.com


##################### Data Query Part ########################
# Libraries (installs any missing libraries)
need.pck <- c("shiny","shinythemes","shinyWidgets","reshape2","ggplot2","ggpubr",
              "lmerTest","multcomp","plyr","data.table","plotly")
library(shinyWidgets)
for (i in need.pck) {
  # if (!i %in% installed.packages()) install.packages(i) # do not use install.package code in shinyapps.io apps
  library(i, character.only = TRUE)
}
options(rsconnect.http = "rcurl")

# Get helperfunctions
source("Helperfunctions.R") # loads helperfunctions into memory

# Get parameters (internal use)
source("GetParameters.R") # definition of internal parameters for data aggregation, analysis and display

# Get complete dataset
source("DownloadDataset.R")

######################## Debugging ###########################
studio <- FALSE
if (debug & studio) {
  # set parameters
  input <- list()
  input$loc = "Chambezi"
  input$subject = "Cassava"
  input$param = "Weight of harvested tubers"
  input$modelmode <- "split-plot"
  input$modelop = "2-way interactions"
  input$ytrans = "identity"
  input$paramrange = NULL
  input$zeros2NA <- FALSE
  input$divisor <- "1"
  input$mergetreatmentlevels <- "L"
  mode = input$modelmode
  modelop = input$modelop
  ylim = input$paramrange
  ytrans = input$ytrans
  lines = "first"
  p.adjust.method = p.adjust
  
  LOCATION = subset(locidx, loc == input$loc)$idx
  PARAMETER = subset(paramidx, param_1 == input$param & param_2 == input$subject)$idx
  input$mrange = c(1,12)
  input$yrange = c(2017,current.year)
  input$trange <- as.Date(c("2017-07-14","2021-12-06"))
  input$periodbreaks = c("2019-03-05","2020-01-18")
  input$periods = c("2019-03-05","2020-01-18","2021-03-03")
  
  # subset data
  ddsub <- do.subset(list = ddinput_all, LOCATION. = input$loc, SUBJECT. = input$subject, PARAMETER. = input$param,
                     t1. = min(periods.all), t2. = max(periods.all))
  
  # determine seasons
  for (i in names(ddsub)) {
    ddsub[[i]][,pername_s] <- period2season(ddsub[[i]][,pername], input$periodbreaks)
  }
  
  # merge treatment levels
  if (input$mergetreatmentlevels %in% levelorder.all) {
    message("\nmerging treatment factor levels...")
    ddmerged <- mergetreatments(list = ddsub, merge = input$mergetreatmentlevels, 
                                mergewith = control.level, fac = treatname)
  } else {
    ddmerged <- ddsub
    ddmerged$levelorder <- levelorder.all
  }
  
  # subsetting dates
  ddinput <- ddmerged
  for (i in names(ddinput)[!names(ddinput) %in% "levelorder"]) {
    ddinput[[i]] <- ddinput[[i]][ddinput[[i]][,pername] %in% as.Date(input$periods, format = "%Y-%m-%d"),]
  }
  
  # drop factor levels
  for (i in names(ddinput)[!names(ddinput) %in% "levelorder"]) {
    for (j in seq(ncol(ddinput[[i]]))) { if (is.factor(ddinput[[i]][,j])) ddinput[[i]][,j] <- droplevels(ddinput[[i]][,j])}
  }
  
  message("dd_wide has dimension ", paste(dim(ddinput$wide), collapse = ","))
  message("dd_long has dimension ", paste(dim(ddinput$long), collapse = ","))
  message("dd_agg_long has dimension ", paste(dim(ddinput$agg_long), collapse = ","))
  
  # divide
  dd_wide <- ddinput$wide
  dd_long <- ddinput$long
  dd_agg_long <- ddinput$agg_long
  ddratio <- list(wide = dd_wide, long = dd_long, agg_long = dd_agg_long)
  
  # anova
  dd_long <- ddratio$long
  dd_agg_long <- ddratio$agg_long
  treatcols <- gg_color_hue(nlevels(dd_long[,treatname]))
  sp.levels <- sp.levels.all[!sp.levels.all %in% input$mergetreatmentlevels]
  wp.levels <- wp.levels.all[!wp.levels.all %in% input$mergetreatmentlevels]
  levelorder <- ddinput$levelorder
  
  aovres <- do.anova(d.long = dd_agg_long, mode = input$modelmode, modelop = input$modelop,
                     wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                     ytrans = input$ytrans, p.adjust.method = p.adjust,
                     plot = T, print = T)
  do.results(aov = aovres)
  do.resanal(aov = aovres, wp.levels = wp.levels, sp.levels = sp.levels)
  print(do.boxplot(d.long = dd_long, mode = "facet", aggfun = aggfun, ylim = input$paramrange,
                   wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                   ytrans = input$ytrans, treatcols = treatcols,
                   stars = do.results(do.anova(d.long = dd_agg_long, mode = "block",
                                               wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                                               modelop = input$modelop, ytrans = input$ytrans,
                                               p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$PairwiseTtest))
  print(do.boxplot(d.long = dd_long, mode = "group", ylim = input$paramrange,
                   wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                   ytrans = input$ytrans, treatcols = treatcols,
                   stars = do.results(do.anova(d.long = dd_agg_long, mode = "block",
                                               wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                                               modelop = input$modelop, ytrans = input$ytrans,
                                               p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$PairwiseTtest))
  print(do.boxplot(d.long = dd_long, mode = "perfac", aggfun = aggfun, ylim = input$paramrange,
                   wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                   ytrans = input$ytrans, treatcols = treatcols,
                   stars = do.results(do.anova(d.long = dd_agg_long, mode = "split-plot",
                                               wp.levels = wp.levels, sp.levels = sp.levels, levelorder = levelorder,
                                               modelop = input$modelop, ytrans = input$ytrans,
                                               p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$Ftest))
  print(do.timeplot(d.long = dd_agg_long, ylim = input$paramrange,
                    ytrans = input$ytrans, treatcols = treatcols))
}


################## Web application Part #####################

##########
### UI ###
##########

ui <- fluidPage(theme = shinythemes::shinytheme("united"),
  
  # App title ----
  titlePanel(title = "Welcome to the Ugunduzi Web App", windowTitle = "Ugunduzi Web App"),
  hr(),
  # print("Please select parameters, then hit the 'Submit' button (twice after changing Subject or Response):"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel = sidebarPanel(width = sidebarwidth,

     actionButton(inputId = "submit",
                  label = "Submit",
                  icon = icon("paper-plane"),
                  width = "100%",
                  style = "color: #fff; background-color: #FE8D19; border-color: #FE8D19"),
                                
     # Input: Drop-down Menu to select Location
     selectInput(inputId = "loc",
                 label = "Location",
                 choices = levels(locidx$loc),
                 selected = levels(locidx$loc)[1],
                 multiple = FALSE,
                 selectize = TRUE,
                 width = NULL,
                 size = NULL),
     
     # # Input: Checkbox Menu to select multiple Location (needs changes to BOXPLOT and ANOVA)
     # checkboxGroupInput(inputId = "loc", 
     #                    label = "Location", 
     #                    choices = levels(locidx$loc),
     #                    selected = levels(locidx$loc)[1], 
     #                    width = NULL),
     
     # Input: Drop-down Menu to select Subject
     selectInput(inputId = "subject", 
                 label = "Subject", 
                 choices = levels(paramidx$param_2),
                 selected = levels(paramidx$param_2)[1], 
                 multiple = FALSE, 
                 selectize = TRUE, 
                 width = NULL, 
                 size = NULL),
     
     # Input: Drop-down Menu to select Response
     uiOutput("param"),
     
     # Input: Drop-down Menu to select Divisor
     uiOutput("divisor"),
     
     # Input: Slider menu to select # planted X per m2
     uiOutput("PerNonLPlot"),
     
     # Input: Slider menu to select # planted X per m2
     uiOutput("PerLPlot"),
     
     # Input: whether to visualize as tonnes / hectare (assuming 144 seeds / plot)
     uiOutput("tonnesperhectare"),
     
     # # Input: Slider-input Widget to select Year range
     # sliderInput(inputId = "yrange",
     #             label = "Period (Years)",
     #             min = first.year,
     #             max = current.year,
     #             value = c(first.year, current.year),
     #             sep = ""),
     # 
     # # Input: Slider-input Widget to select Month range
     # sliderInput(inputId = "mrange",
     #             label = "Period (Months)",
     #             min = first.month,
     #             max = last.month,
     #             value = c(first.month, last.month)),
     # 
     # # Input: Slider-input Widget to select Month range
     # sliderInput(inputId = "trange",
     #             label = "Time Period",
     #             min = min(periods.all),
     #             max = max(periods.all),
     #             step = as.numeric(max(periods.all)-min(periods.all)),
     #             value = c(min(periods.all), max(periods.all))),
     # 
     # # Input: Checkbox to set zero values to NA (removed because all zeros in database are true zeros now)
     # checkboxInput(inputId = "zeros2NA",
     #               label = "Set zero values to NA",
     #               value = FALSE),
     
     # Input: Drop-down Menu to select treatment factors to be combined with control.factor
     selectInput(inputId = "mergetreatmentlevels",
                label = paste0("Merge treatment level with ", control.level),
                # choices = c("no merging", sort(c(wp.levels.all, sp.levels.all))), # only 1 sp.level, do not merge that
                choices = c("no merging", sort(c(wp.levels.all))),
                selected = "no merging",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL),
     
      # Input: Drop-down Menu to select Experimental Design
      selectInput(inputId = "modelmode",
                  label = "Experimental Design",
                  choices = c("block","split-plot"),
                  selected = "split-plot", 
                  multiple = FALSE, 
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL),
      
      # Input: Drop-down Menu to select Interaction effects
      selectInput(inputId = "modelop",
                  label = "Interaction Effects",
                  choices = modelchoices,
                  selected = modelchoices[1], 
                  multiple = FALSE, 
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL),
      
      # Input: Drop-down Menu to select Y Transformation
      selectInput(inputId = "ytrans",
                  label = "Response Transformation",
                  choices = transchoices,
                  selected = transchoices[1], 
                  multiple = FALSE, 
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL),
      
      # Input: Slider-input Widget to select breakpoints after which a new season starts (depends on output)
      # uiOutput("periodbreaks"), # if output$periodbreaks <- renderUI() defined in server
      checkboxGroupInput(inputId = "periodbreaks", label = "Check ends of seasons",
                         selected = NULL,
                         choices = NULL),

      # Input: Slider-input Widget to select periods (depends on output)
      # uiOutput("periods"), # if output$periods <- renderUI() defined in server
      checkboxGroupInput(inputId = "periods", label = "Select dates",
                         selected = NULL,
                         choices = NULL),
     
      # Input: Slider-input Widget to select Y Axis range (depends on output)
      # uiOutput("slider"), # if output$slider <- renderUI() defined in server
      # sliderInput(inputId = "paramrange", label = "Y Axis Range",
      #             min = 0,
      #             max = 1,
      #             value = NULL),
     
      # Input: Color Palette
      selectInput(inputId = "colors",
                  label = "Color Palette",
                  choices = palettes,
                  selected = "angelika", 
                  multiple = FALSE, 
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL),
     
      # Input: Plot and Font sizes for downloaded Plots
     sliderInput(inputId = "fontsize", 
                 label = "Font Size",
                 min = 1,
                 value = 15,
                 max = 25,
                 step = 1,
                 width = NULL),
      sliderInput(inputId = "plotwidth", 
                  label = "Plot Width",
                  min = 1,
                  value = 12,
                  max = 25,
                  step = 1,
                  width = NULL),
     sliderInput(inputId = "plotheight", 
                 label = "Plot Height",
                 min = 1,
                 value = 12,
                 max = 25,
                 step = 1,
                 width = NULL),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel = mainPanel(
    
      # Tabs
      tabsetPanel(
        
        # Output Boxlot ----
        tabPanel("Boxplot I", plotOutput(outputId = "Boxplot", 
                                       width = plotwidth, height = plotheight)), 
        
        # # Output Meanplot ----
        # tabPanel(paste0(firstup(aggfun), "plot"), plotOutput(outputId = "Meanplot", 
        #                                                      width = plotwidth, height = plotheight)), 
        
        # Output Factorplot ----
        tabPanel("Boxplot II", plotOutput(outputId = "Factorplot", 
                                          width = plotwidth, height = plotheight)), 
        
        # # Output Timeplot ----
        # tabPanel("Timeplot", plotly::plotlyOutput(outputId = "Timeplot", 
        #                                   width = plotwidth, height = plotheight)),
        
        # Output ANOVA (text) ----
        tabPanel("ANOVA", verbatimTextOutput("ANOVA")),
        
        # Output ANOVA (table) ----
        tabPanel("Results (Global)", tableOutput("ResultsF")),
        
        # Output ANOVA (table) ----
        tabPanel("Results (Pairwise)", tableOutput("ResultsT")),
        
        # Output Residual Analysis ----
        tabPanel("Residuals",  plotOutput(outputId = "RA", 
                                          width = plotwidth, height = plotheight)),
        
        # Output Data (Raw) ----
        tabPanel("Data (Raw)", dataTableOutput("DataR")),
        
        # Output Data Aggregated over Plots ----
        tabPanel("Data (Means)", dataTableOutput("DataM")),
        
        # Output Data Aggregated over Plots and Months within Years ----
        tabPanel("Data (ANOVA)", dataTableOutput("DataS")),
        
        # Output Data Aggregated over Plots and Months within Years ----
        # tabPanel("Help", tableOutput("Help")) # not needed anymore
      ),
      
      # Download Buttons
      downloadButton("downloadBoxplot", "Boxplot"),
      # downloadButton("downloadMeanplot", "Meanplot"),
      downloadButton("downloadFactorplot", "Factorplot"),
      # downloadButton("downloadTimeplot", "Timeplot"),
      downloadButton("downloadANOVA", "ANOVA"),
      downloadButton("downloadResults", "Results"),
      downloadButton("downloadRA", "Residuals"),
      downloadButton("downloadDataR", "Data (Raw)"),
      downloadButton("downloadDataM", "Data (Means)"),
      downloadButton("downloadDataS", "Data (ANOVA)"),
      downloadButton("downloadDataRDA", "R Data (.rda)")
    ),
    
    # Define positions of sidebar and main panels
    position = c("left", "right")
  ),

  # Footer
  hr(),
  print(paste0("Created by: Simon Crameri (ETH Zurich), Eugenio Tisselli (UNAM), & Angelika Hilbeck (ETH Zurich) (2019-", current.year, ")"))
  
)

##############
### Server ###
##############

shiny_env = new.env()
server <- function(input, output, session) {
  
  ## Subset / reshape data for Location - Subject - Parameter - Period combination
  ddsub <- eventReactive(input$submit, {
    req(ddinput_all)
    
    # subset
    message("\nsubsetting Location, Subject and Response...")
    
    if (debug) message("input$loc is ", paste(input$loc, collapse = ","), 
                       ",\ninput$subject is ", paste(input$subject, collapse = ","), 
                       ",\ninput$param is ", paste(input$param, collapse = ","),
                       ",\ninput$divisor is ", paste(input$divisor, collapse = ","),
                       ",\ninput$yrange is ", paste(input$yrange, collapse = ","),
                       ",\ninput$mrange is ", paste(input$mrange, collapse = ","),
                       ",\ninput$trange is ", paste(input$trange, collapse = ","),
                       ",\nmergetreatmentlevels is ", paste(input$mergetreatmentlevels, collapse = ","),
                       ",\ninput$modelmode is ", paste(input$modelmode, collapse = ","),
                       ",\ninput$modelop is ", paste(input$modelop, collapse = ","),
                       ",\ninput$ytrans is ", paste(input$ytrans, collapse = ","))

    ddsub <- do.subset(list = ddinput_all, LOCATION. = input$loc, SUBJECT. = input$subject, PARAMETER. = input$param,
                       t1. = min(periods.all), t2. = max(periods.all))
    
    if (debug) message("ddsub$wide has dimension ", paste(dim(ddsub$wide), collapse = ","))
    # print(head(ddsub$wide))
    
    # add season according to input$periodbreaks
    for (i in names(ddsub)) {
      ddsub[[i]][,pername_s] <- period2season(ddsub[[i]][,pername], input$periodbreaks)
    }
    return(ddsub)
  })
  
  ## Merge treatment levels
  ddmerged <- eventReactive(input$submit, {
    req(ddsub())
    
    # merge treatment factors according to input$mergetreatmentlevels
    if (input$mergetreatmentlevels %in% levelorder.all) {
      message("\nmerging treatment factor levels...")
      ddmerged <- mergetreatments(list = ddsub(), merge = input$mergetreatmentlevels, 
                                  mergewith = control.level, fac = treatname)
    } else {
      ddmerged <- ddsub()
      ddmerged$levelorder <- levelorder.all
    }
    
    # # set zeros to NA if desired (removed together with ui slider)
    # if (input$zeros2NA) {
    #   message("Set ", sum(ddmerged$long[,valname] == 0), " zero values to NA")
    #   for (i in c("long","agg_long")) {
    #     ddmerged[[i]][,valname][ddmerged[[i]][,valname] == 0] <- NA
    #   }
    # }
    return(ddmerged)
  })

  ## Subset data for selected dates
  ddinput <- eventReactive(input$submit, {
    req(ddmerged()) #; req(input$periods)
    
    ddinput <- ddmerged()

    # select dates
    # message("\nsubsetting dates...")
    # print(periods) # these are not updated, and subsetting results in empty data
    periods.sub <- sort(unique(ddinput$wide[,pername]))
    if (!is.null(input$periods)) {
      if (!all(input$periods %in% periods.sub)) {
        message("\nsubsetting dates...")
        for (i in names(ddinput)[!names(ddinput) %in% "levelorder"]) {
          ddinput[[i]] <- ddinput[[i]][ddinput[[i]][,pername] %in% as.Date(input$periods, format = "%Y-%m-%d"),]
        }
      }
    }
    
    # drop levels
    for (i in names(ddinput)[!names(ddinput) %in% "levelorder"]) {
      for (j in seq(ncol(ddinput[[i]]))) { if (is.factor(ddinput[[i]][,j])) ddinput[[i]][,j] <- droplevels(ddinput[[i]][,j])}
    }
    
    if (debug) message("ddinput$wide has dimension ", paste(dim(ddinput$wide), collapse = ","))
    if (debug) message("dd_long has dimension ", paste(dim(ddinput$long), collapse = ","))
    if (debug) message("dd_agg_long has dimension ", paste(dim(ddinput$agg_long), collapse = ","))
    return(ddinput)
  })
 
  ## Set split-plot and whole-plot levels
  sp.levels <- eventReactive(input$submit, {
    req(ddinput())
    sp.levels <- sp.levels.all[!sp.levels.all %in% input$mergetreatmentlevels]
    if (length(sp.levels) == 0) {
      stop("No split-plot factor present (L)")
    }
    if (debug & !input$mergetreatmentlevels %in% "no merging") message("\nafter merging treatment levels ", paste(input$mertetreatmentlevels, collapse = ","))
    if (debug) message("sp.levels is ", paste(sp.levels, collapse = ","))
    return(sp.levels)
  })
  
  wp.levels <- eventReactive(input$submit, {
    req(ddinput())
    wp.levels <- wp.levels.all[!wp.levels.all %in% input$mergetreatmentlevels]
    if (length(wp.levels) == 0) {
      stop("No whole-plot factor present (S,P)")
    }
    if (debug) message("wp.levels is ", paste(wp.levels, collapse = ","))
    return(wp.levels)
  })
  
  ## Set treatment level order
  levelorder <- eventReactive(input$submit, {
    req(ddinput())
    levelorder <- ddinput()$levelorder
    if (debug) message("levelorder is ", paste(levelorder, collapse = ","))
    return(levelorder)
  })
  
  ## Set seed density
  PerNonLPlot <- eventReactive(input$submit, {
    if (is.null(input$PerNonLPlot)) {
      switch(input$subject,
             "Cassava" = c(10),
             "Maize" = c(200),
             "Pulses" = c(350)
      )
    } else {
      input$PerNonLPlot
    }
  })
  PerLPlot <- eventReactive(input$submit, {
    if (is.null(input$PerLPlot)) {
      switch(input$subject,
             "Cassava" = c(10),
             "Maize" = c(130),
             "Pulses" = c(350)
      )
    } else {
      input$PerLPlot
    }
  })
  
  ## Create target variable ratios
  ddratio <- eventReactive(input$submit, {
    req(ddinput()) ; req(sp.levels()) ; req(wp.levels()) ; req(levelorder())
    
    ddinput <- ddinput()
    dd_wide <- ddinput$wide
    dd_long <- ddinput$long
    dd_agg_long <- ddinput$agg_long
    metanames <- c(locname,pername,repname,idname)
    seedorstick <- ifelse(input$subject == "Cassava", "stick", "seed")
    
    nonl <- levelorder()[!levelorder() %in% L.levels]
    l <- levelorder()[levelorder() %in% L.levels]
    
    if (debug) message("tonnesperhectare() is ", tonnesperhectare())
    if (debug) message("divisor() is ", divisor())
    
    # divide input$param by divisor()
    if (divisor() != "1") {
      
      if (!grepl("OWN INPUT", divisor())) {
        
        # fetch <divisor()> data
        divparam <- as.character(subset(paramidx, param_2 == input$subject & param3 == divisor())$param_1)
        dddiv <- do.subset(list = ddinput_all, LOCATION. = input$loc, SUBJECT. = input$subject, PARAMETER. = divparam,
                           # t1. = input$trange[1], t2. = input$trange[2],
                           t1. = min(periods.all), t2. = max(periods.all))
        
        if (nrow(dddiv$long) == 0) {
          
          stop("No data found for <", divisor(), ">, please use another divisor or divisor = 1.")
          
        } else { # division by # planted/survived X
          
          # match dd_long periods and dddiv periods
          # the dddiv$Period that is closest to dd_long$Period yields is assigned the same date as dd_long$Period
          divdays <- unique(dddiv$wide[,pername])
          breakdays <- as.Date(input$periodbreaks)
          max.before <- 1000
          divdays2 <- as.Date(sapply(divdays, function(x) {
            d <- breakdays - x # negative: x after harvest; positive: x before harvest
            d[d < 0] <- NA
            if (length(na.omit(d[d >= 0])) == 0) {
              NA
            } else {
              if (as.numeric(min(d, na.rm = T)) < max.before) {
                as.character(breakdays[which.min(d)])
              } else {
                NA
              }
            }
          }))
          d.match <- data.frame(Period.div = divdays, Period.harvest = divdays2)
          rownames(d.match) <- d.match$Period.div
          
          # if two Period.div match the same Period.harvest, take the closer one and set the other to NA
          d.match$Days.before <- d.match$Period.harvest - d.match$Period.div
          for (i in seq(nrow(d.match))) {
            t <- d.match[d.match$Period.harvest %in% d.match$Period.harvest[i],]
            if (nrow(t) > 1 & !is.na(unique(t$Period.harvest))) {
              t[-which.min(t$Days.before),"Period.harvest"] <- NA
              d.match[d.match$Period.harvest %in% d.match$Period.harvest[i],] <- t
            }
          }
          
          # assign harvest period to each divisor measurement period
          print(data.frame(na.omit(d.match)))
          
          for (i in names(ddinput)[!names(ddinput) %in% "levelorder"]) {
            dddiv[[i]][,"Period_orig"] <- dddiv[[i]][,pername]
            dddiv[[i]][,pername] <- d.match[as.character(dddiv[[i]][,pername]),"Period.harvest"]
            dddiv[[i]] <- dddiv[[i]][!is.na(dddiv[[i]][,pername]),]
          }
          
          # check conformity of dd_long periods and dddiv periods
          if (!all(as.Date(input$periodbreaks) %in% dddiv$wide[,pername])) {
            
            stop(paste0("The selected periods (", paste(breakdays, collapse = ","),
                        ") do not match the periods for <", divisor(), "> (",
                        paste(sort(unique(as.character(dddiv$long[,pername]))), collapse = ","),
                        "), please use another divisor or divisor = 1."))
            
          } else {
            
            if (nrow(dddiv$wide) == 0) {
              stop(errormessage)
            }
            
            # attach season
            for (i in names(dddiv)) {
              dddiv[[i]][,pername_s] <- period2season(dddiv[[i]][,pername], input$periodbreaks)
            }
            
            # merge treatment levels
            if (input$mergetreatmentlevels %in% levelorder.all) {
              dddiv <- mergetreatments(list = dddiv, merge = input$mergetreatmentlevels, 
                                       mergewith = control.level, fac = treatname)
            } else {
              dddiv$levelorder <- levelorder.all
            }
            
            # drop factor levels
            for (i in names(dddiv)[!names(dddiv) %in% "levelorder"]) {
              for (j in seq(ncol(dddiv[[i]]))) { if (is.factor(dddiv[[i]][,j])) dddiv[[i]][,j] <- droplevels(dddiv[[i]][,j])}
            }
            
            # check conformity
            stopifnot(
              all.equal(nrow(dd_wide), nrow(dddiv$wide)),
              all.equal(nrow(dd_long), nrow(dddiv$long)),
              all.equal(nrow(dd_agg_long), nrow(dddiv$agg_long))
            )
            for (i in c(locname, pername, repname, treatname)) { # season groups may have different names
              stopifnot(
                all.equal(dd_long[,repname], dddiv$long[,repname]),
                all.equal(dd_long[,repname], dddiv$long[,repname]),
                all.equal(dd_agg_long[,repname], dddiv$agg_long[,repname])
              )
            }
            
            # divide <valname> by <divisor()>
            if (all.equal(as.matrix(dd_wide[,metanames]), as.matrix(dddiv$wide[,metanames]), check.attributes = FALSE)) {
              dd_wide[,levelorder()] <-  dd_wide[,levelorder()] / dddiv$wide[,levelorder()]
            } else {
              warning(paste0("The selected periods (", paste(breakdays, collapse = ","),
                             ") do not match the periods for <", divisor(), "> (",
                             paste(sort(unique(as.character(dddiv$long[,pername]))), collapse = ","),
                             ") in the WIDE data, reverting to divisor = 1."))
            }
            dd_long[,valname] <- dd_long[,valname] / dddiv$long[,valname]
            dd_agg_long[,valname] <- dd_agg_long[,valname] / dddiv$agg_long[,valname]
            
            # rename <paramname>
            dd_wide[,paramname] <- gsub("  ", " ", sub("([plant|seed|tuber|stick])s", "\\1", sub(" per plot\\)$", ")", sub("kg per plot", paste0("kg per ", sub(" seeds ", " seed ", sub(" sticks ", " stick ", sub(" plants ", " plant ", sub("Number of", "", divisor()))))), dd_wide[,paramname]))))
            dd_long[,paramname] <- gsub("  ", " ", sub("([plant|seed|tuber|stick])s", "\\1", sub(" per plot\\)$", ")", sub("kg per plot", paste0("kg per ", sub(" seeds ", " seed ", sub(" sticks ", " stick ", sub(" plants ", " plant ", sub("Number of", "", divisor()))))), dd_long[,paramname]))))
            dd_agg_long[,paramname] <- gsub("  ", " ", sub("([plant|seed|tuber|stick])s", "\\1", sub(" per plot\\)$", ")", sub("kg per plot", paste0("kg per ", sub(" seeds ", " seed ", sub(" sticks ", " stick ", sub(" plants ", " plant ", sub("Number of", "", divisor()))))), dd_agg_long[,paramname]))))
          }
        }
      } else { # divide by custom (OWN INPUT)
        
        if (debug) message("PerNonLPlot() is ", PerNonLPlot())
        if (debug) message("PerLPlot() is ", PerLPlot())
        
        # divide <valname> by custom # per Non-L plot
        dd_wide[,nonl] <-  dd_wide[,nonl] / PerNonLPlot()
        dd_long[dd_long[,treatname] %in% nonl,valname] <- dd_long[dd_long[,treatname] %in% nonl,valname] / PerNonLPlot()
        dd_agg_long[dd_agg_long[,treatname] %in% nonl,valname] <- dd_agg_long[dd_agg_long[,treatname] %in% nonl,valname] / PerNonLPlot()
        
        # divide <valname> by custom # per L plot
        dd_wide[,l] <-  dd_wide[,l] / PerLPlot()
        dd_long[dd_long[,treatname] %in% l,valname] <- dd_long[dd_long[,treatname] %in% l,valname] / PerLPlot()
        dd_agg_long[dd_agg_long[,treatname] %in% l,valname] <- dd_agg_long[dd_agg_long[,treatname] %in% l,valname] / PerLPlot()
        
        # rename <paramname>
        dd_wide[,paramname] <- sub("kg per plot", paste0("kg per planted ", seedorstick, "\n[", round(PerNonLPlot()/plot.area), "|", round(PerLPlot()/plot.area),  " ", seedorstick, "/m2]"), dd_wide[,paramname])
        dd_long[,paramname] <- sub("kg per plot", paste0("kg per planted ", seedorstick, "\n[", round(PerNonLPlot()/plot.area), "|", round(PerLPlot()/plot.area),  " ", seedorstick, "/m2]"), dd_long[,paramname])
        dd_agg_long[,paramname] <- sub("kg per plot", paste0("kg per planted ", seedorstick, "\n[", round(PerNonLPlot()/plot.area), "|", round(PerLPlot()/plot.area),  " ", seedorstick, "/m2]"), dd_agg_long[,paramname])
      }
    }
    # kg / plot in tonnes per hectare
    if (!is.null(tonnesperhectare())) {
      if (tonnesperhectare()) {
        
        # plot.area <- input$plotArea # 18 # plot area in m2, fixed in GetParameters
        # plots.per.hectare <- 10000 / plot.area # 10000 (m2 per hectare) / 18 (m2 per plot), fixed in GetParameters
        # PerNonLm2 <- input$PerNonLPlot / plot.area
        # PerLm2 <- input$PerLPlot / plot.area
        
        if (any(grepl(" planted ", divisor()))) {
          ## use this to convert kg/seed to t/ha
          # kg/seed * t/kg * seed/plot * plot/ha = t/ha
          # kg/seed * t/kg * seed/m2 * m2/plot * plot/ha = t/ha
          convertfacNonL <- 1E-03*(PerNonLPlot())*plots.per.hectare
          # convertfacNonL <- 1E-03*(PerNonLm2*plot.area)*plots.per.hectare
          convertfacL <- 1E-03*(PerLPlot())*plots.per.hectare
          # convertfacL <- 1E-03*(PerLm2*plot.area)*plots.per.hectare
        } else {
          # use this to convert kg/plot to t/ha
          # kg/plot * t/kg * plot/ha = t/ha
          convertfacNonL <- 1E-03*plots.per.hectare
          convertfacL <- 1E-03*plots.per.hectare
        }
        if (debug) message("Convert factor: ", round(convertfacNonL, 3))
        if (debug) message("Convert factor: ", round(convertfacL, 3))
        
        # NonL convert to t / ha
        dd_wide[,nonl] <-  dd_wide[,nonl] * convertfacNonL
        dd_long[dd_long[,treatname] %in% nonl,valname] <- dd_long[dd_long[,treatname] %in% nonl,valname] * convertfacNonL
        dd_agg_long[dd_agg_long[,treatname] %in% nonl,valname] <- dd_agg_long[dd_agg_long[,treatname] %in% nonl,valname] * convertfacNonL
        
        # L convert to t / ha
        dd_wide[,l] <-  dd_wide[,l] * convertfacL
        dd_long[dd_long[,treatname] %in% l,valname] <- dd_long[dd_long[,treatname] %in% l,valname] * convertfacL
        dd_agg_long[dd_agg_long[,treatname] %in% l,valname] <- dd_agg_long[dd_agg_long[,treatname] %in% l,valname] * convertfacL
        
        # rename <paramname> with " / divisor()"
        rfun <- function(x) {
          sub(" per planted stick", " per ha",
              sub(" per planted seed", " per ha",
                  sub(" per plot", " per ha",
                      sub(", kg per", ", t per", x))))
        }
        dd_wide[,paramname] <- rfun(dd_wide[,paramname])
        dd_long[,paramname] <- rfun(dd_long[,paramname])
        dd_agg_long[,paramname] <- rfun(dd_agg_long[,paramname])
      }
    }

    ddratio <- list(wide = dd_wide, long = dd_long, agg_long = dd_agg_long)
    # if (debug) save(ddratio, file = "ddratio.rda")
    return(ddratio)
  })

  ## Define two main datasets for Boxplots and ANOVAs
  dd_long <- eventReactive(input$submit, {
    req(ddratio())
    
    dd_long <- ddratio()$long
    
    return(dd_long)
  })
  
  dd_agg_long <- eventReactive(input$submit, {
    req(ddratio())
    
    dd_agg_long <- ddratio()$agg_long
    
    return(dd_agg_long)
  })
  
  paramrange <- eventReactive(input$submit, {
    input$paramrange
  })
  
  ## Update Response input
  output$param <- renderUI({

    param <- if (all(is.na(input$param))) current.param else input$param
    
    selectInput(inputId = "param", 
                label = "Response", 
                choices =  as.character(subset(paramidx, param_2 == input$subject)$param_1),
                selected = param,
                multiple = FALSE, 
                selectize = TRUE, 
                width = NULL, 
                size = NULL)
  })
  
  ## Update divisor input
  output$divisor <- renderUI({
    
    if (!is.null(input$param)) {
      
      if (grepl("^Weight", input$param)) {
        
        choices <- subset(paramidx, param_2 == input$subject)$param3
        seedorstick <- ifelse(input$subject == "Cassava", "stick", "seed")
        
        custom <- paste0("Number of planted ", seedorstick, "s per plot (OWN INPUT)")
        divichoices <- c("1", custom, as.character(choices[grep("^Number.*per plot$", choices)]))
        
        selectInput(inputId = "divisor", 
                    label = "Divide Harvest Weight by", 
                    choices = sort(divichoices),
                    selected = NULL,
                    multiple = FALSE, 
                    selectize = TRUE, 
                    width = NULL, 
                    size = NULL)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  ## Update PerNonLPlot input
  output$PerNonLPlot <- renderUI({
    
    if (!is.null(input$divisor) & !is.null(input$tonnesperhectare)) {
      if (grepl("OWN INPUT", input$divisor)) {
        seedsorsticks <- ifelse(input$subject == "Cassava", "Number of Planted Sticks", "Number of Planted Seeds")
        perm2def <- switch(input$subject,
                           "Cassava" = c(1,10,20,1),
                           "Maize" = c(1,200,300,1),
                           "Pulses" = c(1,350,500,1))
        
        sliderInput(inputId = "PerNonLPlot", 
                    label = paste0(seedsorsticks, " per Non-L-Plot"),
                    min = perm2def[1],
                    value = perm2def[2],
                    max = perm2def[3],
                    step = perm2def[4],
                    width = NULL)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  ## Update PerLPlot input
  output$PerLPlot <- renderUI({
    
    if (!is.null(input$divisor) & !is.null(input$tonnesperhectare)) {
      if (grepl("OWN INPUT", input$divisor)) {
        seedsorsticks <- ifelse(input$subject == "Cassava", "Number of Planted Sticks", "Number of Planted Seeds")
        perm2def <- switch(input$subject,
                           "Cassava" = c(1,10,20,1),
                           "Maize" = c(1,130,300,1),
                           "Pulses" = c(1,350,500,1))
        
        sliderInput(inputId = "PerLPlot", 
                    label = paste0(seedsorsticks, " per L-Plot"),
                    min = perm2def[1],
                    value = perm2def[2],
                    max = perm2def[3],
                    step = perm2def[4],
                    width = NULL)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  ## Update tonnesperhectare input
  output$tonnesperhectare <- renderUI({
    if (!is.null(input$param) & !is.null(input$divisor)) {
      if (grepl("^Weight", input$param)) {
        if (grepl("^1$| planted ", input$divisor)) {
          checkboxInput(inputId = "tonnesperhectare",
                        label = "Compute tonnes per hectare [t/ha]",
                        value = FALSE)
        } else {
          NULL
        }
      } else {
        NULL
      }
    }
  })
  
  tonnesperhectare <- eventReactive(input$submit, {
    # req(input$divisor)
    if (!is.null(input$divisor)) {
      if (!grepl("^1$| planted ", input$divisor)) {
        FALSE
      } else {
        input$tonnesperhectare
      }
    } else {
      FALSE
    }
  })
  
  divisor <- eventReactive(input$submit, {
    if (!is.null(input$param)) {
      if (!grepl("^Weight", input$param)) {
        "1"
      } else {
        input$divisor
      }
    } else {
      "1"
    }
  })
  
  ## Update mergetreatmentlevels input
  # observe({
  #   req(ddmerged())
  #   updateSelectInput(session, inputId = "mergetreatmentlevels",
  #                     label = paste0("Merge treatment level with ", control.level),
  #                     choices = c("no merging", sort(c(wp.levels.all, sp.levels.all))),
  #                     selected = input$mergetreatmentlevels)
  # })
  # 
  ## Update slider input for time period
  # observe({
  #   req(ddmerged())
  #   # periods.all <<- sort(unique(ddmerged()[["long"]][,pername]))
  #   periods <<- if (datachange | all(is.na(input$periods))) periods.all else input$periods
  #   if (debug) message("input$periods is ", paste(input$periods, collapse = ","))
  #   updateSliderInput(session, inputId = "trange", label = "Time Period",
  #                     min = min(periods.all),
  #                     max = max(periods.all),
  #                     step = periods.all,
  #                     value = c(min(periods.all), max(periods.all)))
  # })
  
  ## Update slider input for dates
  observe({
    req(ddmerged())
    periods.sub <- sort(unique(ddmerged()[["long"]][,pername]))
    periods <<- if (datachange | all(is.na(input$periods))) periods.all else input$periods
    if (debug) message("input$periods is ", paste(input$periods, collapse = ","))
    updateCheckboxGroupInput(session, inputId = "periods", label = "Select dates",
                       selected = periods,
                       choices = periods.sub)
  })
  
  ## Update slider input for ends of seasons
  observe({
    req(ddmerged()) ; req(input$periods)
    periodbreaks <<- if (datachange | all(is.na(input$periodbreaks))) guess.period((unique(ddmerged()[["long"]][,pername]))) else input$periodbreaks
    if (debug) message("input$periodbreaks is ", paste(input$periodbreaks, collapse = ","))
    updateCheckboxGroupInput(session, inputId = "periodbreaks", label = "Check ends of seasons",
                             selected = periodbreaks,
                             choices = sort(unique(ddmerged()[["long"]][,pername])))
  })
  
  ## Update slider input (problematic!!!)
  # observe({
  #   req(dd_long()) ; req(input$periods) ; req(input$periodbreaks)
  #   parammin <<- floor(min(dd_long()[,valname], na.rm = TRUE))
  #   parammax <<- ceiling(max(dd_long()[,valname], na.rm = TRUE))
  #   paramselmin <<- min(c(0, round_any(x = min(dd_long()[,valname], na.rm = TRUE), accuracy = 10, f = floor)))
  #   paramselmax <<- max(c(1, round_any(x = max(dd_long()[,valname], na.rm = TRUE), accuracy = 10, f = ceiling)))
  #   if (debug) message("parammin is ", paste(parammin, collapse = ","))
  #   if (debug) message("parammax is ", paste(parammax, collapse = ","))
  #   if (debug) message("paramselmin is ", paste(paramselmin, collapse = ","))
  #   if (debug) message("paramselmax is ", paste(paramselmax, collapse = ","))
  #   
  #   paramrange <<- c(parammin, parammax)
  #   paramsel <- paramrange
  #   # paramsel <<- if (datachange | all(is.na(input$paramrange))) paramrange else input$paramrange
  #   # if (paramselmin > paramsel[1]) paramselmin <<- paramsel[1]
  #   # if (paramselmax < paramsel[2]) paramselmax <<- paramsel[2]
  #   if (debug) message("paramsel is ", paste(paramsel, collapse = ","))
  #   if (debug) message("input$paramrange is ", paste(input$paramrange, collapse = ","))
  #   updateSliderInput(session, inputId = "paramrange", label = "Y Axis Range",
  #                     min = paramselmin,
  #                     max = paramselmax,
  #                     value = paramsel)
  # })
  
  
  
  ## Define Colors
  treatcols <- eventReactive(input$submit, {
    req(dd_long())
    n <- length(levelorder.all)
    allcols <- do.call(input$colors, args = list(n = n))
    allcols[levelorder.all %in% levelorder()]
  })
  
  ## Define ANOVA
  aovres <- eventReactive(input$submit, {
    req(dd_agg_long())
    do.anova(d.long = dd_agg_long(), mode = input$modelmode, 
             wp.levels = wp.levels(), sp.levels = sp.levels(), levelorder = levelorder(),
             modelop = input$modelop, ytrans = input$ytrans, 
             p.adjust.method = p.adjust, plot = FALSE, print = FALSE)
  })
  
  ## Define output Boxplot
  BOXPLOT <- eventReactive(input$submit, {
    req(dd_long())
    
    # if (debug) print(head(dd_agg_long()))
  
    bp <<- do.boxplot(d.long = dd_long(), mode = "facet", aggfun = aggfun,
                      wp.levels = wp.levels(), sp.levels = sp.levels(), levelorder = levelorder(),
                      ylim = input$paramrange, ytrans = input$ytrans, 
                      treatcols = treatcols(),
                      face.strip.text.x = face.strip.text.x, size.strip.text.x = input$fontsize,
                      face.title = face.title, size.title = input$fontsize, 
                      face.axis.title = face.axis.title, size.axis.title = input$fontsize, 
                      face.axis.text = face.axis.text, size.axis.text = input$fontsize,
                      stars = do.results(do.anova(d.long = dd_agg_long(), mode = "block",
                                                  wp.levels = wp.levels(), sp.levels = sp.levels(),
                                                  levelorder = levelorder(),
                                                  modelop = input$modelop, ytrans = input$ytrans,
                                                  p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$PairwiseTtest)
    
    print(bp)           # use with renderPlot in server and plotOutput in ui
    # print(ggplotly(bp)) # use with renderPlotly in server and plotlyOutput in ui
  })
  
  output$Boxplot <- renderPlot({
    BOXPLOT()
  })
  
  ## Define output Meanplot
  MEANPLOT <- eventReactive(input$submit, {
    req(dd_long())
    
    mp <<- do.boxplot(d.long = dd_long(), mode = "group", aggfun = aggfun,
                      wp.levels = wp.levels(), sp.levels = sp.levels(), levelorder = levelorder(),
                      ylim = input$paramrange, ytrans = input$ytrans, 
                      treatcols = treatcols(),
                      face.strip.text.x = face.strip.text.x, size.strip.text.x = input$fontsize,
                      face.title = face.title, size.title = input$fontsize, 
                      face.axis.title = face.axis.title, size.axis.title = input$fontsize, 
                      face.axis.text = face.axis.text, size.axis.text = input$fontsize,
                      stars = do.results(do.anova(d.long = dd_agg_long(), mode = "block", 
                                                  wp.levels = wp.levels(), sp.levels = sp.levels(), 
                                                  levelorder = levelorder(),
                                                  modelop = input$modelop, ytrans = input$ytrans, 
                                                  p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$PairwiseTtest)
    
    print(mp)           # use with renderPlot in server and plotOutput in ui
    # print(ggplotly(mp)) # use with renderPlotly in server and plotlyOutput in ui
  })
  
  output$Meanplot <- renderPlot({
    print(MEANPLOT())
  })
  
  FACTORPLOT <- eventReactive(input$submit, {
    req(dd_long())
    
    fp <<- do.boxplot(d.long = dd_long(), mode = "perfac", aggfun = aggfun,
                      wp.levels = wp.levels(), sp.levels = sp.levels(), levelorder = levelorder(),
                      ylim = input$paramrange, ytrans = input$ytrans, 
                      treatcols = treatcols(),
                      face.strip.text.x = face.strip.text.x, size.strip.text.x = input$fontsize,
                      face.title = face.title, size.title = input$fontsize, 
                      face.axis.title = face.axis.title, size.axis.title = input$fontsize, 
                      face.axis.text = face.axis.text, size.axis.text = input$fontsize,
                      stars = do.results(do.anova(d.long = dd_agg_long(), mode = "split-plot",
                                                  wp.levels = wp.levels(), sp.levels = sp.levels(),
                                                  levelorder = levelorder(),
                                                  modelop = input$modelop, ytrans = input$ytrans,
                                                  p.adjust.method = p.adjust, plot = FALSE, print = FALSE))$Ftest)
  
  print(fp)           # use with renderPlot in server and plotOutput in ui
  # print(ggplotly(fp)) # use with renderPlotly in server and plotlyOutput in ui
  })
  
  output$Factorplot <- renderPlot({
    FACTORPLOT()
  })
  
  ## Define output Timeplot
  TIMEPLOT <- eventReactive(input$submit, {
    req(dd_agg_long()) ; req(treatcols())
    tp <<- do.timeplot(d.long = dd_agg_long(), ylim = input$paramrange, 
                       ytrans = input$ytrans, treatcols = treatcols())
    
    # print(tp)           # use with renderPlot in server and plotOutput in ui
    # print(ggplotly(tp)) # use with renderPlotly in server and plotlyOutput in ui
  })
  
  output$Timeplot <- plotly::renderPlotly({
    # print(ggplotly(TIMEPLOT())) ###
    ggplotly(TIMEPLOT())
  })
  
  ## Define output ANOVA
  output$ANOVA <- renderPrint({
    req(dd_long())
    cat(paste0("\n### ANOVA results for ", locname, " ", unique(dd_long()[,locname]), " - ", paramname, " ", unique(dd_long()[,paramname]), " ###\n"))
    cat(paste("#", aovres()$model), "\n\n")
    switch(input$modelmode,
           "block" = {
             print(aovres()[["sum.aov"]])
             print(aovres()[["sum.glht"]])
             cat("\n# confidence intervals for effect sizes:\n\n")
             print(aovres()[["confint"]])
           },
           "split-plot" = {
             print(aovres()[["sum.anova"]])
             cat("\n# confidence intervals for effect sizes:\n\n")
             print(aovres()[["confint"]])
           })
  })
  
  output$ResultsF <- renderTable({
    do.results(aov = aovres())$Ftest
  })

  output$ResultsT <- renderTable({
    do.results(aov = aovres())$PairwiseTtest
  })
  
    
  # How to render HTML-formatted text
  # output$Stats <- renderUI({
  #   txt <- "my Text"
  #   HTML(paste0("<font face='Courier' color='black'>", txt, '</font>'))
  #   
  # })
  
  ## Define output Residual Analysis
  output$RA <- renderPlot({
    req(aovres())
    do.resanal(aov = aovres(), sp.levels = sp.levels(), wp.levels = wp.levels())
    ds <- paste0("Location: ", unique(dd_long()[,locname]), 
                " ; Period: ", paste(range(dd_long()[,pername]), collapse = " - "),
                " ; Response: ", unique(dd_long()[,paramname]))
    mtext(ds, side = 1, line = -1, outer = TRUE)
  })
  
  ## Define output Data (Raw)
  output$DataR <- renderDataTable({
    datar_raw <- ddinput()$wide
    datar_raw[,pername] <- as.character(datar_raw[,pername])
    datar <- datar_raw[,!names(datar_raw) %in% c(pername_y,pername_m,pername_d)]
    return(datar)
  })
  
  ## Define output Data (Means)
  output$DataM <- renderDataTable({
    datam_raw <- ddinput()$agg
    datam_raw[,pername] <- as.character(datam_raw[,pername])
    datam <- datam_raw[,!names(datam_raw) %in% c(pername_y,pername_m,pername_d)]
    return(datam)
  })
  
  ## Define output Data (ANOVA)
  output$DataS <- renderDataTable({
    datas_raw <- aovres()$d.aov
    datas_raw[,pername_s] <- as.character(datas_raw[,pername_s])
    datas <- datas_raw
    return(datas)
  })
  
  ## Define output Explanations
  output$Help <- renderTable({
    helptext <- data.frame(c(
    "1) Launch App from within RStudio (Run App, best with 'Run External' switched on in the Run App Settings",
    "2) Wait until the data is fully (down)loaded. You should see a message 'Loaded data (wide format) complete' with some summary info about the data",
    "3) Select the desired Location, Subject, Response, and any other selectable analysis parameter",
    "4) Click on 'Submit' to fetch the relevant data subset. This displays the available dates in the 'Check ends of seasons' and 'Select dates' panels of the user interface",
    "5) Look at the Boxplot and select additional analysis parameters (Check end of seasons, Select dates). By default, the last period of each year is selected as the end of a season",
    "6) Click on 'Submit' to subset the data again before the final analysis. This is needed to define the seasons, even if all dates / end of seasons are selected"))
    names(helptext) <- "How to use this app:"
    cat(paste(helptext[,1], collapse = "\n"))
    return(helptext)
  })
  
  ## Define Download options
  # Boxplot
  output$downloadBoxplot <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_boxplot_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".pdf", 
            sep = "") },
    content = function(file) {
      ggsave(file, plot = bp, device = "pdf", width = input$plotwidth, height = input$plotheight)
    }
  )
  
  # Meanplot
  output$downloadMeanplot <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_meanplot_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".pdf", 
            sep = "") },
    content = function(file) {
      ggsave(file, plot = mp, device = "pdf", width = input$plotwidth, height = input$plotheight)
    }
  )
  
  # Factorplot
  output$downloadFactorplot <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_factorplot_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".pdf", 
            sep = "") },
    content = function(file) {
      ggsave(file, plot = fp, device = "pdf", width = input$plotwidth, height = input$plotheight)
    }
  )
  
  # Timeplot
  output$downloadTimeplot <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_timeplot_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".pdf", 
            sep = "") },
    content = function(file) {
      ggsave(file, plot = tp, device = "pdf", width = input$plotwidth, height = input$plotheight)
    }
  )
  
  # ANOVA (summary and confint)
  output$downloadANOVA <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_ANOVA_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".txt", 
            sep = "") },
    content = function(file) {
      write.results(mod = aovres(), filename = file)
    }
  )
  
  # ANOVA (results table)
  output$downloadResults <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_Results_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".txt", 
            sep = "") },
    content = function(file) {
      write.table(do.results(aov = aovres())$Ftest, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
      write.table("\n", file = file, row.names = FALSE, quote = FALSE, sep = "\t", append = TRUE)
      write.table(do.results(aov = aovres())$PairwiseTtest, file = file, row.names = FALSE, quote = FALSE, sep = "\t", append = TRUE)
    }
  )
  
  # ANOVA (residual analysis)
  output$downloadRA <- downloadHandler(
    filename = function() {
      paste("Ugunduzi_downloaded_ANOVA_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".pdf", 
            sep = "") },
    content = function(file) {
      pdf(file, width = 10, height = 10)
      do.resanal(aov = aovres(), sp.levels = sp.levels(), wp.levels = wp.levels())
      ds <- paste0("Location: ", unique(dd_long()[,locname]), 
                   " ; Period: ", paste(range(dd_long()[,pername]), collapse = " - "),
                   " ; Response: ", unique(dd_long()[,paramname]))
      mtext(ds, side = 1, line = -1, outer = TRUE)
      dev.off()
    }
  )
  
  # Data (Raw)
  output$downloadDataR <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_DATAraw_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".txt", 
            sep = "") },
    content = function(file) {
      write.table(ddinput()$wide, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  # Data (Mean)
  output$downloadDataM <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_DATAmean_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".txt", 
            sep = "") },
    content = function(file) {
      write.table(ddinput()$agg, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  # Data (ANOVA - structure depends on choice of experimental design)
  output$downloadDataS <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_DATAanova_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".txt", 
            sep = "") },
    content = function(file) {
      write.table(aovres()$d.aov, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  # Variables in R environment
  output$downloadDataRDA <- downloadHandler(
    filename = function() { 
      paste("Ugunduzi_downloaded_DATA_", 
            gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))), ".rda", 
            sep = "") },
    content = function(file) {
      req(ddinput_all) ; req(ddsub()) ; req(ddmerged()) ; req(ddinput())
      req(sp.levels()) ; req(wp.levels()) ; req(levelorder()) ; req(ddratio())
      req(dd_long()) ; req(dd_agg_long()) ; req(treatcols()) ; req(aovres())
      req(BOXPLOT()) ; req(FACTORPLOT())
      obj <- ls()
      envlist <- list(ddinput_all = ddinput_all, ddsub = ddsub(), ddmerged = ddmerged(),
                      ddinput = ddinput(), sp.levels = sp.levels(), wp.levels = wp.levels(),
                      levelorder = levelorder(), ddratio = ddratio(), dd_long = dd_long(),
                      dd_agg_long = dd_agg_long(), treatcols = treatcols(), aovres = aovres(),
                      BOXPLOT = BOXPLOT(), FACTORPLOT = FACTORPLOT())
      for (i in obj) envlist[[i]] <- get(i) # does not get added
      save(envlist, file = file)
    }
  )
}

### Start Shiny app
shiny::shinyApp(ui = ui, server = server)

# upload app
# 1) go to https://www.shinyapps.io/ and login using your GitHub account, or create an account
# 2) click on your name in the top right, then on "tokens", and copy your rsconnect::setAccountInfo to clipboard
# 3) paste and run the rsconnect::setAccountInfo() code in the R console
# 4) run rsconnect::deployApp(version) from the directory (setwd()) that contains the <version> folder, but no old rsconnect folder
# setwd("~/Dropbox/_ETH_Arbeit/Projects_Shared/Ugunduzi/")
# version = "ugunduzi"
# unlink(file.path(version, "rsconnect"), recursive = T) # delete old folder (deploying does not work otherwise)
# unlink("rsconnect", recursive = T) # delete old folder (deploying does not work otherwise)
# options(repos = BiocManager::repositories()) # makes sure that Bioconductor apps will work
# options(rsconnect.http = "rcurl") # makes sure that read.table will work
# rsconnect::deployApp(version) # deploy app
# setwd(version)
