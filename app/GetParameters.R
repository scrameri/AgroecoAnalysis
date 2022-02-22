#######################################################################
### Set PARAMETERS for Analysis of Tanzania Agricultural Experiment ###
#######################################################################

## Script Author: 
# Simon Crameri, simon.crameri@usys.ethz.ch, sfcrameri@gmail.com

## Set parameters
# download switch
downloadall = TRUE # if TRUE, will download all data, if FALSE, will load "ddinput_all.rda" from disk

# data base for requests
datapath = "http://sautiyawakulima.net/agresearch/r/get_data.php"

# location indices
locidx = data.frame("idx" = 0:2, "loc" = factor(c("Chambezi", "Masasi", "Morogoro")))
current.loc <- levels(locidx$loc)[1] # default loc

# parameter indices
parampath = "http://sautiyawakulima.net/agresearch/r/parameters.php"

if (downloadall) {
  paramidx = read.delim(parampath, header = FALSE, stringsAsFactors = TRUE, sep = ",")
  names(paramidx) <- c("idx", "param", "unit")
  paramlevels <- sapply(strsplit(as.character(paramidx$param), split = " - "), "[", 1)
  # paramdefault <- "Plant size"
  paramidx$param_1 <- paramlevels
  paramidx$param_2 <- sapply(strsplit(as.character(paramidx$param), split = " - "), "[", 2)
  paramidx$param2 <- paste0(paramidx$param_1, " (", paramidx$param_2)
  paramidx <- subset(paramidx, !param_1 %in% c("Health report", "Soil moisture")) ### Remove unwanted data here!
  paramidx$param3 <- apply(paramidx[,c("param_1","unit")], 1, FUN = paste, collapse = " ")
  for (i in c("param_1","param_2","param2","param3")) paramidx[,i] <- factor(paramidx[,i])
  save(paramidx, file = "paramidx.rda")
} else {
  load("paramidx.rda")
}
current.subject <- levels(paramidx$param_2)[1] # default subject
current.param <- levels(paramidx$param_1)[10] # default param

# data aggregation
aggfun <- "mean" # function used to aggregate data

# data transformation (yield in kg/hectare)
plot.area <- 18 # plot area in m2
plots.per.hectare <- 10000 / plot.area # 10000 (m2 per hectare) / 16 (m2 per plot)
# n.per.plot <- 144 # 144 seeds per plot = 9 seeds per m2
# n.per.m2 <- 9 

# data modeling (ANOVA)
modelchoices <- c("2-way interactions", "additive only") # additive and interaction effects
transchoices <- c("identity", "logst", "sqrt", "asin-sqrt") # removed "log" due to zeros

# data ordering
levelorder <- levelorder.all <- c("Control", "L", "P", "S", "PL", "PS", "SL", "PSL") # order of treatment factor levels 
L.levels <- levelorder.all[grepl("L", levelorder.all)]

# experimental design
control.level <- "Control"
wp.levels <- wp.levels.all <- c("S", "P")
sp.levels <- sp.levels.all <- c("L")

# stats
p.adjust <- "single-step" # single-step seems to be less conservative than holm

# time
first.month <- 1
last.month <- 12
first.year <- 2017
current.year <- as.numeric(unlist(strsplit(as.character(Sys.time()), split = "-"))[1])
first.date <- as.Date("2017-07-14")
last.date <- as.Date("2021-12-06")

# set parameters back to default if data subset changes
datachange <- FALSE

## Set identifyers
start.section <- "Location"
start.indbased <- "Sample number"
end.indbased <- "Number of samples"
start.plotbased <- "Field"
end.plotbased <- "Mean"
locname <- "Location"
pername <- "Period"
pername_d <- "Day"
pername_m <- "Month"
pername_y <- "Year"
pername_s <- "Season"
paramname <- "Parameter"
treatname <- "Treatment"
repname <- "Rep"
idname <- "id"
valname <- "Value"

## Plotting
# Side bar width
sidebarwidth <- 3

# Plot height
plotwidth <- "100%"
plotheight <- "800px"

# Plot label size
face.strip.text.x = "plain"
face.title = "plain"
face.axis.title = "plain"
face.axis.text = "bold"
size.strip.text.x = 15
size.title = 15
size.axis.title = 15
size.axis.text = 15

# Color palettes
palettes <- c("angelika", "funky", "gg_color_hue", "rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors")

## Error message (displayed when there is no data for a given user choice)
debug <- TRUE # FALSE # if TRUE, will display messages of current input variable definitions
errormessage <- paste("No data found for this", locname, "/ Subject / ", paramname, "/", pername, "combination! Please hit 'Submit' again!")
