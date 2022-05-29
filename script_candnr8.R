#### SETUP ####
# Packages:
library(tidyverse) # contains dplyr, tidyr and ggplot2
library(lubridate) # Load lubridate, which is a part of tidyverse
library(cowplot) # To arrange multiple ggplots on one page

# Sets working directory to the downloaded directory 
# containing everything for the bachelor thesis:
setwd("/mnt/chromeos/MyFiles/Downloads/BT/")


# Disclaimer: Throughout this project "strawberry" and "woodland strawberry" 
#             refers to the same species!


#### Read csv-files: ----

### Experiment 1 (main-project):
## Strawberry:
raw_ex1_straw_df <- read.csv(
  "Cand8_Strawberries2_20220309_planteye.csv", header = T)

## Radish:
raw_ex1_rad_c <- read.csv(
  "Cand8_Raddishes_control_20220309_planteye.csv", # Control
  header = T)
raw_ex1_rad_d <- read.csv(
  "Cand8_Raddishes_drought_20220309_planteye.csv", # Drought
  header = T)
# Conbine the two data frames into one:
raw_ex1_rad_df <- rbind(raw_ex1_rad_c, raw_ex1_rad_d) # Binds the data frames
rm(raw_ex1_rad_c) # Clean-up
rm(raw_ex1_rad_d) # Clean-up


### Experiment 2 (side-project):
## PlantEye (radishes):
ex2_planteye <- read.csv(
  "Cand8_Raddishes_sideproject_20220309_planteye.csv", 
  header = T)

## Measurements from manual phenotyping methods (weight, volume):
ex2_manual <- read.csv("Manual_measruements.csv", 
                       header = T, dec = ",")

## Leaf area calculated using ImageJ:
larea <- read.csv(
  "leaf_area_imagej.csv", 
  header = T)




#### Tidying up PlantEye data: ----
### First define general functions that will tidy up the PlantEye data:
# Function that uses functions from the packages lubridate
# to calculates the days since day0:
days <- function(df) {
  
  # Convert timestamp to only include the date:
  df$timestamp <- as_date(df$timestamp)
  
  # Create column with days since start:
  day0 <- sort(df$timestamp)[1] # Day 0
  # Convert date to numeric, and calculate difference between day x and day 0
  df$days_since_start <- as.numeric(df$timestamp - day0) 
  # Convert date back to "normal" readable format
  df$timestamp <- as.character(df$timestamp)
  
  return(df)
}



# Function that covers the basic refinements of the PlantEye data:
tidy_planteye <- function(df) {
  # Remove "coordinates" information from unit column  (e.g. 1:2:1, to only 1):
  df$unit <- substring(df$unit, 1, 2)
  
  # Create a new data frame with only the interesting columns:
  df <- df %>% 
    select(!contains(".bin")) %>%  # Remove "bin_" columns
    select(-g_alias) %>%  # Remove g_alias column
    rename(  # Use rename() to rename columns
      species = genotype,
      digital_biomass = Digital.biomass..mm..,                # unit: mm3
      greenness = greenness.average,
      height = Height..mm.,                                   # unit: mm
      height_max = Height.Max..mm.,                           # unit: mm
      hue = hue.average....,                                  # unit: ° (degree) 
      leaf_angle = Leaf.angle....,                            # unit: ° (degree)
      leaf_area = Leaf.area..mm..,                            # unit: mm2
      leaf_area_index = Leaf.area.index..mm..mm..,            # unit: mm2/mm2
      leaf_area_projected = Leaf.area..projected...mm..,      # unit: mm2
      leaf_inclination = Leaf.inclination..mm..mm..,          # unit: mm2/mm2
      light_penetration_depth = Light.penetration.depth..mm., # unit: mm
      NDVI = NDVI.average,
      NPCI = NPCI.average,
      PSRI = PSRI.average
    )
  
  # Set all character values to lower case:
  df$species <- tolower(df$species)
  df$treatment <- tolower(df$treatment)
  
  ## OBS! Following lines correct stuff specific for my experiment!
  # Remove missing units:
  if (is.element("strawberry", df$species)) {
    df <- df[!(df$unit == 17 & df$treatment == "drought"), ] # Block17, drought
    df <- df[!(df$unit == 18 & df$treatment == "drought"), ] # Block18, drought
    df <- df[!(df$unit == 22 & df$treatment == "drought"), ] # Block22, drought
    df <- df[!(df$unit == 26 & df$treatment == "control"), ] # Block26, control 
  }
  
  # Correct typo (raddish to radish):
  if (is.element("raddish", df$species)) {
    df$species[df$species == "raddish"] <- "radish"
  }
  
  
  return(df)
}



### Apply the functions to PlantEye data:

# Experiment 1:
# Calculate the days since start and apply the tidy_planteye function:
raw_ex1_straw_df <- days(raw_ex1_straw_df) %>% tidy_planteye()
raw_ex1_rad_df <- days(raw_ex1_rad_df) %>% tidy_planteye()

# Merge all data from experiment 1:
ex1_planteye <- rbind(raw_ex1_straw_df, raw_ex1_rad_df)
rm(raw_ex1_straw_df) # Clean-up
rm(raw_ex1_rad_df)   # Clean-up


# Experiment 2:
# Calculate the days since start and apply the tidy_planteye function:
ex2_planteye <- days(ex2_planteye) %>% tidy_planteye()

# Experiment 2 also use data gathered from experiment one. Therefore, I combine
# all the data in one data frame :
all_planteye <- rbind(ex1_planteye, ex2_planteye)

# We don't need the old data frame ex2_planteye, and therefore delete it:
rm(ex2_planteye) # Clean-up



#### Tidying up data acquired with manual phenotyping methods: ----

### Now, basic tydying up of the data frame of manual measurements (ex2_manual):
# Convert date to R-readable dates (same format as other data frames):
ex2_manual <- ex2_manual %>% rename(timestamp = date) # Rename column
ex2_manual$timestamp <- as_date(ex2_manual$timestamp, format = "%d.%m.%Y") %>% 
  as.character() # Converts back to character object

# Change the identification "project" to "experiment":
ex2_manual$project <- ifelse(ex2_manual$project == "main", "ex1", "ex2")
ex2_manual <- ex2_manual %>%  rename(experiment=project)

# Adds missing values for treatment in experiment 2:
ex2_manual$treatment[ex2_manual$experiment == "ex2"] <- "control"

# Calculate water displaced (i.e. volume of plant):
ex2_manual <- ex2_manual %>% 
  # Create a new column with the displaced water (volume, cm3)
  mutate(vol_man_cm3 = water_level_at_end - water_level_at_start) %>% 
  # Remove the redundant columns
  select(-c(water_level_at_end, water_level_at_start))


### Then, over to finding data in PlantEye dfs:
# Define a function that finds the value for a specific parameter
# in the PlantEye data frame (all_planteye):
from_planteye <- function(unit_, species_, treatment_, timestamp_, parameter_) {
  val <- all_planteye %>% 
    # Subset df to only include the single row that matches with function arguments:
    filter(unit == unit_, species == species_, 
           treatment == treatment_, timestamp == timestamp_) %>% 
    # The single element present in the chosen parameter column:
    .[[parameter_]]
  
  return(val)
}



## Use the function from_planteye to fill two new columns:
for (parm in c("digital_biomass", "leaf_area")) {
  # Use the information in the rows as arguments in the function:
  for (i in 1:nrow(ex2_manual)) { 
    ex2_manual[[parm]][i] <- from_planteye(
      ex2_manual$unit[i],
      ex2_manual$species[i],
      ex2_manual$treatment[i],
      ex2_manual$timestamp[i],
      parm
    )
  }
}


# The stored digital biomass and leaf area has the respective units mm3 and
# mm2. We further want to convert them to respectively cm3 and cm2, since that
# was the unit for the manual measurements. Moreover, drop the old columns.
ex2_manual <- ex2_manual %>% 
  mutate(digital_biomass_cm3 = round(digital_biomass/1000, digits = 0),
         digital_leaf_area_cm2 = round(leaf_area/100, digits = 0)) %>% 
  select(-c(digital_biomass, leaf_area))


### Lastly, some tidying of the leaf area from ImageJ data, and merging:
# Smaller corrections:
larea$leaf_area_cm2 <- larea$leaf_area_cm2 %>% 
  str_replace(",", ".") %>% # Change the decimal character (, to .)
  as.numeric() %>% # Convert character elements to numeric ones
  round(0) # Round to integer 

# Join the column with manually measured leaf area 
# with the df with the rest of the data:
ex2_manual <- left_join(ex2_manual, larea) 

# Clean-up
rm(larea)




#### Plots all parameters, Experiment 1 ----


# NB! 
# Last day of watering for the drought group was 20th of February for
# both species. The following day (21st of February) is counted as day 0 of
# the drought period. Scanning of the radishes began 2 days ahead of 
# strawberries. This means that day0 of drought period is 17 days 
# after experiment start for strawberries, and 19 for radishes.

### First, create a vector containing all the parameter names:

# All the parameters from the PlantEye dfs:
all_parameters <- colnames(ex1_planteye)[5:(length(colnames(ex1_planteye))-1)]
#parameters <- all_parameters
# Only the parameters of interest:
parameters <- all_parameters %>% 
  .[-c(       # Remove parameters not of interest to this experiment
    which(. == "height_max"),       # Remove parameter "height_max" because...
    which(. == "leaf_area_index")   # Remove "leaf_area_index" because...
  )]

rm(all_parameters) # clean-up


### Secondly, create a data frame which contain the parameter names and units:
# Create a vector for units to all the parameters:
parameter_units <- c("mm\U00B3", "", 
                     "mm", "degrees (°)", 
                     "degrees (°)", "mm\U00B2", 
                     "mm\U00B2", "mm\U00B2 / mm\U00B2", 
                     "mm", "", 
                     "", "") 
# NB! \U00B3 = unicode superscript 3, \U00B2 = unicode superscript 2 
# Create the data frame of names and units:
parm_unit_df <- cbind.data.frame(parameters, parameter_units)

# Clean-up:
rm(parameter_units)


### Lastly, define and apply function that plots all parameters:

# Function that creates a plot for a specified species and parameter: 
plot_parameter <- function(parameter_, species_, day0) {
  # Subset of PlantEye df with only the given species (arg: species_), 
  # parameter (arg: parameter_)
  df <- ex1_planteye %>% 
    filter(species == species_) %>% 
    select(all_of(parameter_), days_since_start, treatment)
  
  # Subtract the value of day0 of drought period (arg: day0) from days interval:
  # E.g.: 1, 2, 3, 4, 5 → -2, -1, 0, 1, 2, if day0 = 3.
  df$days_drought <- df$days_since_start - day0
  
  # Create a condensed / summery data frame:
  cond_df <- df %>% 
    group_by(days_drought, treatment) %>% 
    summarise(units = n(),
              mean_val = mean(get(parameter_)),
              SD = sd(get(parameter_)),
              SEM = SD/sqrt(units),
              .groups = "keep") # Keep the grouping (from group_by())
  
  # Unit of parameter:
  unit_ <- parm_unit_df %>% filter(parameters == parameter_) %>% .[,2]
  
  # Create the plot:
  # Core:
  ggplot(
    cond_df, 
    aes(days_drought, mean_val, colour=treatment, group=treatment)
    )+
    geom_point()+ # Sets the mean values as points
    geom_line()+ # Connects the dots
    # Error bars (mean ± SD):
    geom_errorbar(aes(days_drought, ymax= mean_val+SD, ymin= mean_val-SD), 
                  width=0.5)+
    # Additional settings:
    scale_color_manual(values = c("blue", "red"))+ # Blue = control, Red = drought
    scale_x_continuous(breaks = cond_df$days_drought)+ # Only show the days of measurements
    theme(legend.position = "none", # Remove legend
          axis.title.x=element_blank(), # Remove x-label
          axis.title.y = element_text(face = "bold", size = 14))+ # Make y-label bigger and of bold type
    ylab(unit_)+
    ggtitle(parameter_) # Sets the plot title as the given parameter
  
}


# Creates a list, where the elements are plots created by the function 
# plot_parameter. Each plot is created with a given parameter (elements in
# the vector "parameters"):
straw_plots <- lapply(parameters, plot_parameter, 
                      "strawberry", 17)

rad_plots <- lapply(parameters, plot_parameter, 
                    "radish", 19)

# Display all the plots in the list (default: 4x3):
# do.call(plot_grid, straw_plots)
# do.call(plot_grid, rad_plots)
# If you want to specify how many plots per column/row, run this:
do.call(plot_grid, c(straw_plots, ncol=2, nrow=6))
do.call(plot_grid, c(rad_plots, ncol=2, nrow=6))
# Save the plots by toggling these two lines:
#ggsave("straw_parameters.jpg", width=20, height=25, units = c("cm"))
#ggsave("rad_parameters.jpg", width=20, height=25, units = c("cm"))

# Clean up:
rm(straw_plots)
rm(rad_plots)
rm(parm_unit_df)

#### Statistical calculations, Experiment 1 ----

### First, find out which measuring days to do statistical tests on:

# Function that returns a vector containing day0 of drought stress 
# (arg = day0), and the a given number of measuring days prior and
# after day0 (arg = days_from_day0):
measurement_int <- function(species_, day0, days_from_day0) {
  int <- ex1_planteye$days_since_start[ex1_planteye$species == species_] %>% 
    unique() %>% 
    .[
      (which(. == day0)-days_from_day0) 
      :
        (which(. == day0)+days_from_day0)
    ]
}

# Create the interval for both species:
# NB! I started with 3, but now I know that it needs to be 4 measuring days
inter_straw <- measurement_int("strawberry", 17, 4) 
inter_rad <- measurement_int("radish", 19, 4)


### Secondly, run t-tests and store the p-values

# Function to find measurements from PlantEye data (given species and day), 
# and calculate p-value for the specified parameter:
ttest_parm <- function(species_, day, parameter_) {
  # Data frame of only given species and day:
  df <- ex1_planteye %>% 
    filter(species == species_ & days_since_start == day) %>% 
    select(treatment, all_of(parameter_)) # Use all_of() to silence warning message
  
  # Run a unpaired two-samples t-test:
  test <- t.test( 
    get(parameter_)~treatment, 
    data = df, 
    var.equal = T, # I assume that the variances are equal
    conf.level = 0.95
  )
  
  # Return a vector with parameter, species, day and p-value:
  return(c(parameter_, species_, day, test$p.value))
}



# Create data frame to store the p-values: 
p_df <- data.frame(matrix(ncol = 4, nrow = 0))


# Append result from the function ttest_parm() for strawberries:
for (parm in parameters) { # Parameter name
  for (day in inter_straw) {  # Day in intervall (i.e. measuring day)
    p_df <- rbind(p_df, ttest_parm("strawberry", day, parm))
  }
}

# Append result from the function ttest_parm() for radishes:
for (parm in parameters) {
  for (day in inter_rad) {
    p_df <- rbind(p_df, ttest_parm("radish", day, parm))
  }
}

# Corrects the colnames:
colnames(p_df) <- c("parameter", "species", "day", "p_val")


# Make sure elements are of numeric format:
p_df$p_val <- as.numeric(p_df$p_val)
p_df$day <- as.numeric(p_df$day)



### Now, we would like to adjust/correct the p-values using the Holm-method:

# Function that takes all the raw/unadjusted p-values for all the parameters,
# and adjust them using the Holm-method:
correction <- function(species_, day_) {
  df <- p_df %>%
    filter(species == species_ & day == day_)
  
  df$holm <- p.adjust(df$p_val, "holm") %>% round(4)
  
  return(df)
}


# Create a storage df where the values are corrected 
# for number of parameters:
corr_parm <- data.frame(matrix(ncol = 5, nrow = 0))

# Fills the data frame with a single day at the time, 
# using the function correction():
for (i in 1:length(inter_straw)) { # Iterate from 1 to total number of measuring days
  # First, use the correction function on parameter values for a single day: 
  single_day_straw <- correction("strawberry", day_=inter_straw[i])
  
  single_day_rad <- correction("radish", day_=inter_rad[i])
  
  # Then, add these to the storage data frame
  corr_parm <- rbind(corr_parm, single_day_straw, single_day_rad)
}

# Correct the column names:
colnames(corr_parm) <- c("parameter", "species", 
                         "day", "p_val", "holm_parm")

# Clean-up:
rm(single_day_straw)
rm(single_day_rad)


### Then, we define function that will use the p-values to create plots and tables:

# Function that calculates the first day from day0 (start of drought period)
# when there is a stat.sign.diff. of mean:
rejection_day <- function(df, parameter_) {
  # Subset data frame for the given parameter
  parm_df <- df %>% 
    filter(parameter == parameter_) %>% 
    .[order(.$day),] # Order the df based on day-values 
  
  # Vector of index values for rows where day >= 0:
  days_0_to_end <- which(parm_df$day == 0):nrow(parm_df)
  
  # Loop to check if the p-value is less than alpha (0.05):
  for (i in days_0_to_end) {
    # Check if the p-value is less than alpha (0.05):
    if (parm_df$p_val[i] < 0.05) {
      day_reject_p_uncor <- parm_df$day[i]
      break # The loop brakes so that the the object 
      #(day_reject_p_uncor) holds the value of the first day
      # there is a statistically significant difference
      
    } else { # If none of the days has a p-value less than alpha
      day_reject_p_uncor <- NA # NA: p-value > 0.05 for ALL days!
    }
  }
  
  # Loop idetical with the previous loop, but for the corrected p-value:
  for (i in days_0_to_end) {
    if (parm_df$holm_parm[i] < 0.05) {
      day_reject_holm <- parm_df$day[i]
      break
    } else {
      day_reject_holm <- NA
    }
  }
  
  # Return a vector with the parameter, the first day of stat.sig.dif. 
  # with uncorrected p-value, and the same for Holm-corrected p-value:
  return(c(parameter_, day_reject_p_uncor, day_reject_holm))
}



# Function that uses the function rejection_day() and plots the result:
plot_rejection_day <- function(df) {
  # Storage data frame:
  reject_day_df <- data.frame(matrix(ncol = 3, nrow = 0))
  # Fills the storage df with the result of function rejection_day():
  for (parm in parameters) { # Iterates all parameters 
    reject_day_df <- rbind(reject_day_df, rejection_day(df, parm))
  }
  # Change colnames: 
  colnames(reject_day_df) <- c("parameter",
                               "day_reject_uncorrected", 
                               "day_reject_corrected")
  # Set the day-value as a numeric value:
  reject_day_df$day_reject_uncorrected <- as.numeric(
    reject_day_df$day_reject_uncorrected)
  
  reject_day_df$day_reject_corrected <- as.numeric(
    reject_day_df$day_reject_corrected)
  
  
  # Convert wide-format to long format:
  reject_day_df <- reject_day_df %>% 
    pivot_longer(cols=2:3, 
                 names_to="p_type",
                 values_to="day_reject")
  
  
  # Bar plot:
  ggplot(reject_day_df, aes(x=parameter, y=day_reject, fill=p_type))+
    geom_bar(position="dodge", stat = "identity", width=0.7)+
    ylim(
      0,
      tail(unique(df$day), n=1) # If the interval of days is changed this
      # ensures that y-axis ends with the last day
    )+
    coord_flip()+ # Flip the bar plot (y-axis is now horizontally)
    ggtitle(paste0("Species: ", ifelse(
                                      unique(df$species)=="strawberry", 
                                      "Woodland strawberry", 
                                      "Radish")))+
    ylab("Day until significant difference of mean between groups")+
    xlab("Parameter")+
    labs(fill="p-value")+ # Legend title
    scale_fill_manual(
      values = c("indianred1", "lightseagreen"), # Color of bars
      breaks = c("day_reject_uncorrected", "day_reject_corrected"), # Order of bars
      labels = c("raw p-value", "p-value adjusted
with Holm method") # Name of bars
    )
  
}



# Function that uses the subsection of the data frame corr_parm to show when
# there was a stat.sign.diff. of mean between the two groups:
reject_table <- function(org_df, species_) {
  # Create a new data frame:
  df_wide <- org_df %>% 
    select(parameter, day, holm_parm) %>% 
    group_by(parameter) %>% 
    pivot_wider(names_from = day, values_from = holm_parm) 
  # ↑ Converts from long to wide format, by giving each day its own column
  
  
  # Change the data of the data frame:
  df_wide[, 2:ncol(df_wide)] <- ifelse(
    df_wide[, 2:ncol(df_wide)] < 0.05, # Checks if p-value less than sign.level
    "REJECT", # If statement is TRUE, replace the numeric value with "REJECT"
    "") # If statement is FALSE, replace the numeric value with "", i.e. blank 
  
  View(df_wide)
  
  # Toggle this comment for writing a CSV-file:
  #write.csv(df_wide, paste0("Table_", species_, ".csv"), row.names = F)
}



### Lastly, we create subsections of the data frame corr_parm, based on species,
### and run the functions plot_rejection_day() and reject_table():

## Subsections of corr_parm
rad <- corr_parm %>% 
  filter(species == "radish")
rad$day <- rad$day - 19 # subtract with the value of day 0


straw <- corr_parm %>% 
  filter(species == "strawberry")
straw$day <- straw$day - 17 # subtract with the value of day 0


## Display the result of the function plot_rejection_day:
plot_rejection_day(rad)
plot_rejection_day(straw)

## Display the result of the function reject_table:
reject_table(straw, "Woodland_strawberry")
reject_table(rad, "Radish")



#### Plots for manual vs. digital, Experiment 2 ----

### Leaf area, multiple weeks:

# Create new data frame with needed data:
larea_mweeks <- ex2_manual %>% 
  filter((species == "radish" & treatment == "control")) %>% 
  select(unit, timestamp, treatment, leaf_area_cm2, digital_leaf_area_cm2) %>% 
  .[!is.na(.$leaf_area_cm2),] # Drop rows with NAs (for leaf area)

# Creating a new column with the calculated fraction (rounded to two decimals):
larea_mweeks <- larea_mweeks %>% 
  mutate(fraction = round(leaf_area_cm2/digital_leaf_area_cm2, digits = 2))

# Create a new column containing the weeks since experiment start:
larea_mweeks$week <- NA
# Fills the new column based on the timestamp, to give correct week:
for (i in 1:nrow(larea_mweeks)) { # Iterate from 1 to total number of rows
  if (larea_mweeks$timestamp[i] == "2022-02-16") {
    larea_mweeks$week[i] <- "week 3"
  } else if (larea_mweeks$timestamp[i] == "2022-02-23") {
    larea_mweeks$week[i] <- "week 4" 
  } else if (larea_mweeks$timestamp[i] == "2022-03-02") {
    larea_mweeks$week[i] <- "week 5"
  } else { # Then they are either "2022-03-07" or "2022-03-09"
    larea_mweeks$week[i] <- "week 6"
  }
}

# Plotting box plots for the individual weeks:
ggplot(larea_mweeks, aes(week, fraction))+
  geom_violin()+
  geom_abline(intercept=1,slope = 0, color="blue", linetype="dashed")+ # Indication of fraction value 1.0
  labs(x="", y="manual / digital",
       title = "Parameter: Leaf area     Species: Radish    Treatment: Control",
       subtitle = paste0("Number of units in group: 
                         week 3:  ", length(larea_mweeks$week[larea_mweeks$week == "week 3"]),
                         "        week 4:  ", length(larea_mweeks$week[larea_mweeks$week == "week 4"]),
                         "        week 5:  ", length(larea_mweeks$week[larea_mweeks$week == "week 5"]),
                         "        week 6:  ", length(larea_mweeks$week[larea_mweeks$week == "week 6"])
       ))+
  theme(axis.title.y = element_text(vjust = 0.5, angle = 0)) # Sets the label in centrum and horizontaly

rm(larea_mweeks) # Clean-up



### Leaf area and volume at experiment end (single week):

# Define a function to plot all violin plots:
fraction_plot <- function(org_df, species_, measurement, view_df=F) {
  # Volume:
  if (measurement == "volume") {
    df <- org_df %>% 
      filter(species == species_) %>% 
      select(unit, timestamp, treatment, vol_man_cm3, digital_biomass_cm3) %>% 
      .[!is.na(.$vol_man_cm3),]
    
    df$fraction <- (df$vol_man_cm3/df$digital_biomass_cm3)
    df$fraction <- round(df$fraction, 2)
  
  # Leaf area:
  } else if (measurement == "leaf_area") {
    df <- org_df %>% 
      filter(species == species_) %>% 
      select(unit, timestamp, treatment, leaf_area_cm2, digital_leaf_area_cm2) %>% 
      .[!is.na(.$leaf_area_cm2),]
    
    df$fraction <- (df$leaf_area_cm2/df$digital_leaf_area_cm2)
    df$fraction <- round(df$fraction, 2)
  }
  
  # View the data frame. Could be usefull.
  if (view_df == T) {
    View(df)
  }
  
  # Create a violoin plot of the fraction manual/digital:
  vplot <- ggplot(df, aes(treatment, fraction, fill=treatment))+
    geom_violin()+
    scale_fill_manual(values = c("deepskyblue1", "firebrick2"))+ # deepskyblue1 = control, firebrick2 = drought
    geom_abline(intercept=1,slope = 0, color="blue", linetype="dashed")+
    labs(x="", y="manual / digital", 
         title = paste0("Parameter: ", measurement, "    Species: ", 
                        ifelse(species_=="strawberry", 
                               "Woodland strawberry", 
                               "Radish")),
         subtitle = paste0("Units of control group: ", 
                           length(df$treatment[df$treatment == "control"]),
                           "                ",
                           "Units of drought group: ", 
                           length(df$treatment[df$treatment == "drought"])))+
    theme(axis.title.y = element_text(vjust = 0.5, angle = 0), # display label horizontaly
          legend.position = "none") # Remove legend
  
  return(vplot)
}

# Leaf area, strawberry:
fraction_plot(ex2_manual, "strawberry", "leaf_area", F)


# Leaf area, radish:
# NB! Not all radishes were measured the same week, so I create a new
# data frame with only measurements from the last week: 
radish_df <- ex2_manual %>% filter(
  (timestamp == "2022-03-07" | timestamp == "2022-03-09"))
# Then, plot it:
fraction_plot(radish_df, "radish", "leaf_area", F)
rm(radish_df) # Clean-up


# Volume, both species:
a <- fraction_plot(ex2_manual, "radish", "volume", F)
b <- fraction_plot(ex2_manual, "strawberry", "volume", F)
plot_grid(a, b, labels = c("A.", "B.")) # Display both plots in same window


