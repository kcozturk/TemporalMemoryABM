###### INITIAL SETUP ######

# Clear the environment
rm(list = ls())

# Install required packages if not already installed
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("truncnorm")
# install.packages("data.table")

library(ggplot2)
library(ggpubr)
library(truncnorm)
library(data.table)

# Load custom functions
source("Functions.R")

# Set working directory to script location (if running in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##### --- Overview ---
# 1. Load & Prepare data
# 2. Descriptive statistics
# 3. Compute Foraging Efficiency
# 4. Figures Introduction
# 5. Figures Scenario 1
# 6. Figures Scenario 2
# 7. Descriptive stats: Scenario 3
# 8. Figures Supplementary Material


#==================================#
###### 1. Load & Prepare data ######
#==================================#

# Columns to extract
selected_columns <- c(1, 4, 7, 11, 18, 25, 26)

# function: format one dataset (list of CSVs) into final df
prepare_df <- function(datalist) {
  df <- do.call(cbind, lapply(datalist, function(x) x[selected_columns, -1]))
  df <- data.frame(t(df))
  colnames(df) <- c(
    "eaten", "inaccuracy_factor", "time_to_dis",
    "nTree", "memory_slots", "cent_std", "memory"
  )
  df
}

# function: load one dataset (pattern + number of files) -> df
load_dataset <- function(pattern, n) {
  files     <- sprintf(pattern, seq_len(n))
  data_list <- lapply(files, data.table::fread, showProgress = FALSE)
  prepare_df(data_list)
}

# Main datasets (cognitive, sensory, tradeoff, nonupdate)
df   <- load_dataset("output_decay%d.csv", 1152)
df2  <- load_dataset("output_prod%d.csv", 1152)
df3  <- load_dataset("output_hetero%d.csv", 1152)

df4  <- load_dataset("output_sensory_decay%d.csv", 8)
df5  <- load_dataset("output_sensory_prod%d.csv", 8)
df6  <- load_dataset("output_sensory_hetero%d.csv", 8)

df7  <- load_dataset("output_tradeoff_lin_decay%d.csv", 36)
df8  <- load_dataset("output_tradeoff_lin_prod%d.csv", 36)
df9  <- load_dataset("output_tradeoff_lin_hetero%d.csv",36)

df10 <- load_dataset("output_tradeoff_exp_decay%d.csv", 36)
df11 <- load_dataset("output_tradeoff_exp_prod%d.csv", 36)
df12 <- load_dataset("output_tradeoff_exp_hetero%d.csv", 36)

df13 <- load_dataset("output_nonupdate_decay%d.csv", 1152)
df14 <- load_dataset("output_nonupdate_prod%d.csv", 1152)
df15 <- load_dataset("output_nonupdate_hetero%d.csv", 1152)



#=====================================#
###### 2. Descriptive statistics ######
#=====================================#


# Cognitive vs sensory foragers: mean eaten fruits
cog_eaten_vec <- c(df$eaten, df2$eaten, df3$eaten)
sen_eaten_vec <- c(df4$eaten, df5$eaten, df6$eaten)

mean_cognitive <- mean(cog_eaten_vec)
mean_sensory   <- mean(sen_eaten_vec)

# Standard error of mean eaten
se_cognitive <- sd(cog_eaten_vec) / sqrt(length(cog_eaten_vec))
se_sensory   <- sd(sen_eaten_vec) / sqrt(length(sen_eaten_vec))

#==========================================#
###### 3. Compute Foraging Efficiency ######
#==========================================#

# Sensory averages (used for relative efficiencies)
averages_df4 <- aggregate(eaten ~ time_to_dis, data = df4, FUN = mean)
averages_df5 <- aggregate(eaten ~ nTree,       data = df5, FUN = mean)
averages_df6 <- aggregate(eaten ~ cent_std,    data = df6, FUN = mean)


# Rename second column in sensory averages to'sensory
names(averages_df4)[2] <- "sensory"
names(averages_df5)[2] <- "sensory"
names(averages_df6)[2] <- "sensory"


#================================================================#
### 3.1 CREATE averages_df* (aggregation + merge with sensory) ###
#================================================================#

# Function: aggregate eaten and merge with sensory averages
compute_averages <- function(df, group_vars, env_var, sensory_avg) {
  agg <- aggregate(
    df$eaten,
    by = setNames(lapply(group_vars, function(v) df[[v]]), group_vars),
    FUN = mean
  )
  colnames(agg)[length(group_vars) + 1] <- "eaten"
  
  # env_var must be a column in both agg and sensory_avg
  merged <- merge(agg, sensory_avg, by = env_var)
  merged
}

## Main cognitive scenarios

# df: varies time_to_dis
averages_df <- compute_averages(
  df,
  group_vars = c("time_to_dis", "memory_slots", "inaccuracy_factor"),
  env_var    = "time_to_dis",
  sensory_avg = averages_df4
)

# df2: varies nTree
averages_df2 <- compute_averages(
  df2,
  group_vars = c("nTree", "memory_slots", "inaccuracy_factor"),
  env_var    = "nTree",
  sensory_avg = averages_df5
)

# df3: varies cent_std
averages_df3 <- compute_averages(
  df3,
  group_vars = c("cent_std", "memory_slots", "inaccuracy_factor"),
  env_var    = "cent_std",
  sensory_avg = averages_df6
)

## Scenario 2 – tradeoff (linear)

# df7: time_to_dis & memory_slots
averages_df7 <- compute_averages(
  df7,
  group_vars = c("time_to_dis", "memory_slots"),
  env_var    = "time_to_dis",
  sensory_avg = averages_df4
)

# df8: nTree & memory_slots
averages_df8 <- compute_averages(
  df8,
  group_vars = c("nTree", "memory_slots"),
  env_var    = "nTree",
  sensory_avg = averages_df5
)

# df9: cent_std & memory_slots
averages_df9 <- compute_averages(
  df9,
  group_vars = c("cent_std", "memory_slots"),
  env_var    = "cent_std",
  sensory_avg = averages_df6
)

## Scenario 2 – tradeoff (exponential)

# df10: time_to_dis & memory_slots
averages_df10 <- compute_averages(
  df10,
  group_vars = c("time_to_dis", "memory_slots"),
  env_var    = "time_to_dis",
  sensory_avg = averages_df4
)

# df11: nTree & memory_slots
averages_df11 <- compute_averages(
  df11,
  group_vars = c("nTree", "memory_slots"),
  env_var    = "nTree",
  sensory_avg = averages_df5
)

# df12: cent_std & memory_slots
averages_df12 <- compute_averages(
  df12,
  group_vars = c("cent_std", "memory_slots"),
  env_var    = "cent_std",
  sensory_avg = averages_df6
)

## Scenario 3 – nonupdate

# df13: time_to_dis, memory_slots, inaccuracy_factor
averages_df13 <- compute_averages(
  df13,
  group_vars = c("time_to_dis", "memory_slots", "inaccuracy_factor"),
  env_var    = "time_to_dis",
  sensory_avg = averages_df4
)

# df14: nTree, memory_slots, inaccuracy_factor
averages_df14 <- compute_averages(
  df14,
  group_vars = c("nTree", "memory_slots", "inaccuracy_factor"),
  env_var    = "nTree",
  sensory_avg = averages_df5
)

# df15: cent_std, memory_slots, inaccuracy_factor
averages_df15 <- compute_averages(
  df15,
  group_vars = c("cent_std", "memory_slots", "inaccuracy_factor"),
  env_var    = "cent_std",
  sensory_avg = averages_df6
)


#===================================================#
### 3.2 Add rel_eff & log_rel_eff to averages_df* ###
#===================================================#

avg_names <- c(
  "averages_df", "averages_df2", "averages_df3",
  "averages_df7", "averages_df8", "averages_df9",
  "averages_df10", "averages_df11", "averages_df12",
  "averages_df13", "averages_df14", "averages_df15"
)

for (nm in avg_names) {
  tmp <- get(nm)
  tmp$rel_eff     <- (tmp$eaten + 1) / (tmp$sensory + 1)
  tmp$log_rel_eff <- log(tmp$rel_eff)
  assign(nm, tmp)
}


#===================================#
###### 4. Figures Introduction ######
#===================================#


#====================================#
###### FIG. 1: Simulated output ######
#====================================#


# Set environment boundaries
env_xmin <- 0
env_xmax <- 1000
env_ymin <- 0 
env_ymax <- 1000

# Output figure as PNG
png(filename = "Fig1.png", width = 1100, height = 1200, units = "px", res = 160)

par(mfrow = c(4, 3), oma = c(1, 1, 1.5, 1), mar = c(2, 4.3, 2.2, 1), cex.lab = 1.3)
set.seed(1)


### --- A. Productivity ---
# Figures with spatial distribution of trees for different Patch Spread
homogen           <- 0
num_cent          <- 10
nTree             <- 400
cent_std_values   <- c(20, 40, 80)

for (i in 1:3) {
  cent_std <- cent_std_values[i]
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, 
                              homogenous = homogen, n_centers = num_cent, 
                              std = cent_std
  )
  
  plot(tree_coords, xlim=c(0, 1000), ylim=c(0, 1000), xaxt='n', yaxt = 'n', 
       ylab="", xlab= "", main = paste(cent_std))
  
  if (i == 1) {
    mtext("A. Patch spread:", side = 3, line = -0.2, cex = 0.9, font = 2, 
          adj = 0, outer = TRUE)
  }
}


### --- B. Productivity ---
# Figures with spatial distribution of trees for different productivity

homogen       <- 1
nTree_values  <- c(200, 400, 800)  

for (i in 1:3) {
  nTree <- nTree_values[i]
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, 
                              homogenous = homogen, n_centers = num_cent, 
                              std = cent_std
  )
  
  plot(tree_coords, xlim=c(0, 1000), ylim=c(0, 1000), xaxt='n', yaxt = 'n', 
       ylab="", xlab= "", main = paste(nTree_values[i]))
  
  if (i == 1) {
    mtext("B. Resource Density:", side = 3, line = -14.2, cex = 0.9, font = 2, 
          adj = 0, outer = TRUE)
  }
}



### --- C. Productivity ---
# Figures with ripe fruit over time for different productivity
par(mar = c(5, 4.5, 2.2, 1))

memory_slots        <- 0
inaccuracy_factor   <- 0
homogen             <- 1
time_to_dis         <- 4
init_time           <- 425
nTree_values        <- c(200, 400, 800)
dynamic_inaccuracy <- F               # dynamic Inaccuracy (T/F)
dynamic_inac_relationship <- F        #"linear" or "exp"

initialize_env_agent(nTree = nTree)

for (i in seq_along(nTree_values)) {
  nTree <- nTree_values[i]
  
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, homogenous = homogen, 
                              n_centers = num_cent, std = cent_std)
  
  # Initialize trees
  Trees <- cbind(
    TreeNo = 1:nTree,
    Xcoord = tree_coords[, 1],
    Ycoord = tree_coords[, 2],
    Regeneration = AverageRegenSteps,
    Regen_counter = round(runif(nTree, min = 0, max = AverageRegenSteps)),
    Fruit_total_95 = 0,
    Unripe = 0,
    Total_Fruits_created = 0,
    Fruits_being_created = 0,
    Fruit_ripen_count = 0,
    Fruit_decay_count = 100
  )
  
  # Simulation
  timemax <- init_time
  allocated_size <- round(timemax * 5.1)
  Trees_hist <- matrix(nrow = nTree, ncol = ncol(Trees))
  time_hist <- numeric(allocated_size)
  Trees_hist_plot <- list()
  
  time_hist[1:2] <- 0
  Trees_hist_plot[1:2] <- 0
  
  time <- 0
  ts <- 2
  
  while (time < timemax) {
    time <- time + 0.1
    Trees <- fruitripening(Trees, time_hist, time, ts)
    Trees <- fruitdecay(Trees, time_hist, time, ts)
    Trees <- fruiting(Trees, time_hist, time, ts)
    ts <- ts + 1
    
    if (ts > allocated_size) {
      allocated_size <- as.integer(round(allocated_size * 1.05))
      length(time_hist) <- allocated_size
    }
    
    Trees_hist <- Trees
    time_hist[ts] <- time
    Trees_hist_plot[[ts]] <- Trees
  }
  
  # Plot data
  plotdata <- data.frame(
    time = sapply(3:4252, function(j) time_hist[[j]] - 60),
    ripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 6])),
    unripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 7]))
  )
  plotdata <- plotdata[601:4250, ]
  
  plot(ripefruits ~ time, data = plotdata, type = "l",
       ylab = "Ripe fruit", xlab = "Time",
       main = paste(nTree),
       ylim = c(0, 80))
  
  points(unripefruits ~ time, data = plotdata, type = "l", col = "red")
  
  if (i == 1) {
    mtext("C. Resource Density:", side = 3, line = -28.2, cex = 0.9, font = 2, 
          adj = 0, outer = TRUE)
  }
}


### --- D. Decay Period ---
# Figures with ripe fruit over time for different decay periods
memory_slots      <- 0
inaccuracy_factor <- 0
homogen           <- 1
init_time         <- 425
nTree             <- 400
time_to_dis_vals  <- c(2.0, 4.0, 8.0)
dynamic_inaccuracy <- F               # dynamic Inaccuracy (T/F)
dynamic_inac_relationship <- F        #"linear" or "exp"

initialize_env_agent(nTree = nTree)

for (i in seq_along(time_to_dis_vals)) {
  time_to_dis <- time_to_dis_vals[i]
  
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, homogenous = homogen, 
                              n_centers = num_cent, std = cent_std)
  
  # Initialize trees
  Trees <- cbind(
    TreeNo = 1:nTree,
    Xcoord = tree_coords[, 1],
    Ycoord = tree_coords[, 2],
    Regeneration = AverageRegenSteps,
    Regen_counter = round(runif(nTree, min = 0, max = AverageRegenSteps)),
    Fruit_total_95 = 0,
    Unripe = 0,
    Total_Fruits_created = 0,
    Fruits_being_created = 0,
    Fruit_ripen_count = 0,
    Fruit_decay_count = 100
  )
  
  # Simulation
  timemax <- init_time
  allocated_size <- round(timemax * 5.1)
  Trees_hist <- matrix(nrow = nTree, ncol = ncol(Trees))
  time_hist <- numeric(allocated_size)
  Trees_hist_plot <- list()
  
  time_hist[1:2] <- 0
  Trees_hist_plot[1:2] <- 0
  
  time <- 0
  ts <- 2
  
  while (time < timemax) {
    time <- time + 0.1
    Trees <- fruitripening(Trees, time_hist, time, ts)
    Trees <- fruitdecay(Trees, time_hist, time, ts)
    Trees <- fruiting(Trees, time_hist, time, ts)
    ts <- ts + 1
    
    if (ts > allocated_size) {
      allocated_size <- as.integer(round(allocated_size * 1.05))
      length(time_hist) <- allocated_size
    }
    
    Trees_hist <- Trees
    time_hist[ts] <- time
    Trees_hist_plot[[ts]] <- Trees
  }
  
  # Plot data
  plotdata <- data.frame(
    time = sapply(3:4252, function(j) time_hist[[j]] - 60),
    ripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 6])),
    unripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 7]))
  )
  plotdata <- plotdata[601:4250, ]
  
  plot(ripefruits ~ time, data = plotdata, type = "l",
       ylab = "Ripe fruit", xlab = "Time",
       main = paste(time_to_dis),
       ylim = c(0, 50))
  
  points(unripefruits ~ time, data = plotdata, type = "l", col = "red")
  
  if (i == 1) {
    mtext("D. Resource Availability Period:", side = 3, line = -42.5, cex = 0.9, 
          font = 2, adj = 0, outer = TRUE)
  }
}

dev.off()






#=================================#
###### 5. Figures Scenario 1 ######
#=================================#


#==========================================================#
###### FIG. 3: Rel Foraging Benefit Cognitive Forager ######
#==========================================================#

### --- Plot A: Rel Benefit vs Decay Period ---

# Identify maximum relative efficiency per decay period
high_rel_eff <- data.frame(
  time_to_dis = unique(averages_df$time_to_dis),
  log_rel_eff = tapply(averages_df$log_rel_eff, averages_df$time_to_dis, FUN = max)
)
high_rel_eff$forgetting <- "active forgetting"

# Identify maximum rel efficiency (for non-updating cognitive forager) per decay period
high_rel_eff_nu <- data.frame(
  time_to_dis = unique(averages_df13$time_to_dis),
  log_rel_eff = tapply(averages_df13$log_rel_eff, averages_df13$time_to_dis, FUN = max)
)
high_rel_eff_nu$forgetting <- "no active forgetting"

dfA <- rbind(high_rel_eff, high_rel_eff_nu)

plot <- ggplot(dfA, aes(x = time_to_dis, y = log_rel_eff, group = forgetting)) +
  geom_line(aes(linetype = forgetting), show.legend = FALSE) +  # only shapes in legend
  geom_point(aes(shape = forgetting),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "active forgetting:",
    values = c("active forgetting"    = 19,  # circle
               "no active forgetting" = 15), # square
    labels = c("active forgetting"    = "yes",
               "no active forgetting" = "no")
  ) +
  ylim(0, 2.1) +
  labs(
    x = "Resource Availability Period",
    y = expression(paste("log(", italic(E)[r], ")")),
    tag = "A"
  ) +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  geom_hline(yintercept = log(1), linetype = "dashed", color = "red")


### --- Plot B: Net Benefit vs Productivity ---

high_rel_eff2 <- data.frame(
  nTree = unique(averages_df2$nTree),
  log_rel_eff = tapply(averages_df2$log_rel_eff, averages_df2$nTree, FUN = max)
)
high_rel_eff2$forgetting <- "active forgetting"

high_rel_eff_nu2 <- data.frame(
  nTree = unique(averages_df14$nTree),
  log_rel_eff = tapply(averages_df14$log_rel_eff, averages_df14$nTree, FUN = max)
)
high_rel_eff_nu2$forgetting <- "no active forgetting"

dfB <- rbind(high_rel_eff2, high_rel_eff_nu2)

plot2 <- ggplot(dfB, aes(x = nTree, y = log_rel_eff, group = forgetting)) +
  geom_line(aes(linetype = forgetting), show.legend = FALSE) +
  geom_point(aes(shape = forgetting),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "active forgetting",
    values = c("active forgetting"    = 19, 
               "no active forgetting" = 15),
    labels = c("active forgetting"    = "yes",
               "no active forgetting" = "no")
  ) +
  ylim(0, 2.1) +
  labs(x = "Resource Density", tag = "B") +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  geom_hline(yintercept = log(1), linetype = "dashed", color = "red")


### --- Plot C: Net Benefit vs Patch Spread ---

high_rel_eff3 <- data.frame(
  cent_std = unique(averages_df3$cent_std),
  log_rel_eff = tapply(averages_df3$log_rel_eff, averages_df3$cent_std, FUN = max)
)
high_rel_eff3$forgetting <- "active forgetting"

high_rel_eff_nu3 <- data.frame(
  cent_std = unique(averages_df15$cent_std),
  log_rel_eff = tapply(averages_df15$log_rel_eff, averages_df15$cent_std, FUN = max)
)
high_rel_eff_nu3$forgetting <- "no active forgetting"

dfC <- rbind(high_rel_eff3, high_rel_eff_nu3)

plot3 <- ggplot(dfC, aes(x = cent_std, y = log_rel_eff, group = forgetting)) +
  geom_line(aes(linetype = forgetting), show.legend = FALSE) +
  geom_point(aes(shape = forgetting),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "active forgetting",
    values = c("active forgetting"    = 19, 
               "no active forgetting" = 15),
    labels = c("active forgetting"    = "yes",
               "no active forgetting" = "no")
  ) +
  ylim(0, 2.1) +
  labs(x = "Patch Spread", tag = "C") +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  geom_hline(yintercept = log(1), linetype = "dashed", color = "red")


### --- COMBINE AND EXPORT FIGURE ---

png("Fig3.png", width = 1500, height = 500, res = 200)

figure <- ggarrange(
  plot, plot2, plot3,
  ncol = 3, nrow = 1,
  widths = c(1.05, 1, 1),
  common.legend = TRUE,   # shared legend
  legend = "right"        # on the total right side
)

print(figure)
dev.off()






#================================================#
###### FIG. 4: Relative Foraging Efficiency ######
#================================================#

### ===== FIG. 4 left: Decay Period

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(c(averages_df$log_rel_eff,
               averages_df2$log_rel_eff,
               averages_df3$log_rel_eff))

z_max <- max(c(averages_df$log_rel_eff,
               averages_df2$log_rel_eff,
               averages_df3$log_rel_eff))

min_val <- -1.6
max_val <- 2.1

# Define contour levels and color breaks (same for all plots)
levels <- round(seq(min_val, max_val, length.out = 11), 1)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of time_to_dis
time_values <- unique(averages_df$time_to_dis)[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_decay <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(time_values)){
  t <- time_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df[averages_df$time_to_dis == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = log_rel_eff)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  # Add line when z = 0
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = "Cognitive storage") +
    labs(title = paste(t), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[r], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_decay2 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = log_rel_eff, group = memory_slots)) +
    ylim(floor(min_val), (max_val)) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = expression(paste("log(", italic(E)[r], ")"))) +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\nstorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_decay3 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = log_rel_eff, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage", y = expression(paste("log(", italic(E)[r], ")")))  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay3[[i]] <- p
  
}





# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )

### --- COMBINE FIGURE ---
par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  

figure4left_core <- ggarrange(
  plot_list_decay[[1]], plot_list_decay[[2]], plot_list_decay[[3]], 
  plot_list_decay2[[1]], plot_list_decay2[[2]], plot_list_decay2[[3]], 
  plot_list_decay3[[1]], plot_list_decay3[[2]], plot_list_decay3[[3]], 
  ncol = 3, nrow = 3, widths = c(1.05, rep(1, 2))) 

# Remove legend from the combined figure
figure4left_core_nolegend <- ggpar(figure4left_core, legend = "none")

# Add title at the very top
figure4left <- annotate_figure(
  figure4left_core_nolegend,
  top = text_grob(
    "RESOURCE AVAILABILITY PERIOD (tu):",
    x = 0.46, 
    hjust = 0.5, 
    size = 14
  )
)



### ===== FIG. 4 mid: Resource Density

### --- Plot A: Contour plots ---

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of nTree
nTree_values <- unique(averages_df2$nTree)[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_prod <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(nTree_values)){
  t <- nTree_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df2[averages_df2$nTree == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = log_rel_eff)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error") +
    labs(title = paste(t), tag = "D") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_prod[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[r], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_prod2 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = log_rel_eff, group = memory_slots)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = expression(paste("log(", italic(E)[r], ")"))) +
    labs(tag = "E") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list_prod2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\nstorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_prod3 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = log_rel_eff, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage", y = expression(paste("log(", italic(E)[r], ")")))  +
    labs(tag = "F") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_prod3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- COMBINE ---

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure4mid_core <- ggarrange(plot_list_prod[[1]], plot_list_prod[[2]], plot_list_prod[[3]], 
                             plot_list_prod2[[1]], plot_list_prod2[[2]], plot_list_prod2[[3]], 
                             plot_list_prod3[[1]], plot_list_prod3[[2]], plot_list_prod3[[3]], 
                             ncol = 3, nrow = 3, widths = rep(1, 3)) 

# Remove legend from the combined figure
figure4mid_core_nolegend <- ggpar(figure4mid_core, legend = "none")

# Add title at the very top
figure4mid <- annotate_figure(
  figure4mid_core_nolegend,
  top = text_grob(
    label = expression("RESOURCE DENSITY (" %*% 10^-6 ~ " trees/alu"^2 * "):"),
    x = 0.55,
    hjust = 0.5,
    size = 14
  )
)



### =====  FIG. 4 right: Patch Spread

### --- Plot A: Contour plots ---

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of cent_std (homogeneity)
cent_std_values <- unique(averages_df3$cent_std)[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_het <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(cent_std_values)){
  t <- cent_std_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df3[averages_df3$cent_std == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = log_rel_eff)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  # Add line when z = 0
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing Error") +
    labs(title = paste(t), tag = "G") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_het[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[r], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_het2 <- list()

# Loop through cent_std values and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = log_rel_eff, group = memory_slots)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error") +
    labs(tag = "H") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list_het2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\nstorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_het3 <- list()

# Loop through cent_stdvalues and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = log_rel_eff, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage")  +
    labs(tag = "I") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_het3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- COMBINE---
par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure4right_core <- ggarrange(plot_list_het[[1]], plot_list_het[[2]], plot_list_het[[3]],
                               legend_plot,
                               plot_list_het2[[1]], plot_list_het2[[2]], plot_list_het2[[3]], 
                               legend_plot2,
                               plot_list_het3[[1]], plot_list_het3[[2]], plot_list_het3[[3]], 
                               legend_plot3, ncol = 4, nrow = 3, widths = c(rep(1, 3), 0.55)) 

# Add title at the very top
figure4right <- annotate_figure(
  figure4right_core,
  top = text_grob(
    "PATCH SPREAD (alu):",
    x = 0.45, 
    hjust = 0.5, 
    size = 14
  )
)





### =====  FIG. 4: Combined the three figures side by side
title_left <- text_grob("RESOURCE AVAILABILITY PERIOD (tu):", size = 14)
title_mid <- text_grob("RESOURCE DENSITY (×10^-6 trees/alu^2):", size = 14)
title_right <- text_grob("PATCH SPREAD (alu):", size = 14)


blank <- ggplot() + theme_void()

titles_row <- ggarrange(
  as_ggplot(title_left),
  blank,
  as_ggplot(title_mid),
  blank,
  as_ggplot(title_right),
  ncol = 5,
  widths = c(0.9, 0.05, 0.87, 0.05, 1)  # 0.05 = gap columns
)

plots_row <- ggarrange(
  figure4left_core_nolegend,
  blank,
  figure4mid_core_nolegend,
  blank,
  figure4right_core,
  ncol = 5,
  widths = c(0.9, 0.05, 0.87, 0.05, 1)
)

png("Fig4.png", width = 3 * 1400, height = 1500, res = 200)

combined_fig <- ggarrange(
  titles_row,
  plots_row,
  nrow = 2,
  heights = c(0.08, 0.92)
)

print(combined_fig)
dev.off()



#=======================================================#
####### FIG. 5: Best performing memory parameters #######
#=======================================================#

# Plot A: Decay Period (averages_df & averages_df13)

# Determine index of rows that were best performing in different environments
row_positions1 <- tapply(seq_along(averages_df$log_rel_eff), averages_df$time_to_dis,
                         FUN = function(x) x[which.max(averages_df$log_rel_eff[x])])

row_positions1_nu <- tapply(seq_along(averages_df13$log_rel_eff), averages_df13$time_to_dis,
                            FUN = function(x) x[which.max(averages_df13$log_rel_eff[x])])

best_vals    <- averages_df[row_positions1, ]
best_vals_nu <- averages_df13[row_positions1_nu, ]

counts    <- as.data.frame(table(best_vals[, c(2, 3)]))
counts_nu <- as.data.frame(table(best_vals_nu[, c(2, 3)]))

# count and remove duplicates
rem_dupl_index    <- !duplicated(best_vals[, c(2, 3)])
rem_dupl_index_nu <- !duplicated(best_vals_nu[, c(2, 3)])

best_vals_n    <- best_vals[rem_dupl_index, ]
best_vals_n_nu <- best_vals_nu[rem_dupl_index_nu, ]

# add counts
best_vals_n_freq1    <- merge(best_vals_n,    
                              counts,    
                              by = c("memory_slots", "inaccuracy_factor"))
best_vals_n_freq1_nu <- merge(best_vals_n_nu, 
                              counts_nu, 
                              by = c("memory_slots", "inaccuracy_factor"))

# order by time to dis
best_vals_n_freq1    <- best_vals_n_freq1[order(best_vals_n_freq1$time_to_dis), ]
best_vals_n_freq1_nu <- best_vals_n_freq1_nu[order(best_vals_n_freq1_nu$time_to_dis), ]

# first point per environment (for red outlines)
first_point_A_main <- best_vals_n_freq1[1, ]
first_point_A_nu   <- best_vals_n_freq1_nu[1, ]

##  FULL LINES (no arrows) 

# main environment: full segments
segments_df <- data.frame(
  x    = best_vals_n_freq1$inaccuracy_factor[-nrow(best_vals_n_freq1)],
  y    = best_vals_n_freq1$memory_slots[-nrow(best_vals_n_freq1)],
  xend = best_vals_n_freq1$inaccuracy_factor[-1],
  yend = best_vals_n_freq1$memory_slots[-1]
)

# second environment: full segments
segments_df_nu <- data.frame(
  x    = best_vals_n_freq1_nu$inaccuracy_factor[-nrow(best_vals_n_freq1_nu)],
  y    = best_vals_n_freq1_nu$memory_slots[-nrow(best_vals_n_freq1_nu)],
  xend = best_vals_n_freq1_nu$inaccuracy_factor[-1],
  yend = best_vals_n_freq1_nu$memory_slots[-1]
)

##  TINY MIDPOINT SEGMENTS (for arrowheads only) 

# Fraction of the full line used for the tiny arrow segment (for arrowhead only)
arrow_frac <- 0.02  # 2% of line length

# main env midpoint segments
halfway_segments_df <- segments_df
dx <- segments_df$xend - segments_df$x
dy <- segments_df$yend - segments_df$y

mid_x <- segments_df$x + 0.5 * dx
mid_y <- segments_df$y + 0.5 * dy

halfway_segments_df$x    <- mid_x - 0.5 * arrow_frac * dx
halfway_segments_df$y    <- mid_y - 0.5 * arrow_frac * dy
halfway_segments_df$xend <- mid_x + 0.5 * arrow_frac * dx
halfway_segments_df$yend <- mid_y + 0.5 * arrow_frac * dy

# second env midpoint segments
halfway_segments_df_nu <- segments_df_nu
dx_nu <- segments_df_nu$xend - segments_df_nu$x
dy_nu <- segments_df_nu$yend - segments_df_nu$y

mid_x_nu <- segments_df_nu$x + 0.5 * dx_nu
mid_y_nu <- segments_df_nu$y + 0.5 * dy_nu

halfway_segments_df_nu$x    <- mid_x_nu - 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$y    <- mid_y_nu - 0.5 * arrow_frac * dy_nu
halfway_segments_df_nu$xend <- mid_x_nu + 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$yend <- mid_y_nu + 0.5 * arrow_frac * dy_nu

##  PLOT A 

plotA <- ggplot(best_vals_n_freq1) +
  # POINTS with mapped color (legend comes ONLY from these)
  geom_point(aes(x = inaccuracy_factor, y = memory_slots, color = "yes"),
             size = best_vals_n_freq1$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  geom_point(data = best_vals_n_freq1_nu,
             aes(x = inaccuracy_factor, y = memory_slots, color = "no"),
             size = best_vals_n_freq1_nu$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  # red outline on first point (main)
  geom_point(data = first_point_A_main,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_A_main$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  # red outline on first point (nu)
  geom_point(data = first_point_A_nu,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_A_nu$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  
  scale_x_continuous(limits = c(0, 1),  expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  
  # Legend 
  scale_color_manual(
    name   = "Active foraging:",
    values = c("yes" = "black", "no" = "darkorange3"),
    labels = c("yes" = "yes", "no" = "no")
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    axis.ticks      = element_line(color = "black"),
    axis.text = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "right",
    legend.key      = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin     = margin(t = 25, r = 5, b = 5, l = 5)
  ) +
  
  labs(x = "Timing error",
       y = "Cognitive storage",
       tag = "A") +
  
  # main env: full lines
  geom_segment(data = segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # main env: tiny midpoint segments with arrowheads
  geom_segment(data = halfway_segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "black", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE) +
  
  # second env: full lines 
  geom_segment(data = segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "darkorange3", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # second env: tiny midpoint segments with arrowheads 
  geom_segment(data = halfway_segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "darkorange3", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE)


# Plot B: nTree (averages_df2 & averages_df14)


# main environment
row_positions2 <- tapply(seq_along(averages_df2$log_rel_eff), averages_df2$nTree,
                         FUN = function(x) x[which.max(averages_df2$log_rel_eff[x])])

best_vals <- averages_df2[row_positions2, ]
counts    <- as.data.frame(table(best_vals[, c(2, 3)]))

rem_dupl_index <- !duplicated(best_vals[, c(2, 3)])
best_vals_n    <- best_vals[rem_dupl_index, ]

best_vals_n_freq2 <- merge(best_vals_n, counts, by = c("memory_slots", "inaccuracy_factor"))
best_vals_n_freq2 <- best_vals_n_freq2[order(best_vals_n_freq2$nTree), ]

# second environment (nu)
row_positions2_nu <- tapply(seq_along(averages_df14$log_rel_eff), averages_df14$nTree,
                            FUN = function(x) x[which.max(averages_df14$log_rel_eff[x])])

best_vals_nu <- averages_df14[row_positions2_nu, ]
counts_nu    <- as.data.frame(table(best_vals_nu[, c(2, 3)]))

rem_dupl_index_nu <- !duplicated(best_vals_nu[, c(2, 3)])
best_vals_n_nu    <- best_vals_nu[rem_dupl_index_nu, ]

best_vals_n_freq2_nu <- merge(best_vals_n_nu, 
                              counts_nu, 
                              by = c("memory_slots", "inaccuracy_factor"))
best_vals_n_freq2_nu <- best_vals_n_freq2_nu[order(best_vals_n_freq2_nu$nTree), ]

# first points for red outline
first_point_B_main <- best_vals_n_freq2[1, ]
first_point_B_nu   <- best_vals_n_freq2_nu[1, ]

##  FULL LINES 

# main env: full segments
segments_df <- data.frame(
  x    = best_vals_n_freq2$inaccuracy_factor[-nrow(best_vals_n_freq2)],
  y    = best_vals_n_freq2$memory_slots[-nrow(best_vals_n_freq2)],
  xend = best_vals_n_freq2$inaccuracy_factor[-1],
  yend = best_vals_n_freq2$memory_slots[-1]
)

# second env: full segments
segments_df_nu <- data.frame(
  x    = best_vals_n_freq2_nu$inaccuracy_factor[-nrow(best_vals_n_freq2_nu)],
  y    = best_vals_n_freq2_nu$memory_slots[-nrow(best_vals_n_freq2_nu)],
  xend = best_vals_n_freq2_nu$inaccuracy_factor[-1],
  yend = best_vals_n_freq2_nu$memory_slots[-1]
)

##  MIDPOINT ARROW SEGMENTS 

# main env midpoint segments
halfway_segments_df <- segments_df
dx <- segments_df$xend - segments_df$x
dy <- segments_df$yend - segments_df$y

mid_x <- segments_df$x + 0.5 * dx
mid_y <- segments_df$y + 0.5 * dy

halfway_segments_df$x    <- mid_x - 0.5 * arrow_frac * dx
halfway_segments_df$y    <- mid_y - 0.5 * arrow_frac * dy
halfway_segments_df$xend <- mid_x + 0.5 * arrow_frac * dx
halfway_segments_df$yend <- mid_y + 0.5 * arrow_frac * dy

# second env midpoint segments
halfway_segments_df_nu <- segments_df_nu
dx_nu <- segments_df_nu$xend - segments_df_nu$x
dy_nu <- segments_df_nu$yend - segments_df_nu$y

mid_x_nu <- segments_df_nu$x + 0.5 * dx_nu
mid_y_nu <- segments_df_nu$y + 0.5 * dy_nu

halfway_segments_df_nu$x    <- mid_x_nu - 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$y    <- mid_y_nu - 0.5 * arrow_frac * dy_nu
halfway_segments_df_nu$xend <- mid_x_nu + 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$yend <- mid_y_nu + 0.5 * arrow_frac * dy_nu

##  PLOT B 

plotB <- ggplot(best_vals_n_freq2) +
  # POINTS with mapped color (legend)
  geom_point(aes(x = inaccuracy_factor, y = memory_slots, color = "yes"),
             size = best_vals_n_freq2$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  geom_point(data = best_vals_n_freq2_nu,
             aes(x = inaccuracy_factor, y = memory_slots, color = "no"),
             size = best_vals_n_freq2_nu$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  # red outline on first point (main)
  geom_point(data = first_point_B_main,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_B_main$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  # red outline on first point (nu)
  geom_point(data = first_point_B_nu,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_B_nu$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  
  scale_x_continuous(limits = c(0, 1),  expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  
  scale_color_manual(
    name   = "Active foraging:",
    values = c("yes" = "black", "no" = "darkorange3"),
    labels = c("yes" = "yes", "no" = "no")
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    axis.ticks      = element_line(color = "black"),
    axis.text = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "right",
    legend.key      = element_blank(),
    plot.margin     = margin(t = 25, r = 5, b = 5, l = 5)
  ) +
  
  labs(x = "Timing error",
       y = "Cognitive storage",
       tag = "B") +
  
  # main env: full lines 
  geom_segment(data = segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # main env: arrowhead at midpoint 
  geom_segment(data = halfway_segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "black", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE) +
  
  # nu env: full lines 
  geom_segment(data = segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "darkorange3", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # nu env: arrowhead at midpoint 
  geom_segment(data = halfway_segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "darkorange3", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE)


# Plot C: cent_std (averages_df3 & averages_df15)


# main environment
row_positions3 <- tapply(seq_along(averages_df3$log_rel_eff), averages_df3$cent_std,
                         FUN = function(x) x[which.max(averages_df3$log_rel_eff[x])])

best_vals <- averages_df3[row_positions3, ]
counts    <- as.data.frame(table(best_vals[, c(2, 3)]))

rem_dupl_index <- !duplicated(best_vals[, c(2, 3)])
best_vals_n    <- best_vals[rem_dupl_index, ]

best_vals_n_freq3 <- merge(best_vals_n, counts, by = c("memory_slots", "inaccuracy_factor"))
best_vals_n_freq3 <- best_vals_n_freq3[order(best_vals_n_freq3$cent_std), ]

# second environment (nu)
row_positions3_nu <- tapply(seq_along(averages_df15$log_rel_eff), averages_df15$cent_std,
                            FUN = function(x) x[which.max(averages_df15$log_rel_eff[x])])

best_vals_nu <- averages_df15[row_positions3_nu, ]
counts_nu    <- as.data.frame(table(best_vals_nu[, c(2, 3)]))

rem_dupl_index_nu <- !duplicated(best_vals_nu[, c(2, 3)])
best_vals_n_nu    <- best_vals_nu[rem_dupl_index_nu, ]

best_vals_n_freq3_nu <- merge(best_vals_n_nu, counts_nu, by = c("memory_slots", "inaccuracy_factor"))
best_vals_n_freq3_nu <- best_vals_n_freq3_nu[order(best_vals_n_freq3_nu$cent_std), ]

# first points for red outline
first_point_C_main <- best_vals_n_freq3[1, ]
first_point_C_nu   <- best_vals_n_freq3_nu[1, ]

##  FULL LINES 

# main env: full segments
segments_df <- data.frame(
  x    = best_vals_n_freq3$inaccuracy_factor[-nrow(best_vals_n_freq3)],
  y    = best_vals_n_freq3$memory_slots[-nrow(best_vals_n_freq3)],
  xend = best_vals_n_freq3$inaccuracy_factor[-1],
  yend = best_vals_n_freq3$memory_slots[-1]
)

# second env: full segments
segments_df_nu <- data.frame(
  x    = best_vals_n_freq3_nu$inaccuracy_factor[-nrow(best_vals_n_freq3_nu)],
  y    = best_vals_n_freq3_nu$memory_slots[-nrow(best_vals_n_freq3_nu)],
  xend = best_vals_n_freq3_nu$inaccuracy_factor[-1],
  yend = best_vals_n_freq3_nu$memory_slots[-1]
)

##  MIDPOINT ARROW SEGMENTS (forward & backward for main env) 

# main env: forward direction arrow at midpoint
halfway_segments_df_fwd <- segments_df
dx <- segments_df$xend - segments_df$x
dy <- segments_df$yend - segments_df$y

mid_x <- segments_df$x + 0.5 * dx
mid_y <- segments_df$y + 0.5 * dy

halfway_segments_df_fwd$x    <- mid_x - 0.5 * arrow_frac * dx
halfway_segments_df_fwd$y    <- mid_y - 0.5 * arrow_frac * dy
halfway_segments_df_fwd$xend <- mid_x + 0.5 * arrow_frac * dx
halfway_segments_df_fwd$yend <- mid_y + 0.5 * arrow_frac * dy

# main env: backward direction arrow at midpoint (reverse)
halfway_segments_df_bwd <- halfway_segments_df_fwd
tmp_x    <- halfway_segments_df_bwd$x
tmp_y    <- halfway_segments_df_bwd$y
halfway_segments_df_bwd$x    <- halfway_segments_df_bwd$xend
halfway_segments_df_bwd$y    <- halfway_segments_df_bwd$yend
halfway_segments_df_bwd$xend <- tmp_x
halfway_segments_df_bwd$yend <- tmp_y

# second env: forward arrow at midpoint
halfway_segments_df_nu <- segments_df_nu
dx_nu <- segments_df_nu$xend - segments_df_nu$x
dy_nu <- segments_df_nu$yend - segments_df_nu$y

mid_x_nu <- segments_df_nu$x + 0.5 * dx_nu
mid_y_nu <- segments_df_nu$y + 0.5 * dy_nu

halfway_segments_df_nu$x    <- mid_x_nu - 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$y    <- mid_y_nu - 0.5 * arrow_frac * dy_nu
halfway_segments_df_nu$xend <- mid_x_nu + 0.5 * arrow_frac * dx_nu
halfway_segments_df_nu$yend <- mid_y_nu + 0.5 * arrow_frac * dy_nu

##  PLOT C 

plotC <- ggplot(best_vals_n_freq3) +
  # POINTS with mapped color (legend)
  geom_point(aes(x = inaccuracy_factor, y = memory_slots, color = "yes"),
             size = best_vals_n_freq3$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  geom_point(data = best_vals_n_freq3_nu,
             aes(x = inaccuracy_factor, y = memory_slots, color = "no"),
             size = best_vals_n_freq3_nu$Freq * 0.8,
             shape = 19, alpha = 0.6) +
  # red outline on first point (main)
  geom_point(data = first_point_C_main,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_C_main$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  # red outline on first point (nu)
  geom_point(data = first_point_C_nu,
             aes(x = inaccuracy_factor, y = memory_slots),
             size = first_point_C_nu$Freq * 0.8,
             shape = 21, fill = NA, colour = "red", stroke = 0.7) +
  
  scale_x_continuous(limits = c(0, 1),  expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  
  scale_color_manual(
    name   = "Active foraging:",
    values = c("yes" = "black", "no" = "darkorange3"),
    labels = c("yes" = "yes", "no" = "no")
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    axis.ticks      = element_line(color = "black"),
    axis.text = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "right",
    legend.key      = element_blank(),
    plot.margin     = margin(t = 25, r = 5, b = 5, l = 5)
  ) +
  
  labs(x = "Timing error",
       y = "Cognitive storage",
       tag = "C") +
  
  # main env: full lines 
  geom_segment(data = segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # main env: arrowhead at midpoint (forward)
  geom_segment(data = halfway_segments_df_fwd,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "black", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE) +
  # main env: arrowhead at midpoint (backward) 
  geom_segment(data = halfway_segments_df_bwd,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "black", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE) +
  
  # nu env: full lines 
  geom_segment(data = segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "darkorange3", alpha = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  # nu env: arrowhead at midpoint (forward) 
  geom_segment(data = halfway_segments_df_nu,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "darkorange3", alpha = 0.9,
               inherit.aes = FALSE, show.legend = FALSE)



### --- COMBINE AND EXPORT FIGURE ---

png("Fig5.png", width = 1700, height = 500, res = 200)

figure <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1,
  common.legend = TRUE,
  legend = "right"
)

print(figure)

dev.off()



#=================================#
###### 6. Figures Scenario 2 ######
#=================================#

#===================================================#
######  FIG. 6: Linear trade-off (Non-dynamic) ######
#===================================================#

### ===== FIG. 6 top: Active + passive forgetting

min_val <- -1.6
max_val <- 2.1

unique_mem <- sort(unique(averages_df$memory_slots))
unique_inac <- sort(unique(averages_df$inaccuracy_factor))
combinations <- data.frame(memory_slots = unique_mem, inaccuracy_factor = unique_inac)

# df 1 to 3
tradeoff_df  <- merge(averages_df,  combinations, by = c("memory_slots", "inaccuracy_factor"))
tradeoff_df2 <- merge(averages_df2, combinations, by = c("memory_slots", "inaccuracy_factor"))
tradeoff_df3 <- merge(averages_df3, combinations, by = c("memory_slots", "inaccuracy_factor"))

# the values to use
time_to_dis_vals <- c(0.25, 4, 32)
nTree_vals       <- c(25, 400, 3200)
cent_std_vals    <- c(2.5, 40, 320)

tradeoff_df_vals  <- tradeoff_df[ tradeoff_df$time_to_dis %in% time_to_dis_vals, ]
tradeoff_df2_vals <- tradeoff_df2[tradeoff_df2$nTree      %in% nTree_vals,       ]
tradeoff_df3_vals <- tradeoff_df3[tradeoff_df3$cent_std   %in% cent_std_vals,    ]

# values for axes:
mem_vals        <- sort(unique(tradeoff_df_vals$memory_slots))
timing_err_vals <- sort(unique(tradeoff_df_vals$inaccuracy_factor))
timing_err_vals[seq(2, length(timing_err_vals), by = 2)] <- ""


### --- Top row: A, B, C ---

plotA <- ggplot(data = tradeoff_df_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = time_to_dis)) +
  ylim(min_val, max_val) +
  labs(y = expression(paste("log(", italic(E)[r], ")")), 
       tag = "A") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) +
  geom_line(aes(linetype = factor(time_to_dis)))  +
  geom_point(color = "black", aes(shape = factor(time_to_dis)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Resource Availability Period:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",  # title above symbols
      nrow = 1,                # points in one row
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotB <- ggplot(data = tradeoff_df2_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = nTree)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "B") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) + 
  geom_line(aes(linetype = factor(nTree))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(nTree)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Resource Density:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotC <- ggplot(data = tradeoff_df3_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = cent_std)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "C") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) + 
  geom_line(aes(linetype = factor(cent_std))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(cent_std)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Patch Spread:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Top Plot (A–C side by side) ---

figure6_top <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1
)



### ===== FIG. 6 bot: Passive forgetting

min_val <- -1.6
max_val <- 2.1

unique_mem <- sort(unique(averages_df13$memory_slots))
unique_inac <- sort(unique(averages_df13$inaccuracy_factor))
combinations <- data.frame(memory_slots = unique_mem, inaccuracy_factor = unique_inac)

# df 13 to 15
tradeoff_df13 <- merge(averages_df13, combinations, by = c("memory_slots", "inaccuracy_factor"))
tradeoff_df14 <- merge(averages_df14, combinations, by = c("memory_slots", "inaccuracy_factor"))
tradeoff_df15 <- merge(averages_df15, combinations, by = c("memory_slots", "inaccuracy_factor"))

time_to_dis_vals <- c(0.25, 4, 32)
nTree_vals       <- c(25, 400, 3200)
cent_std_vals    <- c(2.5, 40, 320)

tradeoff_df13_vals <- tradeoff_df13[tradeoff_df13$time_to_dis %in% time_to_dis_vals, ]
tradeoff_df14_vals <- tradeoff_df14[tradeoff_df14$nTree      %in% nTree_vals,       ]
tradeoff_df15_vals <- tradeoff_df15[tradeoff_df15$cent_std   %in% cent_std_vals,    ]

mem_vals        <- sort(unique(tradeoff_df13_vals$memory_slots))
timing_err_vals <- sort(unique(tradeoff_df13_vals$inaccuracy_factor))
timing_err_vals[seq(2, length(timing_err_vals), by = 2)] <- ""


### --- Bottom row: D, E, F ---

plotA <- ggplot(data = tradeoff_df13_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = time_to_dis)) +
  ylim(min_val, max_val) +
  labs(y = expression(paste("log(", italic(E)[r], ")")), 
       tag = "D") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) +
  geom_line(aes(linetype = factor(time_to_dis)))  +
  geom_point(color = "black", aes(shape = factor(time_to_dis)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotB <- ggplot(data = tradeoff_df14_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = nTree)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(),
       tag = "E") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) + 
  geom_line(aes(linetype = factor(nTree))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(nTree)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotC <- ggplot(data = tradeoff_df15_vals[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = cent_std)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "F") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals,
    sec.axis = dup_axis( 
      name   = "Timing error",
      breaks = mem_vals,            
      labels = timing_err_vals     
    )
  ) + 
  geom_line(aes(linetype = factor(cent_std))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(cent_std)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Bottom Plot (D–F side by side) ---

figure6_bot <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1
)


### =====  FIG. 6: Combine and export top and bot

title1 <- text_grob("Active forgetting",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)
title2 <- text_grob("No active forgetting",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)

plots_row <- ggarrange(
  as_ggplot(title1),
  figure6_top,
  as_ggplot(title2),
  figure6_bot,
  ncol    = 1,
  heights = c(0.1, 1, 0.1, 0.8)
)

png("Fig6.png", width = 1500, height = 1000, res = 200)
print(plots_row)
dev.off()






#=============================================#
###### 7. Descriptive stats: Scenario 3 #######
#=============================================#

get_max_mem_slot <- function(df, group_var) {
  # df: data frame with columns eaten, memory_slots, and grouping variable
  # group_var: name of environmental grouping variable in df
  
  idx_list <- split(seq_len(nrow(df)), df[[group_var]])
  
  sapply(idx_list, function(idx) {
    group_eaten <- df$eaten[idx]
    group_slots <- df$memory_slots[idx]
    group_slots[which.max(group_eaten)]
  })
}

# Scenario 3 (no forgetting)
result1 <- get_max_mem_slot(tradeoff_df,  "time_to_dis")
result2 <- get_max_mem_slot(tradeoff_df2, "nTree")
result3 <- get_max_mem_slot(tradeoff_df3, "cent_std")

# Scenario 3 (with forgetting)
result_forg1 <- get_max_mem_slot(tradeoff_df13, "time_to_dis")
result_forg2 <- get_max_mem_slot(tradeoff_df14, "nTree")
result_forg3 <- get_max_mem_slot(tradeoff_df15, "cent_std")

average_mem_slot      <- mean(c(result1,      result2,      result3))
average_forg_mem_slot <- mean(c(result_forg1, result_forg2, result_forg3))

(average_forg_mem_slot / average_mem_slot) * 100





#=============================================#
###### 8. Figures Supplementary Material ######
#=============================================#

#====================================#
###### FIG. S1: Time Estimation ######
#====================================#

# use step-length of 100 alu. 
# movement speed is 500 alu tu-1
# maturation period is 30 tu. 


maturationtime <- 30
passed_time <- 100 / 500 # time that passes with 100 alu
iterations <- (maturationtime / passed_time) + 1
actual_passed_time <- seq(from = 0, by = passed_time, length.out = iterations)

set.seed(10)

# one run for each inaccuracy value
inac_fact_vals <- c(0.1, 0.55, 1.0)

### inac 0.1
estimated_time <- rep(NA, iterations)
estimated_time[1] <- 0 
cum_estimated_time <- rep(NA, iterations) #cumulative
cum_estimated_time[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time <- cumsum(estimated_time)
  estimated_time[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time[i - 1], 
                                      b = Inf, 
                                      mean = passed_time, 
                                      sd = inac_fact_vals[1])
}

### inac 0.6
estimated_time2 <- rep(NA, iterations)
estimated_time2[1] <- 0 
cum_estimated_time2 <- rep(NA, iterations) #cumulative
cum_estimated_time2[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time2 <- cumsum(estimated_time2)
  estimated_time2[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time2[i - 1], 
                                       b = Inf, 
                                       mean = passed_time, 
                                       sd = inac_fact_vals[2])
}


### inac 1.2
estimated_time3 <- rep(NA, iterations)
estimated_time3[1] <- 0 
cum_estimated_time3 <- rep(NA, iterations) #cumulative
cum_estimated_time3[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time3 <- cumsum(estimated_time3)
  estimated_time3[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time3[i - 1], 
                                       b = Inf, 
                                       mean = passed_time, 
                                       sd = inac_fact_vals[3])
}


# Combine into one data frame
df_plot <- data.frame(
  actual_time = rep(actual_passed_time, 3),
  estimated_time = c(cumsum(estimated_time), cumsum(estimated_time2), cumsum(estimated_time3)),
  inac_fact = factor(c(rep(inac_fact_vals[1], 151), 
                       rep(inac_fact_vals[2], 151), 
                       rep(inac_fact_vals[3], 151)))
)

# Point symbols for each inac_fact level 
shape_values <- c(1, 2, 3)  


### === Plot
plotA <- ggplot(df_plot, aes(x = actual_time, y = estimated_time, group = inac_fact)) +
  # Reference line 
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "solid", linewidth = 0.8) +
  
  # Main data
  geom_line(color = "black", aes(linetype = inac_fact)) +
  
  labs(x = "Actual Passed Time",  
       y = "Estimated Time") +
  
  # Axes start at 0
  scale_x_continuous(limits = c(0, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 61), expand = c(0, 0)) +
  
  # Theme
  theme_classic(base_size = 11) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    plot.margin = margin(t = 25, r = 5, b = 5, l = 5)
  ) +
  scale_linetype_discrete(name = "Timing error")


####### multiple runs: FIGURE 3C: Time Estimation
# Parameters
inac_fact_vals <- c(0.1, 0.55, 1.0)
maturationtime <- 30
passed_time <- 100 / 500
iterations <- (maturationtime / passed_time) + 1
actual_passed_time <- seq(from = 0, by = passed_time, length.out = iterations)
num_simulations <- 1000

set.seed(10)
# List to store plots
plot_list <- list()

# Loop through each inaccuracy value
for (i in seq_along(inac_fact_vals)) {
  inac <- inac_fact_vals[i]
  results <- matrix(NA, nrow = num_simulations, ncol = iterations)
  
  for (sim in 1:num_simulations) {
    estimated_time <- numeric(iterations)
    estimated_time[1] <- rtruncnorm(1, a = 0, b = Inf, mean = passed_time, sd = 0.1)
    
    for (j in 2:iterations) {
      cum_estimated_time <- cumsum(estimated_time)
      estimated_time[j] <- rtruncnorm(1, a = -1 * cum_estimated_time[j - 1], b = Inf,
                                      mean = passed_time, sd = inac)
    }
    
    results[sim, ] <- cumsum(estimated_time)
  }
  
  mean_est <- colMeans(results)
  sd_lower <- mean_est - apply(results, 2, sd)  
  sd_upper <- mean_est + apply(results, 2, sd)
  
  plot_data <- data.frame(
    time = actual_passed_time,
    mean = mean_est,
    lower = sd_lower,
    upper = sd_upper
  )
  
  p <- ggplot(plot_data, aes(x = time, y = mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.3) +
    geom_line(color = "black", size = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", 
                linetype = "solid", linewidth = 0.8) +
    labs(
      x = "Actual Passed Time",
      y = if (i == 1) "Estimated Time" else "",
      title = if (i == 1) "B.Timing error:" else "",
      subtitle = paste0(inac)
    ) +
    
    scale_x_continuous(limits = c(0, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 61), expand = c(0, 0)) +
    
    theme_classic() +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = 0.5))
  
  plot_list[[i]] <- p
}

# Unwrapping plots
plotc1 <- plot_list[[1]]
plotc2 <- plot_list[[2]]
plotc3 <- plot_list[[3]]


### --- COMBINE AND EXPORT FIGURE ---

png("FigS1.png", width = 1500, height = 1000, res = 200)

# p1 (top full row) and p2/p3/p3 (bottom split)
figure <- ggarrange(
  plotA,
  ggarrange(plotc1, plotc2, plotc3, ncol = 3),  # bottom row
  nrow = 2,
  heights = c(1, 1),  
  labels = "A."       
)

print(figure)

dev.off()





#=================================================================#
###### FIG. S2: Trade-off Best performing memory parameters  ######
#=================================================================#

### ===== FIG. S2 top: Active + Passive forgetting

#Function to return rows with max eaten per level of environment variable (group_var)
get_max_rows <- function(df, group_var) {
  grp        <- df[[group_var]]
  max_eff  <- ave(df$log_rel_eff, grp, FUN = max)
  max_rows   <- df[df$log_rel_eff == max_eff, , drop = FALSE]
  max_rows[order(max_rows[[group_var]]), , drop = FALSE]
}

max_rows   <- get_max_rows(tradeoff_df,   "time_to_dis")
max_rows2  <- get_max_rows(tradeoff_df2,  "nTree")
max_rowS2  <- get_max_rows(tradeoff_df3,  "cent_std")

max_rows13 <- get_max_rows(tradeoff_df13, "time_to_dis")
max_rows14 <- get_max_rows(tradeoff_df14, "nTree")
max_rows15 <- get_max_rows(tradeoff_df15, "cent_std")



### --- Top row: A, B, C ---
min_val <- 0 
max_val <- 15


plotA <- ggplot(data = max_rows, 
                aes(x = time_to_dis, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Optimal Cognitive Storage", 
       tag = "A") + 
  scale_x_continuous( 
    name   = "Resource Availability Period"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotB <- ggplot(data = max_rows2, 
                aes(x = nTree, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Optimal Cognitive Storage", 
       tag = "B") + 
  scale_x_continuous( 
    name   = "Resource Density"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotC <- ggplot(data = max_rowS2, 
                aes(x = cent_std, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Optimal Cognitive Storage", 
       tag = "C") + 
  scale_x_continuous( 
    name   = "Patch Spread"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Top Plot (A–C side by side) ---

figureS2_top <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1
)



### ===== FIG. S2 bot: Passive forgetting

### --- Bottom row: D, E, F ---

plotD <- ggplot(data = max_rows13, 
                aes(x = time_to_dis, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Optimal Cognitive Storage", 
       tag = "D") + 
  scale_x_continuous( 
    name   = "Resource Availability Period"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotE <- ggplot(data = max_rows14, 
                aes(x = nTree, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Optimal Cognitive Storage", 
       tag = "E") + 
  scale_x_continuous( 
    name   = "Resource Density"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotF <- ggplot(data = max_rows15, 
                aes(x = cent_std, 
                    y = memory_slots)) +
  ylim(min_val, max_val) +
  labs(y = "Cognitive Storage", 
       tag = "F") + 
  scale_x_continuous( 
    name   = "Patch Spread"
  ) + 
  geom_line()  +
  geom_point(color = "black",
             size = 1.5, stroke = 0.3) + 
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Bottom Plot (D–F side by side) ---

figureS2_bot <- ggarrange(
  plotD, plotE, plotF,
  nrow = 1
)






### =====  FIG. S2: Combine and export top and bot

title1 <- text_grob("Active forgetting",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)
title2 <- text_grob("No active forgetting",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)

plots_row <- ggarrange(
  as_ggplot(title1),
  figureS2_top,
  as_ggplot(title2),
  figureS2_bot,
  ncol    = 1,
  heights = c(0.1, 1, 0.1, 1)
)

png("FigS2.png", width = 1500, height = 1000, res = 200)
print(plots_row)
dev.off()














#=======================================#
######  FIG. S3: Dynamic trade-off ######
#=======================================#

### ===== FIG. S3 top: linear trade-off

min_val <- -1.6
max_val <- 2.1

# the values to use
time_to_dis_vals <- c(0.25, 4, 32)
nTree_vals       <- c(25, 400, 3200)
cent_std_vals    <- c(2.5, 40, 320)

# values for axes:
mem_vals        <- sort(unique(averages_df7$memory_slots))


### --- Top row: A, B, C ---

plotA <- ggplot(data = averages_df7[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = time_to_dis)) +
  ylim(min_val, max_val) +
  labs(y = expression(paste("log(", italic(E)[r], ")")), 
       tag = "A") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) +
  geom_line(aes(linetype = factor(time_to_dis)))  +
  geom_point(color = "black", aes(shape = factor(time_to_dis)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Resource Availability Period:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",  # title above symbols
      nrow = 1,                # points in one row
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    # move tag up into same band as legend
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    # extra space at top so tag+legend fit
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotB <- ggplot(data = averages_df8[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = nTree)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "B") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) + 
  geom_line(aes(linetype = factor(nTree))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(nTree)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Resource Density:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotC <- ggplot(data = averages_df9[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = cent_std)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "C") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) + 
  geom_line(aes(linetype = factor(cent_std))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(cent_std)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = "Patch Spread:"
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 7),
    legend.key   = element_blank(),
    legend.position = "top",
    legend.box      = "vertical",
    legend.title.align = 0.5,
    legend.margin      = margin(t = 0, r = 0, b = -5, l = 0),
    legend.box.margin  = margin(t = 0, r = 0, b = 0,  l = 0),
    
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Top Plot (A–C side by side) ---

figureS3_top <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1
)



### ===== FIG. S3 bot: Passive forgetting


### --- Bottom row: D, E, F ---

plotA <- ggplot(data = averages_df10[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = time_to_dis)) +
  ylim(min_val, max_val) +
  labs(y = expression(paste("log(", italic(E)[r], ")")), 
       tag = "D") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) +
  geom_line(aes(linetype = factor(time_to_dis)))  +
  geom_point(color = "black", aes(shape = factor(time_to_dis)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotB <- ggplot(data = averages_df11[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = nTree)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(),
       tag = "E") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) + 
  geom_line(aes(linetype = factor(nTree))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(nTree)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )

plotC <- ggplot(data = averages_df12[, ], 
                aes(x = memory_slots, 
                    y = log_rel_eff, 
                    group = cent_std)) +
  ylim(-0.7, 0.9) +
  labs(y = element_blank(), 
       tag = "F") + 
  scale_x_continuous( 
    name   = "Cognitive storage",
    breaks = mem_vals
  ) + 
  geom_line(aes(linetype = factor(cent_std))) +
  guides(linetype = "none") +
  geom_point(color = "black", aes(shape = factor(cent_std)),
             size = 1.5, stroke = 0.3) + 
  scale_shape_manual(
    values = c(15, 16, 17),
    name   = ""
  ) +
  guides(
    linetype = "none",
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0, 1.00),
    plot.tag = element_text(face = "bold", vjust = 1.0),
    
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5) 
  )


### --- Bottom Plot (D–F side by side) ---

figureS3_bot <- ggarrange(
  plotA, plotB, plotC,
  nrow = 1
)


### =====  FIG. S3: Combine and export top and bot

title1 <- text_grob("Linear trade-off",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)
title2 <- text_grob("Exponential trade-off",
                    size = 12,
                    hjust = 0, x = 0.01,
                    y = 0.5, vjust = 0.5)

plots_row <- ggarrange(
  as_ggplot(title1),
  figureS3_top,
  as_ggplot(title2),
  figureS3_bot,
  ncol    = 1,
  heights = c(0.1, 1, 0.1, 0.8)
)

png("FigS3.png", width = 1500, height = 1000, res = 200)
print(plots_row)
dev.off()







#============================================================#
###### FIG. S4: Forgetting Relative Foraging Efficiency ######
#============================================================#


### === FIRST: Compute foraging efficiency of cognitive foragers that were able to actively 
# forget (from scenario 1) relative to the efficiency of cognitive foragers 
# that did not possess the ability for active forgetting

# df1
averages_df_forg <- merge(averages_df, averages_df13[, 1:4], 
                          by = c("inaccuracy_factor","time_to_dis", "memory_slots"))

colnames(averages_df_forg)[4] <- "eaten"
colnames(averages_df_forg)[8] <- "eaten_no_forgt"
averages_df_forg$rel_eff_forg <- (averages_df_forg$eaten + 1) / (averages_df_forg$eaten_no_forgt + 1)


# df2
averages_df_forg2 <- merge(averages_df2, averages_df14[, 1:4], 
                           by = c("inaccuracy_factor","nTree", "memory_slots"))

colnames(averages_df_forg2)[4] <- "eaten"
colnames(averages_df_forg2)[8] <- "eaten_no_forgt"
averages_df_forg2$rel_eff_forg <- (averages_df_forg2$eaten + 1) / (averages_df_forg2$eaten_no_forgt + 1)

# df3
averages_df_forg3 <- merge(averages_df3, averages_df15[, 1:4], 
                           by = c("inaccuracy_factor","cent_std", "memory_slots"))

colnames(averages_df_forg3)[4] <- "eaten"
colnames(averages_df_forg3)[8] <- "eaten_no_forgt"
averages_df_forg3$rel_eff_forg <- (averages_df_forg3$eaten + 1) / (averages_df_forg3$eaten_no_forgt + 1)






### ===== FIG. S4 left: Decay Period

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(c(averages_df_forg$rel_eff_forg,
               averages_df_forg2$rel_eff_forg,
               averages_df_forg3$rel_eff_forg))

z_max <- max(c(averages_df_forg$rel_eff_forg,
               averages_df_forg2$rel_eff_forg,
               averages_df_forg3$rel_eff_forg))

min_val <- 0.4
max_val <- 2.8

# Define contour levels and color breaks (same for all plots)
levels <- round(seq(min_val, max_val, length.out = 11), 1)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of time_to_dis
time_values <- sort(unique(averages_df_forg$time_to_dis))[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_decay <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(time_values)){
  t <- time_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df_forg[averages_df_forg$time_to_dis == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_eff_forg)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  # Add line when z = 0
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = "Cognitive storage") +
    labs(title = paste(t), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[f], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_decay2 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df_forg, 
                        time_to_dis == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_eff_forg, group = memory_slots)) +
    ylim(floor(min_val), (max_val)) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = expression(paste("log(", italic(E)[f], ")"))) +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\ntorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_decay3 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df_forg, 
                        time_to_dis == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_eff_forg, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage", y = expression(paste("log(", italic(E)[f], ")")))  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_decay3[[i]] <- p
  
}





# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )

### --- COMBINE FIGURE ---
par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figureS4left_core <- ggarrange(
  plot_list_decay[[1]], plot_list_decay[[2]], plot_list_decay[[3]], 
  plot_list_decay2[[1]], plot_list_decay2[[2]], plot_list_decay2[[3]], 
  plot_list_decay3[[1]], plot_list_decay3[[2]], plot_list_decay3[[3]], 
  ncol = 3, nrow = 3, widths = c(1.05, rep(1, 2))) 

# Remove legend from the combined figure
figureS4left_core_nolegend <- ggpar(figureS4left_core, legend = "none")

# Add title at the very top
figureS4left <- annotate_figure(
  figureS4left_core_nolegend,
  top = text_grob(
    "RESOURCE AVAILABILITY PERIOD (tu):",
    x = 0.46, 
    hjust = 0.5, 
    size = 14
  )
)



### ===== FIG. S4 mid: Resource Density

### --- Plot A: Contour plots ---

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of nTree
nTree_values <- sort(unique(averages_df_forg2$nTree))[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_prod <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(nTree_values)){
  t <- nTree_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df_forg2[averages_df_forg2$nTree == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_eff_forg)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  # Add line when z = 0
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error") +
    labs(title = paste(t), tag = "D") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_prod[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[f], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_prod2 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df_forg2, 
                        nTree == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_eff_forg, group = memory_slots)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error", y = expression(paste("log(", italic(E)[f], ")"))) +
    labs(tag = "E") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list_prod2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\nstorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_prod3 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df_forg2, 
                        nTree == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_eff_forg, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage", y = expression(paste("log(", italic(E)[f], ")")))  +
    labs(tag = "F") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_prod3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- COMBINE ---

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figureS4mid_core <- ggarrange(plot_list_prod[[1]], plot_list_prod[[2]], plot_list_prod[[3]], 
                              plot_list_prod2[[1]], plot_list_prod2[[2]], plot_list_prod2[[3]], 
                              plot_list_prod3[[1]], plot_list_prod3[[2]], plot_list_prod3[[3]], 
                              ncol = 3, nrow = 3, widths = rep(1, 3)) 

# Remove legend from the combined figure
figureS4mid_core_nolegend <- ggpar(figureS4mid_core, legend = "none")

# Add title at the very top
figureS4mid <- annotate_figure(
  figureS4mid_core_nolegend,
  top = text_grob(
    label = expression("RESOURCE DENSITY (" %*% 10^-6 ~ " trees/alu"^2 * "):"),
    x = 0.55,
    hjust = 0.5,
    size = 14
  )
)



### =====  FIG. S4 right: Patch Spread

### --- Plot A: Contour plots ---

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of cent_std (homogeneity)
cent_std_values <- sort(unique(averages_df_forg3$cent_std))[c(1, 5, 8)]

# Create empty list to store the plots
plot_list_het <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(cent_std_values)){
  t <- cent_std_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df_forg3[averages_df_forg3$cent_std == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_eff_forg)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = log(1), color = "red", size = 0.6) +  # Add line when z = 0
    scale_x_continuous(expand = c(0, 0)) +  # No margin on x
    scale_y_continuous(expand = c(0, 0)) +  # No margin on y
    coord_cartesian(expand = FALSE) +       # Keep border tight
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Tight box around panel
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(5, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing Error") +
    labs(title = paste(t), tag = "G") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_het[[i]] <- p
}


# Create data for colours in legend
legend_df <- data.frame(
  ymin = head(levels, -1),
  ymax = tail(levels, -1),
  fill = gray_palette
)

# Create text labels at border positions (just top and bottom of each band)
text_df <- data.frame(
  y = levels,                    # y positions for labels
  label = levels
)

#Plot
legend_plot <- ggplot() +
  # Colored blocks
  geom_rect(data = legend_df,
            aes(xmin = 1, xmax = 1.1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 1, xmax = 1.1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.17, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle(expression(paste("log(", italic(E)[f], ")"))) +
  scale_fill_identity() +
  coord_fixed(ratio = 0.2, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  
    plot.title = element_text(size = 13)
  )


### --- Plot B: Cross Section Cognitive Storage ---

# Define three specific memory_slots values
selected_memory_slots <- c(1, 3, 5, 9, 23)

# Create empty list to store the plots
plot_list_het2 <- list()

# Loop through cent_std values and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df_forg3, 
                        cent_std == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_eff_forg, group = memory_slots)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = memory_slots, fill = memory_slots), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Timing error") +
    labs(tag = "H") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list_het2[[i]] <- p
  
}

# Dummy data for legend
legend_df <- data.frame(
  memory_slots = factor(c("1", "3", "5", "9", "23"), 
                        levels = c("1", "3", "5", "9", "23")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot2 <- ggplot(legend_df, 
                       aes(x = x, y = y, 
                           shape = memory_slots, 
                           fill = memory_slots)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Cognitive\nstorage",  # Title for the legend
    fill = "Cognitive\nstorage") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- Plot C: Cross Section Timing Inaccuracy ---

# Define three specific inaccuracy values
selected_inaccuracy <- c(0.01, 0.28, 0.55, 0.82, 1.00)

# Create empty list to store the plots
plot_list_het3 <- list()

# Loop through cent_stdvalues and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df_forg3, 
                        cent_std == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_eff_forg, 
                  group = inaccuracy_factor)) +
    ylim(floor(min_val), max_val) +
    geom_line() +
    geom_hline(yintercept = log(1), linetype = "dashed", color = "red") +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    scale_shape_manual(values = c(21, 22, 21, 22, 21)) +  # All fillable shapes
    scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = element_blank(),
      axis.ticks = element_line(colour = "black")) +  
    labs(x = "Cognitive storage")  +
    labs(tag = "I") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list_het3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.01", "0.28", "0.55", "0.82", "1.00"), 
                             levels = c("0.01", "0.28", "0.55", "0.82", "1.00")),
  x = 9999,  # Place far outside the visible plot
  y = 9999
)

legend_plot3 <- ggplot(legend_df, 
                       aes(x = x, y = y,
                           shape = inaccuracy_factor, 
                           fill = inaccuracy_factor)) +
  geom_point(color = "black", size = 4, stroke = 0.5) +  # Triggers the legend
  scale_shape_manual(values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual(values = c("grey100", "grey80", "grey50", "grey30", "grey0")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Keep the plot area blank
  theme_void() +
  labs(
    shape = "Timing\nerror",  # Title for the shape legend
    fill = "Timing\nerror") +
  theme(
    legend.position = "left",
    plot.margin = margin(5, 5, 5, 45),  # top, right, bottom, left    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      shape = c(21, 22, 21, 22, 21),
      fill = c("grey100", "grey80", "grey50", "grey30", "grey0"),
      color = "black",
      size = 4,
      stroke = 0.5
    )),
    fill = "none"  # prevent duplicated legend
  )


### --- COMBINE---
par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figureS4right_core <- ggarrange(plot_list_het[[1]], plot_list_het[[2]], plot_list_het[[3]],
                                legend_plot,
                                plot_list_het2[[1]], plot_list_het2[[2]], plot_list_het2[[3]], 
                                legend_plot2,
                                plot_list_het3[[1]], plot_list_het3[[2]], plot_list_het3[[3]], 
                                legend_plot3, ncol = 4, nrow = 3, widths = c(rep(1, 3), 0.55)) 

# Add title at the very top
figureS4right <- annotate_figure(
  figureS4right_core,
  top = text_grob(
    "PATCH SPREAD (alu):",
    x = 0.45, 
    hjust = 0.5, 
    size = 14
  )
)





### =====  FIG. S4: Combined the three figures side by side
title_left <- text_grob("RESOURCE AVAILABILITY PERIOD (tu):", size = 14)
title_mid <- text_grob("RESOURCE DENSITY (×10^-6 trees/alu^2):", size = 14)
title_right <- text_grob("PATCH SPREAD (alu):", size = 14)


blank <- ggplot() + theme_void()

titles_row <- ggarrange(
  as_ggplot(title_left),
  blank,
  as_ggplot(title_mid),
  blank,
  as_ggplot(title_right),
  ncol = 5,
  widths = c(0.9, 0.05, 0.87, 0.05, 1)  # 0.05 = gap columns
)

plots_row <- ggarrange(
  figureS4left_core_nolegend,
  blank,
  figureS4mid_core_nolegend,
  blank,
  figureS4right_core,
  ncol = 5,
  widths = c(0.9, 0.05, 0.87, 0.05, 1)
)

png("FigS4.png", width = 3 * 1400, height = 1500, res = 200)

combined_fig <- ggarrange(
  titles_row,
  plots_row,
  nrow = 2,
  heights = c(0.08, 0.92)
)

print(combined_fig)
dev.off()








#=================================================#
###### FIG. S5: Absolute Foraging Efficiency ######
#=================================================#


### --- Plot A: Absolute efficiency best cognitive performer and sensory forager vs Decay Period ---

# Get sensory efficiency per decay period
high_eff <- averages_df4
names(high_eff) <- c("time_to_dis", "abs_eff")

# Identify absolute efficiency of best performer per decay period
high_eff <- rbind(high_eff, 
                  data.frame(
                    time_to_dis = unique(averages_df$time_to_dis),
                    abs_eff = tapply(averages_df$eaten, averages_df$time_to_dis, FUN = max)
                  )
)

high_eff$type <- c(rep("sensory", 8), rep("cognitive", 8))

# Create Plot
plot <- ggplot(high_eff, aes(x = time_to_dis, y = abs_eff, group = type)) +
  geom_line(aes(linetype = type), show.legend = FALSE) +  # only shapes in legend
  geom_point(aes(shape = type),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "Forager type:",
    values = c("sensory"    = 19,  # circle
               "cognitive" = 15), # square
    labels = c("sensory"    = "sensory",
               "cognitive" = "cognitive")
  ) +
  ylim(0, 210) +
  labs(
    x = "Resource Availability Period",
    y = expression(paste(italic(E)[s], " or ", italic(E)[c])),
    tag = "A"
  ) +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  )


### --- Plot B: Net Benefit vs Productivity ---

# Get sensory efficiency per nTree
high_eff2 <- averages_df5
names(high_eff2) <- c("nTree", "abs_eff")

# Identify absolute efficiency of best performer per nTree
high_eff2 <- rbind(high_eff2, 
                   data.frame(
                     nTree = unique(averages_df2$nTree),
                     abs_eff = tapply(averages_df2$eaten, averages_df2$nTree, FUN = max)
                   )
)

high_eff2$type <- c(rep("sensory", 8), rep("cognitive", 8))


# Create Plot
plot2 <- ggplot(high_eff2, aes(x = nTree, y = abs_eff, group = type)) +
  geom_line(aes(linetype = type), show.legend = FALSE) +
  geom_point(aes(shape = type),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "Forager type:",
    values = c("sensory"    = 19,  # circle
               "cognitive" = 15), # square
    labels = c("sensory"    = "sensory",
               "cognitive" = "cognitive")
  ) +
  ylim(0, 210) +
  labs(x = "Resource Density", tag = "B") +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  )


### --- Plot C: Net Benefit vs Patch Spread ---

# Get sensory efficiency per cent_std
high_eff3 <- averages_df6
names(high_eff3) <- c("cent_std", "abs_eff")

# Identify absolute efficiency of best performer per cent_std
high_eff3 <- rbind(high_eff3, 
                   data.frame(
                     cent_std = unique(averages_df3$cent_std),
                     abs_eff = tapply(averages_df3$eaten, averages_df3$cent_std, FUN = max)
                   )
)

high_eff3$type <- c(rep("sensory", 8), rep("cognitive", 8))



# Create Plot
plot3 <- ggplot(high_eff3, aes(x = cent_std, y = abs_eff, group = type)) +
  geom_line(aes(linetype = type), show.legend = FALSE) +
  geom_point(aes(shape = type),
             color = "black", fill = "black", size = 2) +
  scale_shape_manual(
    name   = "Forager type:",
    values = c("sensory"    = 19,  # circle
               "cognitive" = 15), # square
    labels = c("sensory"    = "sensory",
               "cognitive" = "cognitive")
  ) +
  ylim(0, 210) +
  labs(x = "Patch Spread", tag = "C") +
  theme_classic() +
  theme(
    panel.grid   = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks   = element_line(color = "black"),
    plot.margin  = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) 


### --- COMBINE AND EXPORT FIGURE ---

png("FigS5.png", width = 1500, height = 500, res = 200)

figure <- ggarrange(
  plot, plot2, plot3,
  ncol = 3, nrow = 1,
  widths = c(1.05, 1, 1),
  common.legend = TRUE,   # shared legend
  legend = "right"        # on the total right side
)

print(figure)
dev.off()






#==========================================================#
###### 9. Encounter stats: detection radius-encounter ######
#==========================================================#


# Parameters
area <- 1e6        # total area
step_len <- 100    # movement step length

# Compute encounter statistics
encounter_stats <- function(n_trees, area, step_len) {
  density <- n_trees / area
  
  # Detection radius set to expected nearest-neighbour distance
  detect_rad <- 1 / (2 * sqrt(density))
  
  # 1D path covered within detection radius
  covered <- 2 * detect_rad * step_len
  
  # Expected number of trees encountered per random movement
  mean_encountered <- covered * density
  
  # Fraction of all trees encountered in one step
  frac_encountered <- mean_encountered / n_trees
  
  data.frame(
    n_trees         = n_trees,
    detect_rad      = detect_rad,
    mean_encountered = mean_encountered,
    frac_encountered = frac_encountered
  )
}

# Examples: low vs high density
res_low  <- encounter_stats(n_trees = 25,   area = area, step_len = step_len)
res_high <- encounter_stats(n_trees = 3200, area = area, step_len = step_len)

rbind(res_low, res_high)


