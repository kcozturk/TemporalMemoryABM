###### INITIAL SETUP ######

# Clear the environment
rm(list = ls())

# (Optional) Install required packages if not already installed
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("truncnorm")

# Load libraries
library(ggplot2)
library(ggpubr)
library(truncnorm) # necessary for figure 3

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###### LOAD & PREPARE DATA ######

# Define file name patterns for all six datasets
file_names  <- sprintf("output_decay%d.csv", 1:1152)         # Time to decay
file_names2 <- sprintf("output_prod%d.csv", 1:1152)          # Productivity
file_names3 <- sprintf("output_hetero%d.csv", 1:1152)       # Spatial heterogeneity

file_names4 <- sprintf("output_sensory_decay%d.csv", 1:8)
file_names5 <- sprintf("output_sensory_prod%d.csv", 1:8)
file_names6 <- sprintf("output_sensory_hetero%d.csv", 1:8)

# Load all CSVs into lists
data_list  <- lapply(file_names, read.csv)
data_list2 <- lapply(file_names2, read.csv)
data_list3 <- lapply(file_names3, read.csv)
data_list4 <- lapply(file_names4, read.csv)
data_list5 <- lapply(file_names5, read.csv)
data_list6 <- lapply(file_names6, read.csv)

# Define columns to extract
selected_columns <- c(1, 4, 7, 11, 18, 25, 26)

# Function to format data frames
prepare_df <- function(datalist) {
  df <- do.call(cbind, lapply(datalist, function(x) x[selected_columns, -1]))
  df <- data.frame(t(df))
  colnames(df) <- c("eaten", "inaccuracy_factor", "time_to_dis", "nTree", "memory_slots", "cent_std", "memory")
  return(df)
}

# Create structured data frames
df  <- prepare_df(data_list)
df2 <- prepare_df(data_list2)
df3 <- prepare_df(data_list3)
df4 <- prepare_df(data_list4)
df5 <- prepare_df(data_list5)
df6 <- prepare_df(data_list6)

# Clean up workspace
rm(data_list, data_list2, data_list3, data_list4, data_list5, data_list6,
   file_names, file_names2, file_names3, file_names4, file_names5, file_names6)


###### DESCRIPTIVE STATISTICS ######

# Calculate mean eaten fruits for cognitive and sensory foragers
mean(c(df$eaten, df2$eaten, df3$eaten))
mean(c(df4$eaten, df5$eaten, df6$eaten))

# Calculate standard error of mean eaten
se_cognitive <- sd(c(df$eaten, df2$eaten, df3$eaten)) / sqrt(length(c(df$eaten, df2$eaten, df3$eaten)))
se_sensory   <- sd(c(df4$eaten, df5$eaten, df6$eaten)) / sqrt(length(c(df4$eaten, df5$eaten, df6$eaten)))


###--- COMPUTE FORAGING BENEFITS ---

# Compute average and SD of eaten fruits for sensory foragers
averages_df4 <- aggregate(df4$eaten, by = list(time_to_dis = df4$time_to_dis), 
                          FUN = function(x) c(mean = mean(x), sd = sd(x)))
averages_df5 <- aggregate(df5$eaten, by = list(nTree = df5$nTree), 
                          FUN = function(x) c(mean = mean(x), sd = sd(x)))
averages_df6 <- aggregate(df6$eaten, by = list(cent_std = df6$cent_std), 
                          FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Merge aggregated result with df based on time_to_dis/nTree/cent_std
merged_df <- merge(df, averages_df4, by = "time_to_dis")
merged_df2 <- merge(df2, averages_df5, by = "nTree")
merged_df3 <- merge(df3, averages_df6, by = "cent_std")

# Determine the net foraging benefit 
merged_df$net_ben <- (merged_df$eaten - merged_df$x[, 1])
merged_df2$net_ben <- (merged_df2$eaten - merged_df2$x[, 1])
merged_df3$net_ben <- (merged_df3$eaten - merged_df3$x[, 1])

# Determine the relative foraging benefit 
merged_df$rel_ben <- (merged_df$eaten - merged_df$x[, 1]) / merged_df$x[, 1]
merged_df2$rel_ben <- (merged_df2$eaten - merged_df2$x[, 1]) / merged_df2$x[, 1]
merged_df3$rel_ben <- (merged_df3$eaten - merged_df3$x[, 1]) / merged_df3$x[, 1]


# Compute net and relative foraging benefit for cognitive foragers
averages_df <- aggregate(merged_df$net_ben, 
                         by = list("time_to_dis" = merged_df$time_to_dis, 
                                   "memory_slots" = merged_df$memory_slots, 
                                   "inaccuracy_factor" = merged_df$inaccuracy_factor), 
                         FUN = mean)
colnames(averages_df)[4] <- "net_ben"

averages_df$rel_ben <- aggregate(merged_df$rel_ben, 
                                 by = list("time_to_dis" = merged_df$time_to_dis, 
                                           "memory_slots" = merged_df$memory_slots, 
                                           "inaccuracy_factor" = merged_df$inaccuracy_factor), 
                                 FUN = mean)[, 4]

averages_df2 <- aggregate(merged_df2$net_ben, 
                          by = list("nTree" = merged_df2$nTree, 
                                    "memory_slots" = merged_df2$memory_slots, 
                                    "inaccuracy_factor" = merged_df2$inaccuracy_factor), 
                          FUN = mean)
colnames(averages_df2)[4] <- "net_ben"
averages_df2$rel_ben <- aggregate(merged_df2$rel_ben, 
                                  by = list("nTree" = merged_df2$nTree, 
                                            "memory_slots" = merged_df2$memory_slots, 
                                            "inaccuracy_factor" = merged_df2$inaccuracy_factor), 
                                  FUN = mean)[, 4]

averages_df3 <- aggregate(merged_df3$net_ben, 
                          by = list("cent_std" = merged_df3$cent_std, 
                                    "memory_slots" = merged_df3$memory_slots, 
                                    "inaccuracy_factor" = merged_df3$inaccuracy_factor), 
                          FUN = mean)
colnames(averages_df3)[4] <- "net_ben"
averages_df3$rel_ben <- aggregate(merged_df3$rel_ben, 
                                  by = list("cent_std" = merged_df3$cent_std, 
                                            "memory_slots" = merged_df3$memory_slots, 
                                            "inaccuracy_factor" = merged_df3$inaccuracy_factor), 
                                  FUN = mean)[, 4]



###--- EXTRACT MAX. FORAGING BENEFITS ---

# Extract max. relative foraging efficiency in the 3 datasets
averages_df[which.max(averages_df$rel_ben), ]
averages_df2[which.max(averages_df2$rel_ben), ]
averages_df3[which.max(averages_df3$rel_ben), ]

#Extract max. net foraging efficiency in the 3 df's
averages_df[which.max(averages_df$net_ben), ]
averages_df2[which.max(averages_df2$net_ben), ]
averages_df3[which.max(averages_df3$net_ben), ]










###### FIGURE 1: SIMULATED OUTPUT ######

# Load custom functions
source("Functions.R")

# Set environment boundaries
env_xmin <- 0
env_xmax <- 1000
env_ymin <- 0 
env_ymax <- 1000

# Output figure as PNG
png(filename = "Fig1.png", width = 1100, height = 1200, units = "px", res = 160)

par(mfrow = c(4, 3), oma = c(0, 0, 0, 0), mar = c(2, 4.3, 2.2, 1), cex.lab = 1.3)
set.seed(1)


### --- A. Productivity ---
# Figures with spatial distribution of trees for different Patch Spread
homogen           <- 0
num_cent          <- 10
nTree_values      <- 320
cent_std_values   <- c(20, 40, 80)

for (i in 1:3) {
  cent_std <- cent_std_values[i]
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, homogenous = homogen, n_centers = num_cent, 
                              std = cent_std
  )
  
  plot(tree_coords, xlim=c(0, 1000), ylim=c(0, 1000), xaxt='n', yaxt = 'n', 
       ylab="", xlab= "", main = paste("Patch Spread =", cent_std, "alu"))
  
  if (i == 1) {
    mtext("A", side = 3, line = 1, adj = -0.3, font = 2)
  }
}


### --- B. Productivity ---
# Figures with spatial distribution of trees for different productivity

homogen       <- 1
nTree_values  <- c(160, 320, 640)  

for (i in 1:3) {
  nTree <- nTree_values[i]
  tree_coords <- generate_env(ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
                              ymin = env_ymin, ymax = env_ymax, homogenous = homogen, n_centers = num_cent, 
                              std = cent_std
  )
  
  plot(tree_coords, xlim=c(0, 1000), ylim=c(0, 1000), xaxt='n', yaxt = 'n', 
       ylab="", xlab= "", main = paste("Productivity =", nTree_values[i], "Trees"))
  
  if (i == 1) {
    mtext("B", side = 3, line = 1, adj = -0.3, font = 2)
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
nTree_values        <- c(160, 320, 640)
initialize_env_agent()



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
    ripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 6]))
  )
  plotdata <- plotdata[601:4250, ]
  
  plot(ripefruits ~ time, data = plotdata, type = "l",
       ylab = "Ripe fruit", xlab = "Time",
       main = paste("Productivity =", nTree, "tu"),
       ylim = c(0, 20))
  
  if (i == 1) {
    mtext("C", side = 3, line = 1, adj = -0.3, font = 2)
  }
}


### --- D. Decay Period ---
# Figures with ripe fruit over time for different decay periods
memory_slots      <- 0
inaccuracy_factor <- 0
homogen           <- 1
init_time         <- 425
nTree             <- 320
time_to_dis_vals  <- c(2.0, 4.0, 8.0)
initialize_env_agent()

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
    ripefruits = sapply(3:4252, function(j) sum(Trees_hist_plot[[j]][, 6]))
  )
  plotdata <- plotdata[601:4250, ]
  
  plot(ripefruits ~ time, data = plotdata, type = "l",
       ylab = "Ripe fruit", xlab = "Time",
       main = paste("Decay Period =", time_to_dis, "tu"),
       ylim = c(0, 20))
  
  if (i == 1) {
    mtext("D", side = 3, line = 1, adj = -0.3, font = 2)
  }
}

dev.off()


###### FIGURE 3B: Time Estimation ######
# 
# use step-length of 100 alu. 
# movement speed is 500 alu tu-1
# maturation period is 30 tu. 


maturationtime <- 30
passed_time <- 100 / 500 # time that passes with 100 alu
iterations <- (maturationtime / passed_time) + 1
actual_passed_time <- seq(from = 0, by = passed_time, length.out = iterations)

set.seed(10)

######## one run for each inaccuracy value
#seq(from = 0.0, by = 0.09, length.out = 12)
inac_fact_vals <- c(0.09, 0.54, 0.99)

### inac 0.1
estimated_time <- rep(NA, iterations)
estimated_time[1] <- 0 
cum_estimated_time <- rep(NA, iterations) #cumulative
cum_estimated_time[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time <- cumsum(estimated_time)
  estimated_time[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time[i - 1], b = Inf, mean = passed_time, sd = inac_fact_vals[1])
}

### inac 0.6
estimated_time2 <- rep(NA, iterations)
estimated_time2[1] <- 0 
cum_estimated_time2 <- rep(NA, iterations) #cumulative
cum_estimated_time2[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time2 <- cumsum(estimated_time2)
  estimated_time2[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time2[i - 1], b = Inf, mean = passed_time, sd = inac_fact_vals[2])
}


### inac 1.2
estimated_time3 <- rep(NA, iterations)
estimated_time3[1] <- 0 
cum_estimated_time3 <- rep(NA, iterations) #cumulative
cum_estimated_time3[1] <- 0 

for(i in 2 : iterations) {
  cum_estimated_time3 <- cumsum(estimated_time3)
  estimated_time3[i] <- new_rtruncnorm(1, a = -1 * cum_estimated_time3[i - 1], b = Inf, mean = passed_time, sd = inac_fact_vals[3])
}


# Combine into one data frame for ggplot
df <- data.frame(
    actual_time = rep(actual_passed_time, 3),
    estimated_time = c(cumsum(estimated_time), cumsum(estimated_time2), cumsum(estimated_time3)),
    inac_fact = factor(c(rep(inac_fact_vals[1], 151), 
                         rep(inac_fact_vals[2], 151), 
                         rep(inac_fact_vals[3], 151)))
  )

# Point symbols for each inac_fact level (pch in ggplot2)
shape_values <- c(1, 2, 3)  

# ---- Plot ----
plotb1 <- ggplot(df, aes(x = actual_time, y = estimated_time, shape = inac_fact)) +
  # Reference line (plotted first, behind)
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", linewidth = 1) +
  
  # Main data
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.5) +
  
  # Shapes and labels
  scale_shape_manual(values = shape_values) +
  labs(
    x = "Actual Passed Time",
    y = "Estimated Time",
    shape = "Timing Inaccuracy"
  ) +
  
  # Axes start at 0
  scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 61), expand = c(0, 0)) +
  
  # Theme
  theme_classic(base_size = 14) +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    plot.margin = margin(t = 25, r = 5, b = 5, l = 5)
  )
 

####### multiple runs: FIGURE 3C: Time Estimation
# Parameters
inac_fact_vals <- c(0.09, 0.54, 0.99)
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
    geom_segment(aes(x = 0, y = 0, xend = 30, yend = 30),
                 color = "red", linetype = "dashed", inherit.aes = FALSE) +
    labs(
      x = "Actual Passed Time",
      y = "Estimated Time",
      title = paste0("Timing Inaccuracy: ", inac)
    ) +
    coord_cartesian(xlim = c(0, 30), ylim = c(0, 70)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_list[[i]] <- p
}

# Assigning plots to variable
plotc1 <- plot_list[[1]]
plotc2 <- plot_list[[2]]
plotc3 <- plot_list[[3]]

# Optionally show them
print(plotc1)
print(plotc2)
print(plotc3)


### --- COMBINE AND EXPORT FIGURE ---

png("Fig3.png", width = 1500, height = 1000, res = 200)

# Arrange p1 (top full row) and p2/p3/p3 (bottom split)
figure <- ggarrange(
  plotb1,
  ggarrange(plotc1, plotc2, plotc3, ncol = 3, labels = c("B", "C", "D")),  # bottom row
  nrow = 2,
  heights = c(1, 1),  # equal heights, adjust if needed
  labels = "A"        # label for top plot
)

print(figure)

dev.off()



###### FIGURE 5: Foraging Efficiency Sensory Forager ######

### --- Plot A: Efficiency vs Decay Period ---

plot <- ggplot(averages_df4, aes(x = time_to_dis, y = x[,1])) +
  geom_ribbon(aes(ymin = x[,1] - x[,2], ymax = x[,1] + x[,2]),
              fill = "darkgrey", alpha = 0.3) +
  geom_line() +
  geom_point(color = "black", size = 2.5, stroke = 0.3) +
  ylim(0, 250) +
  labs(x = "Decay Period", y = "Foraging Efficiency", tag = "A") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )

### --- Plot B: Efficiency vs Productivity ---
plot2 <- ggplot(averages_df5, aes(x = nTree, y = x[,1])) +
  geom_ribbon(aes(ymin = x[,1] - x[,2], ymax = x[,1] + x[,2]),
              fill = "darkgrey", alpha = 0.3) +
  geom_line() +
  geom_point(color = "black", size = 2.5, stroke = 0.3) +
  ylim(0, 250) +
  labs(x = "Productivity", y = "Foraging Efficiency", tag = "B") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )

### --- Plot C: Efficiency vs Patch Spread ---
plot3 <- ggplot(averages_df6, aes(x = cent_std, y = x[,1])) +
  geom_ribbon(aes(ymin = x[,1] - x[,2], ymax = x[,1] + x[,2]),
              fill = "darkgrey", alpha = 0.3) +
  geom_line() +
  geom_point(color = "black", size = 2.5, stroke = 0.3) +
  ylim(0, 100) +
  labs(x = "Patch Spread", y = "Foraging Efficiency", tag = "C") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )

### --- COMBINE AND EXPORT FIGURE ---

png("Fig5.png", width = 1500, height = 500, res = 200)

# Arrange the three plots into a single row
figure <- ggarrange(plot, plot2, plot3,
                    ncol = 3, nrow = 1,
                    widths = c(1.05, 1, 1))

print(figure)

dev.off()






###### FIGURE 6: Net Foraging Benefit Cognitive Forager ######

### --- Plot A: Net Benefit vs Decay Period ---

# Aggregate statistics for Decay Period
avg_df <- aggregate(merged_df$net_ben, 
                    by = list(time_to_dis = merged_df$time_to_dis, 
                              memory_slots = merged_df$memory_slots, 
                              inaccuracy_factor = merged_df$inaccuracy_factor), 
                    FUN = mean)
colnames(avg_df)[4] <- "net_ben"

# Add standard deviation and count
avg_df$sd <- aggregate(merged_df$net_ben, 
                       by = list(time_to_dis = merged_df$time_to_dis, 
                                 memory_slots = merged_df$memory_slots, 
                                 inaccuracy_factor = merged_df$inaccuracy_factor), 
                       FUN = sd)[,4]

avg_df$length <- aggregate(merged_df$net_ben, 
                           by = list(time_to_dis = merged_df$time_to_dis, 
                                     memory_slots = merged_df$memory_slots, 
                                     inaccuracy_factor = merged_df$inaccuracy_factor), 
                           FUN = length)[,4]

# Identify maximum net benefit per decay period
high_net_ben <- data.frame(
  time_to_dis = unique(avg_df$time_to_dis),
  net_ben = tapply(avg_df$net_ben, avg_df$time_to_dis, FUN = max)
)

plot <- ggplot(high_net_ben, aes(x = time_to_dis, y = net_ben)) +
  geom_line() +
  geom_point(color = "black", size = 3, stroke = 0.3) +
  ylim(0, 60) +
  labs(x = "Decay Period", y = "Net Foraging Benefit", tag = "A") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )


### --- Plot B: Net Benefit vs Productivity ---

# Aggregate statistics for Productivity
avg_df2 <- aggregate(merged_df2$net_ben, 
                     by = list(nTree = merged_df2$nTree, 
                               memory_slots = merged_df2$memory_slots, 
                               inaccuracy_factor = merged_df2$inaccuracy_factor), 
                     FUN = mean)
colnames(avg_df2)[4] <- "net_ben"

avg_df2$sd <- aggregate(merged_df2$net_ben, 
                        by = list(nTree = merged_df2$nTree, 
                                  memory_slots = merged_df2$memory_slots, 
                                  inaccuracy_factor = merged_df2$inaccuracy_factor), 
                        FUN = sd)[,4]

avg_df2$length <- aggregate(merged_df2$net_ben, 
                            by = list(nTree = merged_df2$nTree, 
                                      memory_slots = merged_df2$memory_slots, 
                                      inaccuracy_factor = merged_df2$inaccuracy_factor), 
                            FUN = length)[,4]

# Identify maximum net benefit per productivity level
high_net_ben2 <- data.frame(
  nTree = unique(avg_df2$nTree),
  net_ben = tapply(avg_df2$net_ben, avg_df2$nTree, FUN = max)
)

plot2 <- ggplot(high_net_ben2, aes(x = nTree, y = net_ben)) +
  geom_line() +
  geom_point(color = "black", size = 3, stroke = 0.3) +
  ylim(0, 60) +
  labs(x = "Productivity", tag = "B") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )


### --- Plot B: Net Benefit vs Patch Spread ---

# Aggregate statistics for Patch Spread
avg_df3 <- aggregate(merged_df3$net_ben, 
                     by = list(cent_std = merged_df3$cent_std, 
                               memory_slots = merged_df3$memory_slots, 
                               inaccuracy_factor = merged_df3$inaccuracy_factor), 
                     FUN = mean)
colnames(avg_df3)[4] <- "net_ben"

avg_df3$sd <- aggregate(merged_df3$net_ben, 
                        by = list(cent_std = merged_df3$cent_std, 
                                  memory_slots = merged_df3$memory_slots, 
                                  inaccuracy_factor = merged_df3$inaccuracy_factor), 
                        FUN = sd)[,4]

avg_df3$length <- aggregate(merged_df3$net_ben, 
                            by = list(cent_std = merged_df3$cent_std, 
                                      memory_slots = merged_df3$memory_slots, 
                                      inaccuracy_factor = merged_df3$inaccuracy_factor), 
                            FUN = length)[,4]

# Identify maximum net benefit per patch spread level
high_net_ben3 <- data.frame(
  cent_std = unique(avg_df3$cent_std),
  net_ben = tapply(avg_df3$net_ben, avg_df3$cent_std, FUN = max)
)

plot3 <- ggplot(high_net_ben3, aes(x = cent_std, y = net_ben)) +
  geom_line() +
  geom_point(color = "black", size = 3, stroke = 0.3) +
  ylim(0, 90) +
  labs(x = "Patch Spread", tag = "C") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(25, 1, 1, 5),
    plot.tag.position = c(0, 1.05)
  )


### --- COMBINE AND EXPORT FIGURE ---

png("Fig6.png", width = 1500, height = 500, res = 200)

figure <- ggarrange(plot, plot2, plot3,
                    ncol = 3, nrow = 1,
                    widths = c(1.05, 1, 1))

print(figure)

dev.off()





###### FIGURE 7: Net Foraging Benefit Decay Period ######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df$net_ben)
z_max <- max(averages_df$net_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(-5, 55, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of time_to_dis
time_values <- unique(averages_df$time_to_dis)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df[averages_df$time_to_dis == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = net_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Decay Period =", t, "tu"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Net\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = net_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Net Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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

# Define three specificinaccuracy values
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = net_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Net Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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


### --- COMBINE AND EXPORT FIGURE ---

png("Fig7.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3, 
                    widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)
dev.off()






###### FIGURE 8: Net Foraging Benefit Productivity######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df2$net_ben)
z_max <- max(averages_df2$net_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(-5, 55, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of nTree
nTree_values <- unique(averages_df2$nTree)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(nTree_values)){
  t <- nTree_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df2[averages_df2$nTree == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = net_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Productivity =", t, "trees"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Net\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = net_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Net Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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

# Define three specificinaccuracy values
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = net_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Net Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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


### --- COMBINE AND EXPORT FIGURE ---

png("Fig8.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3, widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)


# Close the PNG device to save the plot
dev.off()



###### FIGURE 9: Net Foraging Benefit Patch Spread ######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df3$net_ben)
z_max <- max(averages_df3$net_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(0, 90, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of cent_std (homogeneity)
cent_std_values <- unique(averages_df3$cent_std)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(cent_std_values)){
  t <- cent_std_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df3[averages_df3$cent_std == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = net_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Patch Spread =", t, "alu"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Net\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through cent_std values and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = net_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Net Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through cent_stdvalues and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = net_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Net Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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


### --- COMBINE AND EXPORT FIGURE ---

png("Fig9.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3,  widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)

# Close the PNG device to save the plot
dev.off()


#=================================#
###### SUPPLEMENTARY FIGURES ######
#=================================#

###### FIGURE S2: Relative Foraging Benefit Decay Period ######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df$rel_ben)
z_max <- max(averages_df$rel_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(-5, 20, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of time_to_dis
time_values <- unique(averages_df$time_to_dis)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(time_values)){
  t <- time_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df[averages_df$time_to_dis == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Decay Period =", t, "tu"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0.7, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0.7, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Relative\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Rel. Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through time_to_dis values and create separate plots
for (i in seq_along(time_values)) {
  t <- time_values[i]
  
  # Filter for current time_to_dis and selected memory slots
  filtered_df <- subset(averages_df, 
                        time_to_dis == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Rel. Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}





# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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

### --- COMBINE AND EXPORT FIGURE ---

png("FigS2.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3, widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)

# Close the PNG device to save the plot
dev.off()








###### FIGURE S3: Relative Foraging Benefit Productivity ######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df2$rel_ben)
z_max <- max(averages_df2$rel_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(-5, 5, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of nTree
nTree_values <- unique(averages_df2$nTree)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(nTree_values)){
  t <- nTree_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df2[averages_df2$nTree == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Productivity =", t, "trees"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0.8, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0.8, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Relative\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Rel. Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through nTree values and create separate plots
for (i in seq_along(nTree_values)) {
  t <- nTree_values[i]
  
  # Filter for current nTree and selected memory slots
  filtered_df <- subset(averages_df2, 
                        nTree == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Rel. Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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


### --- COMBINE AND EXPORT FIGURE ---

png("FigS3.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3, widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)

# Close the PNG device to save the plot
dev.off()



###### FIGURE S4: Relative Foraging Benefit Patch Spread ######

### --- Plot A: Contour plots ---

# Compute global range of z values for all plots
z_min <- min(averages_df3$rel_ben)
z_max <- max(averages_df3$rel_ben)

# Define contour levels and color breaks (same for all plots)
levels <- seq(0, 3, length.out = 11)

# Create matching grayscale palette
n_intervals <- length(levels) - 1
gray_palette <- colorRampPalette(c("gray85", "black"))(n_intervals)

# Define the values of cent_std (homogeneity)
cent_std_values <- unique(averages_df3$cent_std)

# Create empty list to store the plots
plot_list <- list()

# Loop through each time value to generate contour plots for df1
for (i in seq_along(cent_std_values)){
  t <- cent_std_values[i]
  
  # Step 1: Filter the dataframe for the current time value
  filtered_df <- averages_df3[averages_df3$cent_std == t, ]
  
  # Plot with border at edge
  p <- ggplot(filtered_df, aes(
    x = inaccuracy_factor,
    y = memory_slots,
    z = rel_ben)) +
    geom_contour_filled(breaks = levels) +
    scale_fill_manual(values = gray_palette, drop = FALSE) +
    geom_contour(breaks = 0, color = "red", size = 0.6) +  # Add line when z = 0
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
    labs(x = "Inaccuracy Factor", y = "Cognitive Storage") +
    labs(title = paste("Patch Spread =", t, "alu"), tag = "A") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.tag.position = c(0.07, 0.98),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list[[i]] <- p
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
            aes(xmin = 0.95, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
            color = NA) +
  # Outer border
  annotate("rect",
           xmin = 0.95, xmax = 1,
           ymin = min(levels), ymax = max(levels),
           color = "black", fill = NA, size = 0.6) +
  # Labels on the right
  geom_text(data = text_df,
            aes(x = 1.25, y = y, label = label),
            hjust = 0, size = 3.5) +
  ggtitle("Relative\nForaging\nBenefit") +
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
plot_list2 <- list()

# Loop through cent_std values and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & memory_slots %in% selected_memory_slots)
  
  filtered_df$memory_slots <- as.factor(filtered_df$memory_slots)
  
  p <- ggplot(data = filtered_df, 
              aes(x = inaccuracy_factor, 
                  y = rel_ben, group = memory_slots)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Inaccuracy Factor", y = "Rel. Foraging Benefit") +
    labs(tag = "B") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text() else element_blank())
  
  # Add the plot to the list
  plot_list2[[i]] <- p
  
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
    shape = "Cognitive\nStorage",  # Title for the legend
    fill = "Cognitive\nStorage") +
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
selected_inaccuracy <- c(0.0, 0.27, 0.54, 0.81, 0.99)

# Create empty list to store the plots
plot_list3 <- list()

# Loop through cent_stdvalues and create separate plots
for (i in seq_along(cent_std_values)) {
  t <- cent_std_values[i]
  
  # Filter for current cent_std and selected memory slots
  filtered_df <- subset(averages_df3, 
                        cent_std == t & inaccuracy_factor %in% selected_inaccuracy)
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p <- ggplot(data = filtered_df, 
              aes(x = memory_slots, 
                  y = rel_ben, 
                  group = inaccuracy_factor)) +
    ylim(floor(z_min), ceiling((z_max))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
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
    labs(x = "Cognitive Storage", y = "Rel. Foraging Benefit")  +
    labs(tag = "C") +
    theme(plot.tag.position = c(0.07, 1.06),
          plot.tag = if (i == 1) element_text()  else element_blank())
  
  # Add the plot to the list
  plot_list3[[i]] <- p
  
}


# Dummy data for legend
legend_df <- data.frame(
  inaccuracy_factor = factor(c("0.0", "0.27", "0.54", "0.81", "0.99"), 
                             levels = c("0.0", "0.27", "0.54", "0.81", "0.99")),
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
    shape = "Timing\nInaccuracy",  # Title for the shape legend
    fill = "Timing\nInaccuracy") +
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


### --- COMBINE AND EXPORT FIGURE ---

png("FigS4.png", width = 4000, height = 1500, res = 200)

par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  # Set margins for the plots

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    legend_plot,
                    plot_list2[[1]], plot_list2[[2]], plot_list2[[3]], plot_list2[[4]],
                    plot_list2[[5]], plot_list2[[6]], plot_list2[[7]], plot_list2[[8]],
                    legend_plot2,
                    plot_list3[[1]], plot_list3[[2]], plot_list3[[3]], plot_list3[[4]],
                    plot_list3[[5]], plot_list3[[6]], plot_list3[[7]], plot_list3[[8]],
                    legend_plot3, ncol = 9, nrow = 3, widths = c(1.05, rep(1, 7), 0.55)) 

print(figure)

# Close the PNG device to save the plot
dev.off()


