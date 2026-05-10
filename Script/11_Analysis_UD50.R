
#====================#
###### 10. UD50 ######
#====================#

# Load necessary libraries
library(adehabitatHR)
library(adehabitatLT)
library(dplyr)
library(data.table)
library(future.apply)

# Set working directory to script location (if running in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#==============================#
###### Functions for UD50 ######
#==============================#

# Create functions to help with creating UD50

###### Function: clean_track() ######
# Removes moment that where agent eats 
# and remove time from eating (to decrease time spent due to eating)
clean_track <- function(track) {
  
  # Identify rows where the animal is eating
  row_eaten <- track$branch == "eat"
  
  # Time differences between row and previous row
  dt <- c(0, diff(track$time))
  
  # (cumulative) duration spent eating at each step:
  #    If row is "eat", count its dt as eating time,
  #    otherwise 0
  eat_dt <- ifelse(row_eaten, dt, 0)
  eat_cum <- cumsum(eat_dt)
  
  # New time that skips eating periods
  track$time_no_eat <- track$time - eat_cum
  
  # Keep only non‑eating rows
  keep <- !row_eaten
  cleaned <- track[keep, , drop = FALSE]
  
  rownames(cleaned) <- NULL
  return(cleaned)
}

#============================================#
###### Function: compute_UD50_for_track ######
#============================================#

# Function to compute UD50% area for a single track data.frame
compute_UD50_for_track <- function(track_df) {
  # Clean and add time
  clean_df <- clean_track(track_df)
  
  # Convert to ltraj
  track_ltraj <- as.ltraj(
    xy   = clean_df[, c("Xcoord", "Ycoord")],
    date = as.POSIXct(clean_df$time * 24 * 60 * 60,
                      origin = "2000-01-01",
                      tz     = "UTC"),
    id    = "1",
    typeII = TRUE
  )
  
  # BRB UD
  ud <- BRB(track_ltraj, D = 0, Tmax = 1, Lmin = 0, hmin = 10, type = "UD")
  
  # 50% utilization distribution
  UD50 <- getverticeshr(ud, percent = 50)
  
  return(UD50)
}




#============================================#
###### Function: loading_track ######
#============================================#

# Loading using data.table::fread
load_track_default <- function(prefix, z_index, sim_id) {
  file_name <- sprintf("%s%04d_sim%04d.csv", prefix, z_index, sim_id)
  data.table::fread(file_name)
}


#=======================================================#
###### Determine UD50 for environmental parameters ######
#=======================================================#

## Parameter grids
n_inacc_memslot           <- 12
inaccuracy_factor_values  <- seq(0.01, by = 0.09, length.out = n_inacc_memslot)
memory_slots_values       <- seq(1,    by = 2,  length.out = n_inacc_memslot)
time_to_dis_values        <- 0.25 * 2^(0:7)
nTree_values              <- 25 * 2^(0:7)
cent_std_values           <- 2.5 * 2^(0:7)

# All used parameter combinations in simulations
# Row number corresponds to z in file names
param_grid <- expand.grid(
  inaccuracy_factor = inaccuracy_factor_values,
  memory_slots      = memory_slots_values,
  time_to_dis       = time_to_dis_values
)

total_combinations <- nrow(param_grid)

# Subset of values to get UD from
time_to_dis_UD      <- time_to_dis_values[c(1, 5, 8)]
inaccuracy_factor_UD <- inaccuracy_factor_values[c(1, 7, 12)]
memory_slots_UD      <- memory_slots_values[c(1, 5, 12)]

# All parameter combinations for which we compute UD50 for time_to_dis
time_to_dis_UD_combination <- expand.grid(
  inaccuracy_factor = inaccuracy_factor_UD,
  memory_slots      = memory_slots_UD,
  time_to_dis       = time_to_dis_UD
)

# Add explicit index to param_grid = z in file name
param_grid$z_index <- seq_len(nrow(param_grid))

# Add z-values also to UD_combinations
time_to_dis_UD_comb <- merge(time_to_dis_UD_combination, 
                             param_grid, 
                             by = c("inaccuracy_factor", 
                                    "memory_slots", 
                                    "time_to_dis"), all.x = TRUE)


time_to_dis_UD_comb <- time_to_dis_UD_comb[order(time_to_dis_UD_comb$z_index), ]

# Add nTree and cent_std values to combinations
UD_comb <- time_to_dis_UD_comb

UD_comb$cent_std <- UD_comb$time_to_dis*10
UD_comb$nTree <- UD_comb$time_to_dis*100


#=====================================================#
###### Function to loop parallel to compute UD50 ######
#=====================================================#
compute_UD50_grid <- function(comb_df,
                              by_cols,
                              nsim      = 300,
                              prefix,
                              load_track_fun = load_track_default) {
  
  row_indices <- seq_len(nrow(comb_df))
  
  res_list <- future_lapply(
    X = row_indices,
    FUN = function(i) {
      row_df <- comb_df[i, , drop = FALSE]
      z      <- row_df$z_index
      
      UD50_vec <- numeric(nsim)
      
      for (sim_id in seq_len(nsim)) {
        track_obj <- load_track_fun(prefix = prefix, z_index = z, sim_id = sim_id)
        UD50_val  <- compute_UD50_for_track(track_obj)
        UD50_vec[sim_id] <- UD50_val$area
      }
      
      # One row per simulation with raw UD50 values
      out <- row_df[rep(1, nsim), by_cols, drop = FALSE]
      out$z_index   <- z
      out$sim_id    <- seq_len(nsim)
      out$UD50_area <- UD50_vec
      
      out
    },
    future.seed = NULL
  )
  
  dplyr::bind_rows(res_list)
}


#========================#
###### Compute UD50 ######
#========================#


# Parallel plan, set number of cores
plan(multisession, workers = 5)

# Decay (time_to_dis)
UD50_results_df_decay <- compute_UD50_grid(
  comb_df = UD_comb,
  by_cols = c("inaccuracy_factor", "memory_slots", "time_to_dis"),
  nsim    = 300,
  prefix  = "Mov_outputs/mov_decay_z"
)

# Production (nTree)
UD50_results_df_prod <- compute_UD50_grid(
  comb_df = UD_comb,
  by_cols = c("inaccuracy_factor", "memory_slots", "nTree"),
  nsim    = 300,
  prefix  = "Mov_outputs/mov_prod_z"
)

# Heterogeneity (cent_std)
UD50_results_df_het <- compute_UD50_grid(
  comb_df = UD_comb,
  by_cols = c("inaccuracy_factor", "memory_slots", "cent_std"),
  nsim    = 300,
  prefix  = "Mov_outputs/mov_hetero_z"
)




#========================#
######   FIG. S5: UD  ######
#========================#

# determine mean 
avg_UD50_decay <- aggregate(
  UD50_area ~ time_to_dis + memory_slots + inaccuracy_factor,
  data = UD50_results_df_decay,
  FUN = mean, na.rm = TRUE
)

# determine mean 
avg_UD50_prod <- aggregate(
  UD50_area ~ nTree + memory_slots + inaccuracy_factor,
  data = UD50_results_df_prod,
  FUN = mean, na.rm = TRUE
)

# determine mean 
avg_UD50_het <- aggregate(
  UD50_area ~ cent_std + memory_slots + inaccuracy_factor,
  data = UD50_results_df_het,
  FUN = mean, na.rm = TRUE
)

maxval <- max(c(avg_UD50_prod$UD50_area, 
                avg_UD50_decay$UD50_area,
                avg_UD50_het$UD50_area)
              )



nTree_UD <- unique(avg_UD50_prod$nTree)
cent_std_UD <- unique(avg_UD50_het$cent_std)


# empty list for plot
p_decay <- vector("list", length(time_to_dis_UD))
p_prod <- vector("list", length(nTree_UD))
p_het <- vector("list", length(cent_std_UD))


# Plot for UD50 over ripe-fruit duration period values (i.e. time_to_dis)
for (i in seq_along(time_to_dis_UD)){
  t <- time_to_dis_UD[i]
  
  # Step 1: Filter the dataframe for the current time to dis value
  filtered_df <- avg_UD50_decay[avg_UD50_decay$time_to_dis == t, ]
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p_decay[[i]] <- ggplot(data = filtered_df, 
         aes(x = memory_slots, 
             y = UD50_area, 
             group = inaccuracy_factor))  +
    ylim(0, ceiling(maxval)) +
    geom_line() +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black"),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.tag.position = c(0.38, 1.14),
      plot.tag = if (i == 1) element_text()  else element_blank()
      )  +  
    labs(x = "", y = "UD50 area",
         title = paste(t), tag = "A)   Ripe-Fruit Duration:",
         shape = "Timing error",   # legend title
         fill  = "Timing error"    # legend title
         )
}


# Plot for UD50 over resource density values (i.e. nTree)
for (i in seq_along(nTree_UD)){
  t <- nTree_UD[i]
  
  # Step 1: Filter the dataframe for the current time to dis value
  filtered_df <- avg_UD50_prod[avg_UD50_prod$nTree == t, ]
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p_prod[[i]] <- ggplot(data = filtered_df, 
                         aes(x = memory_slots, 
                             y = UD50_area, 
                             group = inaccuracy_factor))  +
    ylim(0, ceiling(maxval)) +
    geom_line() +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black"),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.tag.position = c(0.36, 1.14),
      plot.tag = if (i == 1) element_text()  else element_blank()
      )  +  
    labs(x = "", y = "UD50 area",
         title = paste(t), tag =  "B)   Resource Density:",
         shape = "Timing error",   # legend title
         fill  = "Timing error"    # legend title
    )
}


# Plot for UD50 over patch spread values (i.e. cent_std)
for (i in seq_along(cent_std_UD)){
  t <- cent_std_UD[i]
  
  # Step 1: Filter the dataframe for the current time to dis value
  filtered_df <- avg_UD50_het[avg_UD50_het$cent_std == t, ]
  
  filtered_df$inaccuracy_factor <- as.factor(filtered_df$inaccuracy_factor)
  
  p_het[[i]] <- ggplot(data = filtered_df, 
                        aes(x = memory_slots, 
                            y = UD50_area, 
                            group = inaccuracy_factor))  +
    ylim(0, ceiling(maxval)) +
    geom_line() +
    geom_point(aes(shape = inaccuracy_factor, fill = inaccuracy_factor), 
               color = "black", size = 3, stroke = 0.3) +
    theme_classic() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),  # Remove gridlines
      plot.margin = margin(25, 1, 1, 5),  # Remove extra space
      axis.title.y = if (i == 1) element_text(size = 11) else element_blank(),
      axis.ticks = element_line(colour = "black"),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.tag.position = c(0.3, 1.14),
      plot.tag = if (i == 1) element_text()  else element_blank()
      )  +  
    labs(x = "Memory size", y = "UD50 area",
         title = paste(t), 
         tag = "C)   Patch Spread:",
         shape = "Timing error",   # legend title
         fill  = "Timing error"    # legend title
         )
}



### --- COMBINE FIGURE ---
par(mar = c(2, 2, 4, 0.5), cex.lab = 0.8, cex.main = 0.9)  

png("FigS5.png", width = 1600, height = 1450, res = 200)

figure_combined <- ggarrange(
  p_decay[[1]], p_decay[[2]], p_decay[[3]], 
  p_prod[[1]], p_prod[[2]], p_prod[[3]], 
  p_het[[1]], p_het[[2]], p_het[[3]], 
  ncol = 3, nrow = 3, widths = c(1.05, rep(1, 2)),
  common.legend = TRUE,   # <- collect a single legend
  legend = "right"        # <- place it on the right of the whole figure
  ) 

print(figure_combined)
dev.off()
