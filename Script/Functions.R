######Functions for Temporal ABM######
library(compiler) #library for compiling function


##### Function: random_movement() #####
# Simulates the random movement of an agent.
random_movement <- function(x_prev, y_prev, x_curr, y_curr, stepmean, stepsd, wrap_rho) {
  
  # Determine movement direction
  prev_angle <- anglefun(x_curr, y_curr, x_prev, y_prev)
  
  if (is.na(prev_angle) || prev_angle == 0) {
    # Pure random movement when angle is undefined
    angle_movement <- runif(1, 0, 360)
  } else {
    # Directional movement with wrapped normal noise
    noise <- as.vector(rwrappednormal(1, mu = circular(0), rho = wrap_rho, control.circular = list(units = "degrees")))
    angle_movement <- prev_angle + noise %% 360
  }
  
  # Compute movement distance
  movement_distance <- rnorm(1, mean = stepmean, sd = stepsd)
  
  # Compute displacement in x and y directions
  x_move <- sin(angle_movement * pi / 180) * movement_distance
  y_move <- cos(angle_movement * pi / 180) * movement_distance
  
  return(list(x_move = x_move, y_move = y_move, angle_movement = angle_movement, distance = movement_distance))
}

##### Function: move_2target() #####
# Moves an agent towards a target location with a maximum step length
move_2target <- function(agent_x, agent_y, target_x, target_y, max_step) {
  # Compute the distance to the target
  distance <- sqrt((target_x - agent_x)^2 + (target_y - agent_y)^2)
  
  # If the agent can reach the target in one step, move directly to the target
  if (distance <= max_step) {
    return(c(target_x, target_y))
  }
  
  # Compute the movement proportion
  ratio <- max_step / distance
  
  # Compute new position
  new_x <- agent_x + (target_x - agent_x) * ratio
  new_y <- agent_y + (target_y - agent_y) * ratio
  
  return(c(new_x, new_y))
}


###### Function: collision() ######
# This function detects collision point between agent random movement and boudaries.
# The function is used within the function refl_boundary()
collision <- function(prim_x, prim_y, angle_degrees, step, xmin, xmax, ymin, ymax) {
  
  angle_rad <- angle_degrees * pi / 180
  
  dx <- sin(angle_rad)
  dy <- cos(angle_rad)
  
  # Distances to each boundary
  t_xmax <- if (dx > 0) (xmax - prim_x)/dx else Inf
  t_xmin <- if (dx < 0) (xmin - prim_x)/dx else Inf
  t_ymax <- if (dy > 0) (ymax - prim_y)/dy else Inf
  t_ymin <- if (dy < 0) (ymin - prim_y)/dy else Inf
  
  t_candidates <- c(t_xmin, t_xmax, t_ymin, t_ymax)
  t_hit <- min(t_candidates[t_candidates > 0 & is.finite(t_candidates)])
  
  x_hit <- prim_x + t_hit * dx
  y_hit <- prim_y + t_hit * dy
  
  # Snap round-off errors onto the boundary
  eps <- 1e-8
  if (abs(x_hit - xmin) < eps) x_hit <- xmin
  if (abs(x_hit - xmax) < eps) x_hit <- xmax
  if (abs(y_hit - ymin) < eps) y_hit <- ymin
  if (abs(y_hit - ymax) < eps) y_hit <- ymax
  
  remaining <- step - t_hit
  
  return(list(
    col_x = x_hit,
    col_y = y_hit,
    remaining = remaining
  ))
}





###### Function: safe_angle_segment() ######
# This function detects safe angles that can be taken from collision point, 
# that do not go outside of boundaries.
# The function is used within the function refl_boundary() to then draw a random angle.
safe_angle_segment <- function(col_x, col_y, remaining, xmin, xmax, ymin, ymax) {
  
  # Compute max/min angles allowed to stay within rectangle
  # Angles in radians (0 = up, pi/2 = right, etc.)
  
  # Left/right limits
  if (remaining <= 0) stop(paste("Remaining step must be positive, remaining value is:", remaining, 
                                 "col_x:", col_x, "col_y:", col_y))
  
  # Exactly in the corner
  if (col_x == xmax && col_y == ymax) {
    angle_min <- 180
    angle_max <- 270
  } else if (col_x == xmin && col_y == ymin) {
    angle_min <- 0
    angle_max <- 90
  } else if (col_x == xmax && col_y == ymin) {
    angle_min <- 270
    angle_max <- 360
  } else if (col_x == xmin && col_y == ymax) {
    angle_min <- 90
    angle_max <- 180
  } else if (col_x == xmin || col_x == xmax) { # For circle with center of collision, determine intersection with the two adjacent walls
    intsct_top <- pcds::intersect.line.circle(c(xmin, ymax), c(xmax, ymax), c(col_x, col_y), remaining)
    a1 <- ifelse(
      !is.null(intsct_top),
      {
        intsct_top <- abs(matrix(intsct_top, ncol = 2))  
        idx <- which.min(intsct_top[, 1])
        anglefun(
          intsct_top[idx, 1],
          intsct_top[idx, 2],
          col_x, col_y
        )
      },
      360 # value/degree if there is no intersection
    )
    
    intsct_bot <- pcds::intersect.line.circle(c(xmin, ymin), c(xmax, ymin), c(col_x, col_y), remaining)
    a2 <- ifelse(
      !is.null(intsct_bot),
      {
        intsct_bot <- abs(matrix(intsct_bot, ncol = 2))
        idx <- which.min(intsct_bot[, 1])
        anglefun(
          intsct_bot[idx, 1],
          intsct_bot[idx, 2],
          col_x, col_y
        )
      },
      180 # value/degree if there is no intersection
    )
    
    if (col_x == xmin) {
      angle_min <- a1
      angle_max <- a2
    } else {
      angle_min <- a2
      angle_max <- a1
    }
    
  } else if (col_y == ymin || col_y == ymax) {
    intsct_left <- pcds::intersect.line.circle(c(xmin, ymin), c(xmin, ymax), c(col_x, col_y), remaining)
    a1 <- ifelse(
      !is.null(intsct_left),
      {
        intsct_left <- abs(matrix(intsct_left, ncol = 2))
        idx <- which.min(intsct_left[, 2])
        anglefun(
          intsct_left[idx, 1],
          intsct_left[idx, 2],
          col_x, col_y
        )
      },
      270 
    )
    
    intsct_right <- pcds::intersect.line.circle(c(xmax, ymin), c(xmax, ymax), c(col_x, col_y), remaining)
    a2 <- ifelse(
      !is.null(intsct_right),
      {
        intsct_right <- abs(matrix(intsct_right, ncol = 2))  
        idx <- which.min(intsct_right[, 2])
        anglefun(
          intsct_right[idx, 1],
          intsct_right[idx, 2],
          col_x, col_y
        )
      },
      90 
    )
    if (col_y == ymin) {
      angle_min <- a1
      angle_max <- a2
    } else {
      angle_min <- a2
      angle_max <- a1
    }
  } 
  return(c(angle_min, angle_max))
}


##### Function: refl_boundary() #####
# Implements a reflecting boundary for agent movement.
# If boundary is crossed it moves agent back by chosing a new angle from 
# collisionpoint with boundary
refl_boundary <- function(prim_x, prim_y, angle_deg, step, xmin, xmax, ymin, ymax) {
  angle_rad <- angle_deg * pi/180
  
  # Compute first segment
  x_new <- prim_x + step * sin(angle_rad)
  y_new <- prim_y + step * cos(angle_rad)
  
  # If still inside, return directly
  if (x_new >= xmin && x_new <= xmax && y_new >= ymin && y_new <= ymax) {
    return(list(
      final_x = x_new,
      final_y = y_new,
      seg1 = c(prim_x, prim_y, x_new, y_new),
      seg2 = NULL,
      new_angle_deg = NULL
    ))
  }
  
  # If on edge
  if (prim_x == xmin || prim_x == xmax || prim_y == ymin || prim_y == ymax) {
    col <- list(col_x = prim_x, col_y = prim_y, remaining = step)
  } else {
    # First collision
    col <- collision(prim_x, prim_y, angle_deg, step, xmin, xmax, ymin, ymax)
  }
  
  # Determine safe angle range
  safe_range <- safe_angle_segment(col$col_x, col$col_y, col$remaining, xmin, xmax, ymin, ymax)
  
  # ensure that max degree is the larger value 
  if (safe_range[2] < safe_range[1]) {
    safe_range[2] <- safe_range[2] + 360
  }
  
  # Pick new angle
  new_angle <- runif(1, safe_range[1], safe_range[2])  %% 360 # adjust for the + 360
  new_rad <- new_angle * pi / 180
  
  # Second segment end point
  x_end <- col$col_x + col$remaining * sin(new_rad) 
  y_end <- col$col_y + col$remaining * cos(new_rad)
  
  return(list(
    final_x = x_end,
    final_y = y_end,
    seg1 = c(prim_x, prim_y, col$col_x, col$col_y),
    seg2 = c(col$col_x, col$col_y, x_end, y_end),
    new_angle_deg = new_angle
  ))
}




##### Function: distance() #####
# Computes distance between two points.
distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}



##### Function: anglefun() #####
# Computes the angle (in degrees) between two locations.
# x1, and y1, are the locations that the agent ended at, and x2 and y2 where it came from
anglefun <- function(x1, y1, x2, y2) {
  temp_angle <- (atan2(x1 - x2, y1 - y2) * (180 / pi)) %% 360
  return(temp_angle)  # Convert negative angles to positive range
}


##### Function: linedistances() #####
# Computes the shortest distance between trees and the agent's movement path.
linedistances <- function(trees, line) {
  start <- line[1, ]
  end <- line[2, ]
  
  distances <- apply(trees, 1, function(tree) {
    num <- abs((end[2] - start[2]) * (tree[1] - start[1]) - (end[1] - start[1]) * (tree[2] - start[2]))
    denom <- sqrt((end[2] - start[2])^2 + (end[1] - start[1])^2)
    
    # Compute projection onto line segment
    proj <- sum((tree - start) * (end - start)) / sum((end - start)^2)
    on_segment <- (proj >= 0) && (proj <= 1)
    
    if (!on_segment) {
      # Point is outside the segment; compute distance to endpoints
      min(apply(rbind(start, end), 1, function(p) distance(tree[1], tree[2], p[1], p[2])))
    } else {
      # Point is on the line segment
      num / denom
    }
  })
  
  return(distances)
}



##### Function: ripen() #####
# Simulates fruit ripening over time.
ripen <- function(y_value, time_history, time, timestep, b_value = 100 / time_to_ripen) {
  y_value + b_value * (time - time_history[timestep])
}



##### Function: fruitripening() #####
# Updates fruit ripeness status based on elapsed time.
fruitripening <- function(food, time_history, time, timestep,
                          fruit_dim_creation = 9, fruit_dim_ripeness = 10, 
                          ripe_prim = edibilityPrim, fruit_col_prim = 6, 
                          fruit_unripe = 7, total_fruits = 8) {
  
  # Update ripeness for fruits in the process of ripening
  ripe_filter <- food[, fruit_dim_creation] == 1 & food[, fruit_dim_ripeness] < ripe_prim
  food[ripe_filter, fruit_dim_ripeness] <- ripen(food[ripe_filter, fruit_dim_ripeness], time_history, time, timestep)
  
  # Determine which fruits have ripened
  ripe_rows <- food[, fruit_dim_ripeness] >= ripe_prim & food[, fruit_unripe] > 0
  
  # Update fruit columns
  food[ripe_rows, fruit_col_prim] <- food[ripe_rows, fruit_unripe] 
  food[ripe_rows, total_fruits] <- food[ripe_rows, total_fruits] + food[ripe_rows, fruit_col_prim]  
  food[ripe_rows, fruit_unripe] <- 0  
  
  return(food)
}



##### Function: decay() #####
# Simulates fruit decay over time.
decay <- function(y_value, time_history, time, timestep, decaysteps = time_to_dis) {
  y_value + (-100 / decaysteps) * (time - time_history[timestep])
}



##### Function: fruitdecay() #####
# Updates decay status of fruits.
fruitdecay <- function(food, time_history, time, timestep,
                       fruit_dim_creation = 9, fruit_dim_ripeness = 10, 
                       fruit_dim_decay = 11, decay_str = decay_start, 
                       fruit_col_prim = 6) {
  
  # Apply decay process
  ripe_filter <- food[, fruit_col_prim] > 0
  food[ripe_filter, fruit_dim_decay] <- decay(food[ripe_filter, fruit_dim_decay], time_history, time, timestep)
  
  # Identify decayed fruits
  decay_rows <- food[, fruit_dim_decay] <= 0
  
  # Reset decayed fruit properties
  food[decay_rows, fruit_dim_creation] <- 0
  food[decay_rows, fruit_dim_ripeness] <- 0
  food[decay_rows, fruit_col_prim] <- 0
  food[decay_rows, fruit_dim_decay] <- 100  # Reset decay counter
  
  return(food)
}



##### Function: fruiting() #####
# Checks whether new unripe fruits need to be created at a tree based on a counter.
#
# Arguments:
# food                : Dataframe with tree information.
# fruit_dim_creation  : Column indicating whether there is fruit on a tree.
# regen_col           : Column indicating the number of timesteps until fruit regeneration.
# regen_counter       : Column tracking time left until fruit regeneration.
# fruit_unripe        : Column tracking the number of unripe fruits on trees.
# max_fruits          : Maximum number of unripe fruits a tree can have.

fruiting <- function(food, time_history, time, timestep,
                     fruit_dim_creation = 9, 
                     regen_col = 4,  
                     regen_counter = 5,  
                     fruit_unripe = 7,  
                     max_fruits = Fruit_max) {
  
  # Decrease counter by the elapsed time for each tree
  food[, regen_counter] <- food[, regen_counter] - (time - time_history[timestep]) 
  
  # Identify trees where the regeneration counter has reached zero
  update_rows <- food[, regen_counter] <= 0
  
  if (any(update_rows)) {
    # Update trees where new fruits should be created
    food[update_rows, fruit_dim_creation] <- 1  # Mark trees as having fruit
    food[update_rows, fruit_unripe] <- max_fruits  # Assign max unripe fruits
    food[update_rows, regen_counter] <- food[update_rows, regen_col] + food[update_rows, regen_counter]  # Reset counter with any overflow accounted for
  }
  
  return(food)
}



##### Function: Remembertemporal #####
# This function allows the agent to remember the locations of trees with unripe 
# fruit within its visual range. It adds information about these trees to the 
# agent's memory, ensuring that only trees not already in memory are considered. 
# The function checks for trees with unripe fruit, calculates their distance 
# from the agent (either from its current position or from its movement history), 
# and updates the memory with relevant details, such as the tree's location, 
# number of unripe fruits, and estimated ripening time.
#
# Parameters:
#   prim_agent           : Dataframe containing the agent's current location.
#   food                 : Dataframe containing information about trees, including 
#                          tree location and fruit count.
#   remembered           : Dataframe storing the agent's memory of tree locations.
#   history              : List of historical agent positions at each timestep.
#   timestep             : Current timestep in the simulation.
#   xloc_agent           : Column index for the agent's X-coordinate (default: 2).
#   yloc_agent           : Column index for the agent's Y-coordinate (default: 3).
#   xloc_food            : Column index for the tree's X-coordinate (default: 2).
#   yloc_food            : Column index for the tree's Y-coordinate (default: 3).
#   visdet               : Visual detection range of the agent (default: visual_det_range).
#   fruit_unripe         : Column index indicating the number of unripe fruits on a tree (default: 7).
#   remembered_tree_no   : Column index indicating the tree number in memory (default: 2).
#   time_to_forget       : Number of steps before a memory is forgotten (default: mem_length_ts).
#   fruit_dim_ripeness   : Column index tracking the ripeness level of fruits on trees (default: 10).
#   Tree_nr              : Column index for the tree number (default: 1).
#
# Returns:
#   A dataframe (`remembered`) with updated memory, including newly remembered trees.
#
# The function performs the following steps:
# 1. Checks that the `food` dataframe is not empty.
# 2. Identifies trees that are not already in memory.
# 3. Filters trees with unripe fruit.
# 4. Calculates distances from the agent's current or previous location to the trees.
# 5. Filters trees that are within the agent's visual detection range.
# 6. Incorporate trees in memory if it's not already stored.
Remembertemporal <- function(prim_agent, food, remembered, history, timestep,
                             xloc_agent = 2, yloc_agent = 3, 
                             xloc_food = 2, yloc_food = 3, 
                             visdet = visual_det_range, 
                             fruit_unripe = 7,  
                             remembered_tree_no = 2,
                             time_to_forget = mem_length_ts,
                             fruit_dim_ripeness = 10,  
                             Tree_nr = 1) {
  
  # Determine empty memory spots
  empty_slots <- which(remembered[, remembered_tree_no] == 0)
  
  # Ensure there is storage capacity left
  if (length(empty_slots) == 0) return(remembered)
  
  # Identify trees NOT already in memory
  unseen_trees <- food[!food[, Tree_nr] %in% remembered[, remembered_tree_no], , drop = FALSE]
  
  # Ensure unseen_trees is valid
  if (is.null(unseen_trees) || nrow(unseen_trees) == 0) return(remembered)
  
  # Find trees with unripe fruit
  unripe_trees <- unseen_trees[unseen_trees[, fruit_unripe] > 0, , drop = FALSE]
  
  # Ensure unripe_trees is valid before checking nrow()
  if (is.null(unripe_trees) || nrow(unripe_trees) == 0) return(remembered)
  
  # Extract locations of unripe trees
  tree_locations <- cbind(
    matrix(unripe_trees[, c(Tree_nr, xloc_food, yloc_food)], ncol = 3),
    rep(prim_agent[1, xloc_agent], times = nrow(unripe_trees)),
    rep(prim_agent[1, yloc_agent], times = nrow(unripe_trees))
  )
  
  # Calculate distances over movement path if agent had moved
  if (history[[timestep - 1]][1, xloc_agent] != prim_agent[1, xloc_agent] ||
      history[[timestep - 1]][1, yloc_agent] != prim_agent[1, yloc_agent]) {
    
    # Compute distances from movement path
    distances <- cbind(
      tree_locations[, 1], 
      linedistances(
        matrix(tree_locations[, 2:3, drop = FALSE], ncol = 2),
        matrix(c(history[[timestep - 1]][1, xloc_agent], 
                 history[[timestep - 1]][1, yloc_agent],
                 prim_agent[1, xloc_agent], 
                 prim_agent[1, yloc_agent]), 
               ncol = 2, byrow = TRUE)
      )
    )
    # remember that agent did move
    moved <- 1
    
  } else {  
    # Compute distances from current location
    distances <- cbind(
      tree_locations[, 1], 
      mapply(distance, tree_locations[, 2], tree_locations[, 3],
             tree_locations[, 4], tree_locations[, 5])
    )
    
    # remember that agent did not move
    moved <- 0
  }
  
  # Filter trees within the detection range
  distances <- distances[distances[, 2] < visdet, , drop = FALSE]
  
  # Ensure there are trees within detection range
  if (is.null(distances) || nrow(distances) == 0) return(remembered)
  
  # when there is enough storage capacity to store encountered unripe fruit-bearing trees. All of them are memorized
  if (length(empty_slots) >= nrow(distances)) {
    remembered[empty_slots[1:nrow(distances)], c(2:5, 7, 8, 11)] <- cbind(
      distances[, 1],  # Tree number
      food[distances[, 1], xloc_food],  # Tree X location
      food[distances[, 1], yloc_food],  # Tree Y location
      time_to_forget,  # Forget time
      remembered[empty_slots[1:nrow(distances)], 7] + 1,  # Memory count
      ripeness_estim(food[distances[, 1], fruit_dim_ripeness]),  # Estimated ripening time
      food[distances[, 1], fruit_unripe]  # Unripe fruit count
    )
    return(remembered)
  } 
  
  if (moved == 1) { # if agent did move, the trees first encountered are memorized (i.e. furthest to the point of arrival)
    
    # distances to end point
    dist_to_end <- apply(food[distances[, 1], 2:3, drop=FALSE], 1, function(row) {
      distance(prim_agent[1, xloc_agent], prim_agent[1, yloc_agent], row[1], row[2])
    })
    
    # order trees based on first encountered 
    ordered_distances <- distances[order(dist_to_end, decreasing = T), ]  
    ordered_distances <- ordered_distances[1:length(empty_slots), , drop = FALSE] # reduce to the number of empty slots
    
  } else { # if agent did not move, the trees closest to ripening are memorized
    # order trees by ripening
    ordered_distances <- distances[order(food[distances[, 1], fruit_dim_ripeness], decreasing = T), ]  
    ordered_distances <- ordered_distances[1:length(empty_slots), , drop = FALSE] # reduce to the number of empty slots
  }
  
  remembered[empty_slots[1:nrow(ordered_distances)], c(2:5, 7, 8, 11)] <- cbind(
    ordered_distances[, 1],  # Tree number
    food[ordered_distances[, 1], xloc_food],  # Tree X location
    food[ordered_distances[, 1], yloc_food],  # Tree Y location
    time_to_forget,  # Forget time
    remembered[empty_slots[1:nrow(ordered_distances)], 7] + 1,  # Memory count
    ripeness_estim(food[ordered_distances[, 1], fruit_dim_ripeness]),  # Estimated ripening time
    food[ordered_distances[, 1], fruit_unripe]  # Unripe fruit count
  )

  return(remembered)
}



##### Function: forgetting() #####
# Simulates memory decay for unripe fruit locations in an agent's memory.
# The agent forgets tree locations over time and estimates elapsed time inaccurately.
#
# Parameters:
# - remembered:       Dataframe storing remembered tree locations.
# - time_history:     Vector tracking time history.
# - time:             Current simulation time.
# - timestep:         Current timestep.
# - tree_no:          Column index in `remembered` storing tree IDs.
# - forget_counter:   Column index tracking how many timesteps remain before memory fades.
# - count_elapsed_ts: Column index tracking actual elapsed time in memory.
# - est_elapsed_ts:   Column index tracking agent's estimated elapsed time.
# - inac_fact:        Inaccuracy factor for estimating elapsed time.
#
# Returns:
# Updated `remembered` dataframe with decayed memories.
forgetting <- function(remembered, time_history, time, timestep,
                       tree_no = 2, forget_counter = 5, 
                       count_elapsed_ts = 12, est_elapsed_ts = 13, 
                       inac_fact = inaccuracy_factor,
                       forgetting_rate = forgetting_rate) {
  
  # Identify filled memory slots
  memories <- remembered[, tree_no] > 0
  
  # Ensure remembered is not empty
  if (nrow(remembered) == 0 || sum(memories) == 0) return(remembered)
  
  # Calculate elapsed time since last step
  passed_time <- time - time_history[timestep]
  
  if (sum(memories) > 0) {
    # Estimate elapsed time
    est_passed_time <- sapply(which(memories), function(i) { new_rtruncnorm(1,
                                                                            a = -1 * remembered[i, est_elapsed_ts],
                                                                            b = Inf, mean = passed_time,
                                                                            sd = inac_fact)})
    
    # Update the agent's estimate of elapsed time with noise
    remembered[memories, est_elapsed_ts] <- remembered[memories, est_elapsed_ts] + est_passed_time
    
    # Update the actual elapsed time since the tree was placed in memory
    remembered[memories, count_elapsed_ts] <- remembered[memories, count_elapsed_ts] + passed_time 
    
    # Reduce forget counter for remembered trees
    remembered[memories & remembered[, forget_counter] > 0, forget_counter] <- 
      remembered[memories & remembered[, forget_counter] > 0, forget_counter] - passed_time 
    
    # Reset memory slots based on forgetting curve: simple power law)
    # simple power law: R=t^(-b)
    
    values <- remembered[memories, count_elapsed_ts]
    
    # probability of retention of memory
    probabilities <- values^(-forgetting_rate)
    probabilities <- pmin(pmax(probabilities, 0), 1)  # keep in [0,1]
    
    retention <- rbinom(n = length(probabilities), size = 1, prob = probabilities)
    
    reset_rows <- retention == 0
    if (any(reset_rows)) {
      remembered[which(memories)[reset_rows], c(2:6, 8, 10, 11:13)] <- matrix(
        rep(c(0, 0, 0, 0, 0, 0, 10000, 0, 0, 0), times = sum(reset_rows)),
        ncol = 10, byrow = TRUE
      )
    }
  }
  
  return(remembered)
}



##### Function: update_mem() #####
# This function updates the agent’s memory of trees and their fruit ripeness. 
# It removes trees from memory if they no longer contain unripe fruit, ensures 
# that a remembered tree is not mistakenly targeted, and adjusts the agent’s 
# estimated ripening time for still unripe trees. 
# 
# ## Functional Logic:
# - If an agent moves, compute distances to remembered trees.
# - If a remembered tree is within visual range:
# - If the tree has no unripe fruit, remove it from memory.
# - If it still has unripe fruit but was a target, adjust the agent’s guess on ripening time.
#
# ## Parameters:
# - prim_agent:             Matrix containing the agent’s location.
# - food:                   Matrix with trees and fruit ripeness details.
# - remembered:             Matrix tracking remembered tree locations.
# - history:                List tracking past agent positions.
# - timestep:               The current timestep.
# - xloc_agent, yloc_agent: Column indices for the agent’s x and y location.
# - remembered_tree_no:     Column index storing the tree number in memory.
# - visdet:                 The agent’s vision detection range.
# - fruit_unripe:           Column index in `food` tracking unripe fruit counts.
# - target_col:             Column in `remembered` indicating if a tree is a target.
# - counter_to_ripen_col:   Column tracking when an agent estimates fruit will ripen.
# - fruit_ripeness_col:     Column in `food` indicating fruit ripeness.
# - yloc_remembered:        Column tracking remembered tree y-coordinates.
# - count_elapsed_ts, est_elapsed_ts: Columns tracking elapsed and estimated time since memory was created.

update_mem <- function(prim_agent, food, remembered, history, timestep,
                        xloc_agent = 2, yloc_agent = 3,
                        remembered_tree_no = 2, visdet = visual_det_range,
                        fruit_unripe = 7, target = 6, counter_to_ripen_col = 8, 
                        fruit_dim_ripeness = 10, yloc_remembered = 4, 
                        count_elapsed_ts = 12, est_elapsed_ts = 13,
                        time_to_forget = mem_length_ts) {
  
  # Ensure there are remembered trees before proceeding
  if (sum(remembered[, remembered_tree_no] == 0)) {
    return(remembered)  # Nothing to process, return unchanged
  }
  
  # Compute distance to remembered locations if agent moved
  prev_x <- history[[timestep - 1]][1, xloc_agent]
  prev_y <- history[[timestep - 1]][1, yloc_agent]
  curr_x <- prim_agent[1, xloc_agent]
  curr_y <- prim_agent[1, yloc_agent]
  
  if (prev_x != curr_x || prev_y != curr_y) {
    treedistances <- cbind(
      remembered,  # Remembered number, tree number
      linedistances(
        matrix(remembered[, 3:4, drop = FALSE], ncol = 2),
        matrix(c(prev_x, prev_y, curr_x, curr_y), ncol = 2, byrow = TRUE)
      )
    )
  } else {
    treedistances <- cbind(
      remembered,
      mapply(distance,
             remembered[, 3], remembered[, 4],
             rep(prim_agent[1, xloc_agent], times = nrow(remembered)),
             rep(prim_agent[1, yloc_agent], times = nrow(remembered))
             )
    )
  }
  
  # Remembered trees within visual detection range that have no more unripe fruit
  treedist_visible_nofruit <- (treedistances[, 14] < visdet) & (treedistances[, remembered_tree_no] != 0) & (food[treedistances[, 2], fruit_unripe]  == 0)
  
  if (any(treedist_visible_nofruit)) {
    # Reset memory for trees with no unripe fruit
    remembered[treedist_visible_nofruit, c(2:6, 8, 10:13)] <- 
      matrix(rep(c(0, 0, 0, 0, 0, 0, 10000, 0, 0, 0), 
                 times = sum(treedist_visible_nofruit)), ncol = 10, byrow = TRUE)
  }
    
  # Identify trees that still contain unripe fruit within visual detection range
  treedist_visible_fruit <- (treedistances[, 14] < visdet) & (treedistances[, remembered_tree_no] != 0) & (food[treedistances[, 2], fruit_unripe]  != 0)
  
  # Update memory for trees with unripe fruit
  if (any(treedist_visible_fruit)) {
    
    remembered[treedist_visible_fruit, c(5, 6, 12, 13)] <- cbind(time_to_forget,  # Forget time
      0, # Should not be a target
      0, # count_elapsed_ts
      0 # est_elapsed_ts
    )
    
    tree_nr_upd <- remembered[treedist_visible_fruit, remembered_tree_no]
    remembered[treedist_visible_fruit, counter_to_ripen_col]  <- ripeness_estim(food[tree_nr_upd, fruit_dim_ripeness])  # Estimated ripening time
  }
  
  return(remembered)
}



###### Function: move_primate() ######
# Simulates movement of an agent (primate) using episodic memory.
#
# Functionality:
# - Moves the agent towards visible food (within range).
# - Uses episodic memory if no food is in sight.
# - Moves randomly if no food is visible or remembered.
# - Detects food while moving and adjusts path if needed.
#
# Parameters:
# - prim_agent: Dataframe containing the agent's location.
# - food: Dataframe containing tree locations and fruit availability.
# - remembered: Dataframe with remembered tree locations.
# - history: List containing previous states of prim_agent.
# - timestep: Current timestep of the ABM.
# - xloc_agent, yloc_agent: Columns indicating agent's x/y location.
# - xloc_food, yloc_food: Columns for tree x/y locations.
# - fruit_col_prim: Column indicating number of ripe fruits (ripeness = 100).
# - visdet: Visual detection range for spotting food.
# - xmin, xmax, ymin, ymax: Environment boundaries.
# - stepmean, stepsd: Mean and SD of step lengths (normally distributed).
# - remembered_tree_x, remembered_tree_y: Columns for remembered tree locations.
# - eatdist: Distance within which the agent can eat fruit.
# - fruit_dim_creation, fruit_dim_ripeness, fruit_dim_decay: Columns tracking fruit status.
# - eaten_col: Column tracking number of eaten fruits.
# - target: Column indicating if a remembered tree is a target.
# - counter_to_ripen_col: Estimated ripening time of remembered fruit.
# - ang_sd: SD of the movement angle when moving randomly.
# - count_elapsed_ts, est_elapsed_ts: Elapsed time since memory creation.
# - memory: Boolean indicating whether memory is used.
# - targ_thres: Threshold determining if tree is targeted. time to ripen - estimated elapsed time < threshold
# - rememb_index: Column containing the index on which it is based of a remembered tree is a target.

move_primate <- function(prim_agent, food, remembered, history, timestep,
                         xloc_agent = 2, yloc_agent = 3, 
                         xloc_food = 2, yloc_food = 3, 
                         fruit_col_prim = 6, visdet = visual_det_range, 
                         xmin = env_xmin, xmax = env_xmax, 
                         ymin = env_ymin, ymax = env_ymax, 
                         stepmean = stepsize_mean, stepsd = stepsize_sd, 
                         remembered_tree_x = 3, remembered_tree_y = 4, 
                         eatdist = eat_range,  
                         fruit_dim_creation = 9, fruit_dim_ripeness = 10, fruit_dim_decay = 11, 
                         eaten_col = 4, target = 6,  
                         counter_to_ripen_col = 8, wrap_rho = wrapped_rho,
                         count_elapsed_ts = 12, est_elapsed_ts = 13, memory = memoryused,
                         rememb_index = 10, time_to_ripen = time_to_ripen, weight_time_dist = weight_time_dist) {
  
  # Initialize tracking variables
  memory_used <- F
  
  # Extract agent's location
  prim_x <- prim_agent[1, xloc_agent]
  prim_y <- prim_agent[1, yloc_agent]
  
  # ========== DETERMINE DISTANCE TO RIPE TREES ==========
  treelocations_ripe <- food[food[, fruit_col_prim] > 0, 1:yloc_food, drop = FALSE]
  
  # Check if there are trees with ripe fruit
  if (nrow(treelocations_ripe) > 0) {
    
    # Calculate distances from agent to each ripe tree
    treedistances_ripe <- cbind(
      tree_id = treelocations_ripe[, 1],  # Keep tree IDs
      distance = mapply(FUN = distance, 
                        treelocations_ripe[, xloc_food], 
                        treelocations_ripe[, yloc_food], 
                        MoreArgs = list(prim_x, prim_y))
    )
    
    # Get the closest location and number of these food tree with fruits
    min_index <- which.min(treedistances_ripe[, 2])    
    closesttree_ripe <- treedistances_ripe[min_index, ]
    
    # ========== EATING ==========
    if (closesttree_ripe[2] == eatdist) {
      eaten <- "Y"
      food[closesttree_ripe[1], fruit_col_prim] <- food[closesttree_ripe[1], fruit_col_prim] - 1
      
      # If the tree is depleted. The creation, ripeness, decay columns of the Tree will be reset
      if (food[closesttree_ripe[1], fruit_col_prim] == 0) {
        food[closesttree_ripe[1], c(fruit_dim_creation, fruit_dim_ripeness, fruit_dim_decay)] <- c(0, 0, 100)
      }
      
      # The fruit that is eaten is counted for the agent
      prim_agent[1, eaten_col] <- prim_agent[1, eaten_col] + 1
      
      return(list(prim_agent, food, remembered, moved = 0))
      
    } else if (closesttree_ripe[2] < visdet) {
      
      # ========== VISUAL DETECTION ==========
      # If one of trees is in detection range. It will move to the closest tree 
      prim_agent[1, yloc_agent] <- food[closesttree_ripe[1], yloc_food]
      prim_agent[1, xloc_agent] <- food[closesttree_ripe[1], xloc_food]
      
      # Calculate movement distance from previous timestep
      moved <- distance(prim_agent[1, xloc_agent], prim_agent[1, yloc_agent],
                        food[closesttree_ripe[1], xloc_food], food[closesttree_ripe[1], yloc_food])
        
      return(list(prim_agent, food, remembered, moved))
    }
  }
  
  # ========== EPISODIC MEMORY USAGE Part 1 ==========
  # Determining index and targeting remembered trees
  if (memory == T && sum(remembered[, 2]) > 0) { 
    
    # If no food is targeted yet
    if (sum(remembered[, target]) == 0) {
      
      # Non-empty memory slots get assigned the index value, which is: time to ripen - estimated elapsed time 
      valid_memory <- remembered[, 2] > 0
      remembered[valid_memory, rememb_index] <- abs(remembered[valid_memory, counter_to_ripen_col] - remembered[valid_memory, est_elapsed_ts])
      remembered[!valid_memory, rememb_index] <- 10000  # Assign a high value to empty slots
      
      # Determine distance to memories
      distances_remembered <- c(
        mapply(FUN = distance, 
               remembered[, remembered_tree_x], 
               remembered[, remembered_tree_y], 
               MoreArgs = list(prim_x, prim_y))
      )
      
      # index vector for out of sight trees
      trees_outofsight <- (distances_remembered > visdet) & valid_memory 
      
      if (length(trees_outofsight) > 0) {
        
        # Determine for trees_outofsight which has the lowest index (based on distance and ripening), 
        # for that tree is decides whether to target it or not
        minTime <- 0
        maxTime <- time_to_ripen 
        minDist <- visdet
        maxDist <- sqrt((xmax^2) + (ymax^2)) # longest possible distance in the environment
        
        # Time and distance scaled based on min/max
        scaled_time <- (remembered[trees_outofsight, rememb_index] - minTime) / (maxTime - minTime)
        scaled_dist <- (distances_remembered[trees_outofsight] - minDist) / (maxDist - minDist)
        
        wTime <- weight_time_dist / (weight_time_dist + 1)
        wDist <- 1 / (weight_time_dist + 1)
        
        # Get indices of the subset in the full dataframe
        subset_idx <- which(trees_outofsight)
        
        # Pick the absolute index of the best tree
        shortlist <- subset_idx[which.min(wTime * scaled_time + wDist * scaled_dist)]
        
        # Update target
        remembered[shortlist, target] <- targeting(remembered[shortlist, rememb_index])
      }
    }
    
    # ========== EPISODIC MEMORY USAGE Part 2 ==========
    # If a location has been targeted. Moving towards target
    if (sum(remembered[, target]) == 1) {               
      
      nr_target <- which.max(remembered[, target])
      distance_target <- distance(remembered[nr_target, remembered_tree_x], 
                                  remembered[nr_target, remembered_tree_y],
                                  prim_x, prim_y)
      
      # If the targeted location is out-of-sight, the agent moves to target
      if (distance_target > visdet) {  
        prim_agent[1, c(xloc_agent, yloc_agent)] <- move_2target(prim_x, prim_y, 
                                                                 remembered[nr_target, remembered_tree_x],
                                                                 remembered[nr_target, remembered_tree_y],
                                                                 rnorm(1, mean = stepmean, sd = stepsd))
        angle_move <- anglefun(prim_agent[1, xloc_agent],
                               prim_agent[1, yloc_agent],
                               prim_x, prim_y)
        dist_move <- distance(prim_agent[1, xloc_agent],
                              prim_agent[1, yloc_agent],
                              prim_x, prim_y)
        
        memory_used <- T 
      }
    }
  }

  
  #  ========= RANDOM MOVEMENT ========== 
  prim_agent_refl <- NULL
  
  if (!memory_used) {                                
    move <- random_movement(history[[timestep - 1]][1,xloc_agent], 
                            history[[timestep - 1]][1,yloc_agent],
                            prim_x, prim_y, stepmean = stepmean, 
                            stepsd = stepsd, wrap_rho = wrap_rho)
    x_move <- move$x_move
    y_move <- move$y_move
    
    angle_move <- move$angle_movement 
    dist_move <- move$distance
    
    # Add the reflecting boundary  
    prim_agent_refl <- refl_boundary(prim_x, prim_y, angle_move, 
                                     step = dist_move, 
                                     xmin = xmin, xmax = xmax, 
                                     ymin = ymin, ymax = ymax)

    prim_agent[1, c(xloc_agent, yloc_agent)] <- c(prim_agent_refl[[1]], prim_agent_refl[[2]])
    
  }
  
  
  #  ========= Perceptual range over moved line  ==========
  # When the agent moved randomly or moved with memory 
  # and if there was ripe fruits to potentially see, check perceptual range
  encountered <- NULL
  encountered2 <- NULL
  
  if (nrow(treelocations_ripe) > 0) {
    # Determine if trees with ripe fruit are encountered on path and which one first
    encountered <- path_encounter(prim_x, prim_y, 
                                  angle_move, dist_move, 
                                  matrix(treelocations_ripe[, 2:3], ncol = 2), visdet)
    
    # If reflective boundary was used and there was nothing at first segment:
    if (length(encountered) == 0 && length(prim_agent_refl[[4]]) != 0) {    
      
        # Determine if trees with ripe fruit are encountered on second segment of path (after reflection)
        encountered2 <- path_encounter(prim_agent_refl[[4]][1], prim_agent_refl[[4]][2], 
                                      prim_agent_refl[[5]], 
                                       distance(prim_agent_refl[[4]][1], 
                                                prim_agent_refl[[4]][2], 
                                                prim_agent_refl[[4]][3], 
                                                prim_agent_refl[[4]][4]), 
                                       matrix(treelocations_ripe[, 2:3], ncol = 2), visdet)
    }
  }

  # Adjust location of agent if a tree was encountered across the CRW, 
  # determine moved distance.
  if (length(encountered) > 0) { 
    prim_agent[1, xloc_agent:yloc_agent] <- treelocations_ripe[encountered[[1]], 2:3, drop=FALSE]  
    moved <- encountered[[2]] + encountered[[4]] # moved distance
    
    } else if (length(prim_agent_refl[[4]]) != 0 && length(encountered2) > 0) { 
      prim_agent[1, xloc_agent:yloc_agent] <- treelocations_ripe[encountered2[[1]], 2:3, drop=FALSE]  
      moved1 <- distance(prim_agent_refl[[3]][1], 
                         prim_agent_refl[[3]][2], 
                         prim_agent_refl[[3]][3], 
                         prim_agent_refl[[3]][4]) # moved distance along first segment
      moved2 <- encountered2[[2]] + encountered2[[4]] # moved distance along second segment
      moved <- moved1 + moved2
    } else {
    # Calculate movement distance from previous timestep
    moved <- distance(prim_x, prim_y, prim_agent[1, xloc_agent], prim_agent[1, yloc_agent])
  }
  
  
  return(list(prim_agent, food, remembered, moved))                                                                    
}



###### Function: ripeness_estim() ######
# This function estimates the number of time units required for a fruit to become ripe.
# 
# Arguments:
# - y_value: Current ripeness value of the fruit (range: 0-100, where 100 = fully ripe).
# - b_value: Scaling factor for time (default: 100 / time_to_ripen).
# - bwaarde: Scalar property for time uncertainty (range: 0 - 0.2). 
#            A value of 0 is perfect accuracy, while 0.2 is the least accurate.
# - edibile: Minimum ripeness value required for the fruit to be edible.
#
# Returns:
# - Estimated time units until the fruit reaches ripeness.

ripeness_estim <- function(y_value, 
                              b_value = (100 / time_to_ripen),  
                              edibile = edibilityPrim) {
  
  # Ensure that y_value does not exceed the edibility threshold
  y_value[y_value > edibile] <- edibile
  
  # Compute estimated time to ripeness with a normally distributed error term
  estimated_time <- ((edibile - y_value) / b_value)
  
  return(estimated_time)
}



###### Function: timepassage() ######
# Determines the amount of time that has passed since the last step in the model.
# Also corrects for movement/eating events that exceed the time maximum.
#
# Agent parameters:
# - Moves at a speed of movspeed (arbitrary length units per time unit).
# - Eats food at a rate of eatrate: food items/t (arbitrary food items per time unit).
#
# Arguments:
# - moved:        Moved distance of agent
# - time:         Current time value.
# - movspeed:     Speed of movement.
# - eatrate:      Eating rate.
#
# Returns:
# - Updated time value.

timepassage <- function(moved, time,
                        movspeed = movspeed, eatrate = eatrate) {
  
  # Update time based on movement or eating action
  if (moved > 0) {
    time <- time + (moved / movspeed)
  } else {
    time <- time + (1 / eatrate)  # If no movement, eating occurred
  }
  
  return(time)
}



###### Function: timecap() ######
# 
# Corrects movement or eating of an agent after the maximum allowed time.
# 
# Description:
# This function ensures that an agent does not eat beyond a predefined maximum time (`tmax`).  
# If the agent has eaten beyond `tmax`, the consumed fruit is restored.  
#
# Arguments:
# - prim_agent      : Matrix representing the primary agent's state.
# - history         : List of previous agent states, indexed by timestep.
# - food            : Matrix of current Trees.
# - foodhistory     : Matrix of Trees at previous ts.
# - timestep        : Representing the current time step.
# - time            : Representing the current simulation time.
# - xloc_agent      : Column index for the agent's x-coordinate.
# - yloc_agent      : Column index for the agent's y-coordinate.
# - movingspeed     : Speed at which the agent moves.
# - eatingspeed     : Speed at which the agent eats.
# - tmax            : Maximum allowed time.
# - fruit_col_prim  : Column index representing fruit quantity.
#
# Returns:
# A list containing:
# - Updated time (set to `tmax`).
# - Updated prim_agent matrix with corrected movement.
# - Updated food matrix with restored food (if necessary).

timecap <- function(prim_agent, history, food, foodhistory, timestep, time, 
                    xloc_agent = 2, yloc_agent = 3, 
                    movingspeed = movspeed, eatingspeed = eatrate, 
                    tmax = timemax, fruit_col_prim = 8) {
  
  # Check if the agent has eaten after max time
  if (prim_agent[1, 4] > history[[timestep]][1, 4]) {
    prim_agent[1, 4] <- prim_agent[1, 4] - 1
    
    # Identify fruit that was eaten in this step
    eaten_food <- food[, fruit_col_prim] != foodhistory[, fruit_col_prim]
    
    # Restore the fruit since it was eaten after tmax
    food[eaten_food, fruit_col_prim] <- food[eaten_food, fruit_col_prim] + 1
    
  } 
  
  # Set time to max allowable time
  time <- tmax
  
  return(list(time, prim_agent, food))
}



###### Function: initialize_env_agent() ######
# This function initializes various environmental and agent-related parameters.
# It sets up the environment's boundaries, the agent's initial state, 
# and relevant parameters for primate behavior, movement, memory, and fruit 
# ripening. These parameters are used throughout the simulation to govern 
# the agent's interactions with the environment.
#
# The function performs the following tasks:
# 1. Initializes environmental parameters such as landscape size and fruit
#    regeneration steps.
# 2. Sets agent-related parameters including detection range, cognitive
#    memory, and movement speeds.
# 3. Creates an agent object starting at the center of the environment.
# 4. Sets up a dataframe to store temporally remembered locations of trees
#    (with fruit), including memory characteristics and fruit ripening estimates.
#
# The initialized parameters are set globally and can be accessed
# throughout the simulation. 
initialize_env_agent <- function(nTree = nTree) {
  
  #### Environmental parameters ####
  # Initialize input values for parameters
  env_xmin          <<- 0
  env_xmax          <<- 1000
  env_ymin          <<- 0
  env_ymax          <<- 1000
  AverageRegenSteps <<- 365
  Fruit_max         <<- 1
  time_to_ripen     <<- 30
  decay_start       <<- 100
  
  #### Primate parameters ####
  # Initialize input values for parameters
  visual_det_range  <<- (2 * sqrt(nTree / (env_xmax * env_ymax))) ^ -1
  eat_range         <<- 0
  edibilityPrim     <<- 100
  
  forgetting_rate  <<- 0.001
  weight_time_dist <<- 2 # weight of time vs distance. time 2 times as important
  
  # Parameters related to cognitive abilities of primates
  scalarproptimeval <<- 0.00
  
  # Random movement parameters (for correlated random walk)
  stepsize_mean     <<- 100
  stepsize_sd       <<- 0
  
  # Angle parameters (correlated random walk)
  wrapped_rho       <<- 0.9
  
  ###### Creating Agent ######
  # Agent starts in the middle of the landscape
  # AgentNo   : Number of the agent
  # Xcoord    : X-coordinate of Agent
  # Ycoord    : Y-coordinate of Agent
  # eaten     : Number of eaten fruits
  Primate_agent <<- cbind(
    AgentNo = 1, 
    Xcoord = (env_xmax - env_xmin) / 2, 
    Ycoord = (env_ymax - env_ymin) / 2, 
    eaten = 0
  )
  
  ##### Dataframe for temporally remembered locations #####
  # RememberedNo: Number of the memory
  # Tree_no             : The number of the tree with which the memory corresponds
  # Xcoord              : X-coordinate of the remembered tree
  # Ycoord              : Y-coordinate of the remembered tree
  # forget_counter      : Counter for steps after which a memory is forgotten
  # target              : Indicator if memory is a target (0 or 1)
  # howmany             : Check how many times a memory is remembered in the memory slot
  # est_to_ripen        : Estimate of agent for number of steps until unripe fruit ripens
  # target_count        : Count how many times something is made a target
  # index               : Determines if something is made a target
  # nr_of_fruits        : Number of fruits on the remembered tree
  # counter_elapsed_ts  : Number of timesteps since tree was placed in memory
  # est_elapsed_ts      : Estimated number of timesteps since tree was placed in memory
  
  Temporal_remembered <<- cbind(
    RememberedNo = 1:memory_slots, 
    Tree_no = 0, 
    Xcoord = 0, 
    Ycoord = 0, 
    forget_counter = 0, 
    target = 0, 
    howmany = 0, 
    est_to_ripen = 0, 
    target_count = 0, 
    index = 10000, 
    nr_of_fruits = 0, 
    counter_elapsed_ts = 0, 
    est_elapsed_ts = 0
  )
  
}



########### ABM Function to Run ABM #########
ABM <- function(init_time = 60, sim_time = 365) {
  
  #initialize environmental and agent parameters
  initialize_env_agent(nTree = nTree)
  
  ###### Creating Food Trees ######
  # Creating locations of food.
  # TreeNo: Number of the Tree
  # Xcoord: X-coordinate of Tree
  # Ycoord: Y-coordinate of Tree
  # Regeneration: Number of timesteps for trees to re-fruit after fruiting
  # Regen_counter: Countdown for fruit ripening (values: 50,100,150,...350)
  # Fruit_total_95: Number of fully ripe fruits at a timestep
  # Unripe: Number of unripe fruits in process
  # Total_Fruits_created: Total ripe fruits created in simulation
  # Fruits_being_created: Fruits currently in development
  # Fruit_ripen_count: Ripeness counter (0-100)
  # Fruit_decay_count: Decay counter (100-0)
  
  tree_coords <- generate_env(
    ntree = nTree, xmin = env_xmin, xmax = env_xmax, 
    ymin = env_ymin, ymax = env_ymax, homogenous = homogen, 
    n_centers = num_cent, std = cent_std
  )
  
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
  
  ###### Run Simulation ######
  ## Initialization Phase
  timemax <- init_time
  allocated_size <- round(timemax * 5.1)
  
  Primate_agent_hist <- vector("list", timemax)  # Store primate states at each step
  Trees_hist <- matrix(nrow = nTree, ncol = ncol(Trees))  # Track tree state
  time_hist <- numeric(allocated_size)  # Track time progression
  
  # Initialize first two steps for correlated random walk
  Primate_agent_hist[[1]] <- Primate_agent
  Primate_agent_hist[[2]] <- Primate_agent
  time_hist[1:2] <- 0
  moved <- 0
    
  time <- 0
  ts <- 2
  
  ###### Run Initialization Simulation ######
  while (time < timemax) {

    movement_primatelist <- move_primate(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts, time_to_ripen = time_to_ripen, weight_time_dist = weight_time_dist)
    Primate_agent <- movement_primatelist[[1]]
    Trees <- movement_primatelist[[2]]
    Temporal_remembered <- movement_primatelist[[3]]
    moved <- movement_primatelist[[4]]
    
    rm(movement_primatelist)

    time <- timepassage(moved, time, movspeed = movspeed, eatrate = eatrate)  # Distance-based time progression
    
    if (time > timemax) {        
      timelist <- timecap(Primate_agent, Primate_agent_hist, Trees, Trees_hist, ts, time, tmax = timemax)
      time <- timelist[[1]]
      Primate_agent <- timelist[[2]]
      Trees <- timelist[[3]]
      rm(timelist)
    }

    # Update memory and tree states
    Temporal_remembered <- Remembertemporal(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts)
    Temporal_remembered <- update_mem(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts)
    Temporal_remembered <- forgetting(Temporal_remembered, time_hist, time, ts, forgetting_rate = forgetting_rate)

    Trees <- fruitripening(Trees, time_hist, time, ts)
    Trees <- fruitdecay(Trees, time_hist, time, ts)
    Trees <- fruiting(Trees, time_hist, time, ts)
    
    ts <- ts + 1

    # Expand allocated size if exceeded
    if (ts > allocated_size) {
      allocated_size <- as.integer(round(allocated_size * 1.05))
      length(time_hist) <- allocated_size
    }
    
    # Store updated objects
    Primate_agent_hist[[ts]] <- Primate_agent
    Trees_hist <- Trees
    time_hist[ts] <- time
    
  }
  
  # Trim excess allocation
  time_hist <- time_hist[1:(ts - 1)]
  
  ###### Run Main Simulation (365 timesteps) ######
  timemax <- sim_time
  allocated_size <- round(timemax * 5.1)
  
  # Reset Primate agent
  Primate_agent <<- cbind(
    AgentNo = 1, 
    Xcoord = (env_xmax - env_xmin) / 2, 
    Ycoord = (env_ymax - env_ymin) / 2, 
    eaten = 0
  )
  

  Primate_agent_hist <- vector("list", timemax)
  Trees_hist <- matrix(nrow = nTree, ncol = ncol(Trees))
  time_hist <- numeric(allocated_size)
  
  # Reset fruit tracking and eating counts
  Trees[Trees[, 7] == 0, 8] <- 0  # Reset fruit creation count
  Primate_agent[1, 4] <- 0  # Reset eaten fruit count
  
  # Assign initial agent states
  Primate_agent_hist[[1]] <- Primate_agent
  Primate_agent_hist[[2]] <- Primate_agent

  time_hist[1:2] <- 0
  time <- 0
  ts <- 2
  
  moved <- 0
  
  ###### Run Execution Simulation ######
  while (time < timemax) {

    movement_primatelist <- move_primate(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts, time_to_ripen = time_to_ripen, weight_time_dist = weight_time_dist)
    Primate_agent <- movement_primatelist[[1]]
    Trees <- movement_primatelist[[2]]
    Temporal_remembered <- movement_primatelist[[3]]
    moved <- movement_primatelist[[4]]
    
    rm(movement_primatelist)
    
    time <- timepassage(moved, time, movspeed = movspeed, eatrate = eatrate)
    
    if (time > timemax) {        
      timelist <- timecap(Primate_agent, Primate_agent_hist, Trees, Trees_hist, ts, time, tmax = timemax)
      time <- timelist[[1]]
      Primate_agent <- timelist[[2]]
      Trees <- timelist[[3]]
      rm(timelist)
    }

    # Update memory and tree states
    Temporal_remembered <- Remembertemporal(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts)
    Temporal_remembered <- update_mem(Primate_agent, Trees, Temporal_remembered, Primate_agent_hist, ts)
    Temporal_remembered <- forgetting(Temporal_remembered, time_hist, time, ts, forgetting_rate = forgetting_rate)
    
    Trees <- fruitripening(Trees, time_hist, time, ts)
    Trees <- fruitdecay(Trees, time_hist, time, ts)
    Trees <- fruiting(Trees, time_hist, time, ts)

    ts <- ts + 1
    
    # Expand allocated size if exceeded
    if (ts > allocated_size) {
      allocated_size <- as.integer(round(allocated_size * 1.1))
      length(time_hist) <- allocated_size
    }
    
    # Store updated objects
    Primate_agent_hist[[ts]] <- Primate_agent
    Trees_hist <- Trees
    time_hist[ts] <- time
  }
  
  # Trim excess allocation
  time_hist <- time_hist[1:(ts - 1)]
  
  # Return output
  c(
    eaten = Primate_agent[1, 4], n_fruits_created = sum(Trees[, 8]), 
    scalarproptimeval = scalarproptimeval, inaccuracy_factor = inaccuracy_factor, 
    time_to_ripen = time_to_ripen, edibilityPrim = edibilityPrim, time_to_dis = time_to_dis, 
    decay_start = decay_start, Fruit_max = Fruit_max, visual_det_range = visual_det_range, 
    nTree = nTree, env_xmin = env_xmin, env_ymin = env_ymin, env_ymax = env_ymax, 
    AverageRegenSteps = AverageRegenSteps, eat_range = eat_range, 
    mem_length_ts = mem_length_ts, memory_slots = memory_slots, 
    stepsize_mean = stepsize_mean, stepsize_sd = stepsize_sd, wrapped_rho = wrapped_rho, 
    time = time, ts = ts, seed = seed, cent_std = cent_std, memory = memoryused)
}




###### Function: new_rtruncnorm() ######
# This function generates truncated normal random variables. It wraps around 
# the `rtruncnorm` function, with an added check for when the standard deviation 
# (sd) is zero. If sd is zero, the function returns the mean value as the 
# result. Otherwise, it generates truncated normal random variables using `rtruncnorm`.
#
# Arguments:
# - n     : The number of random variables to generate.
# - a     : The lower bound for truncation (default is -Inf).
# - b     : The upper bound for truncation (default is Inf).
# - mean  : The mean of the normal distribution (default is 0).
# - sd    : The standard deviation of the normal distribution (default is 1).

new_rtruncnorm <- function(n, a = -Inf, b = Inf, mean = 0, sd = 1) {
  # Check if the standard deviation is zero
  if (sd == 0) {
    # Return the mean if there is no variance (degenerate distribution)
    return(mean)
  } else {
    # Generate and return truncated normal random variables
    return(rtruncnorm(n, a, b, mean, sd))
  }
}


##### Function: generate_centered_trees() #####
# This function generates tree coordinates centered around a given (x, y) center 
# according to a circular bivariate normal distribution. The function ensures that 
# the generated coordinates stay within specified boundary limits. If some of the 
# generated coordinates are outside the boundary, it recursively generates new 
# coordinates to replace the invalid ones.
#
# Arguments:
#   num_trees_per_center : Integer. The number of tree coordinates to generate.
#   center_x             : Numeric. The X-coordinate of the center of the distribution.
#   center_y             : Numeric. The Y-coordinate of the center of the distribution.
#   sd                   : Numeric. The standard deviation that determines the spread 
#                          of the trees around the center.
#   boundary_low         : Numeric. The lower boundary of the area (default: 0).
#   boundary_high        : Numeric. The upper boundary of the area (default: 1000).
#
# Returns:
#   A matrix with `num_trees_per_center` rows and 2 columns representing the 
#   (X, Y) coordinates of the generated tree locations, all within the specified 
#   boundary limits.

generate_centered_trees <- function(num_trees_per_center, center_x, center_y, sd, 
                                    boundary_low = env_xmin, boundary_high = env_xmax) {
  
  # Generate initial coordinates from a bivariate normal distribution centered at (center_x, center_y)
  temp_coords <- matrix(
    MASS::mvrnorm(n = num_trees_per_center, 
                  mu = c(center_x, center_y), 
                  Sigma = matrix(c(sd^2, 0, 0, sd^2), nrow = 2)),
    ncol = 2
  )
  
  # Filter out coordinates that are outside the defined boundary limits
  valid_coords <- temp_coords[
    temp_coords[, 1] >= boundary_low & temp_coords[, 1] <= boundary_high &
      temp_coords[, 2] >= boundary_low & temp_coords[, 2] <= boundary_high, 
    , drop = FALSE
  ]
  
  # If the number of valid coordinates is less than required, generate additional coordinates
  if (nrow(valid_coords) < num_trees_per_center) {
    additional_coords <- generate_centered_trees(
      num_trees_per_center - nrow(valid_coords), 
      center_x, center_y, sd, boundary_low, boundary_high
    )
    valid_coords <- rbind(valid_coords, additional_coords)
  }
  
  # Return only the required number of valid coordinates
  return(valid_coords[1:num_trees_per_center, ])
}











##### Function: generate_het_env() #####
# This function generates tree coordinates centered around multiple random centers 
# within a specified area, following a circular bivariate normal distribution. 
# The coordinates are distributed across a specified number of centers, ensuring 
# that the total number of trees is divided as evenly as possible between the centers. 
# If there are any leftover trees, they are added to the last center.
#
# Arguments:
#   num_trees   : Integer. The total number of tree coordinates to generate.
#   num_centers : Integer. The number of centers around which trees will be generated.
#   std_dev     : Numeric. The standard deviation that determines the spread of trees 
#                 around each center.
#   x_min,x_max : Numeric. The min & max X/Y-coordinate value for the center locations.
#   y_min,y_max : Numeric. The min & max X/Y-coordinate value for the center locations.
#
# Returns:
#   A matrix with `num_trees` rows and 2 columns, representing the (X, Y) coordinates 
#   of the generated tree locations around multiple centers within the specified boundaries.

generate_het_env <- function(num_trees, num_centers, std_dev, x_min, x_max, y_min, y_max) {
  
  # Calculate the number of trees per center, with remaining trees assigned to the last center
  trees_per_center <- floor(num_trees / num_centers)
  remaining_trees <- num_trees - trees_per_center * (num_centers - 1)
  
  # Initialize an empty list to store tree coordinates from each center
  all_centers <- lapply(1:(num_centers - 1), function(i) {
    # Randomly generate the (x, y) coordinates for the center
    center_x <- runif(1, x_min, x_max)
    center_y <- runif(1, y_min, y_max)
    
    # Generate tree coordinates around this center
    coords <- generate_centered_trees(trees_per_center, center_x, center_y, std_dev)
    return(coords)
  })
  
  # Generate the coordinates for the last center with any remaining trees
  last_center <- lapply(1, function(i) {
    center_x <- runif(1, x_min, x_max)
    center_y <- runif(1, y_min, y_max)
    
    # Generate the coordinates for the remaining trees
    coords <- generate_centered_trees(remaining_trees, center_x, center_y, std_dev)
    return(coords)
  })
  
  # Combine all generated coordinates from each center
  all_coords <- c(all_centers, last_center)
  tree_coords <- do.call(rbind, all_coords)  # Combine into a single matrix
  
  return(tree_coords)
}



##### Function: generate_env #####
# This function generates tree coordinates within a specified environment. 
# The environment can be either homogeneous or heterogeneous based on the value of the `homogenous` parameter.
# - If `homogenous = 1`, the environment is homogeneous, and tree coordinates are generated randomly across 
#   the entire area defined by the given boundaries.
# - If `homogenous = 0`, the environment is heterogeneous, with tree coordinates generated around multiple centers 
#   following a circular bivariate normal distribution. The number of centers and the standard deviation of the distribution 
#   are specified by `n_centers` and `std`.
#
# Arguments:
#   ntree     : The number of tree coordinates to generate.
#   xmin      : he minimum X-coordinate boundary of the environment.
#   xmax      : The maximum X-coordinate boundary of the environment.
#   ymin      : The minimum Y-coordinate boundary of the environment.
#   ymax      : The maximum Y-coordinate boundary of the environment.
#   homogenous: If 1, the environment is homogeneous. If 0, the environment is heterogeneous.
#   n_centers : The number of centers for generating tree coordinates in a heterogeneous environment (default: 0).
#   std       : The standard deviation of the circular bivariate normal distribution for the heterogeneous environment (default: 0).
#
# Returns:
#   A matrix with `ntree` rows and 2 columns, representing the (X, Y) coordinates of the generated tree locations.

generate_env <- function(ntree, xmin, xmax, ymin, ymax, homogenous = 1, n_centers = 0, std = 0) {
  
  # Check if the environment is homogeneous
  if (homogenous == 1) {
    # Generate random tree coordinates across the entire environment
    tree_coord <- cbind(
      Xcoord = runif(ntree, xmin, xmax),  # X coordinates between xmin and xmax
      Ycoord = runif(ntree, ymin, ymax)   # Y coordinates between ymin and ymax
    )
    
  } else {
    # If the environment is heterogeneous, generate tree coordinates around multiple centers
    tree_coord <- generate_het_env(ntree, n_centers, std, xmin, xmax, ymin, ymax)
  }
  
  # Return the generated tree coordinates
  return(tree_coord)
}



##### Function: targeting #####
# This function assigns a probability based on the tu_diff and determines whether
# a Tree should be targeted based on this.
# 
# Arguments:
#   tu_diff : The estimated difference between to time to ripen - elapsed time 
#   scale   : scale that ensures that smaller values of tu_diff yield higher probabilities of 1.
targeting <- function(tu_diff, scale = target_scale) {
  # Convert tu_diff into a probability (Gaussian function)
  prob <- exp(- (tu_diff^2) / (2 * scale^2))  # Closer to 0 → Higher probability of 1
  
  # Ensure probability is within (0,1)
  prob <- max(0, min(1, prob))
  
  # Generate 1 or 0 based on the computed probability
  return(rbinom(1, 1, prob))
}




##### Function: path_encounter #####
# Checks if trees are encountered on the way
#
# Arguments :
#   Ax, Ay            : Starting coordinates of A (agent)
#   angle_deg         : Movement direction in degrees
#   step_length       : Moved distance along the path to check
#   Tree_matrix       : Matrix of Tree locations (columns: x, y coordinates)
#   detection_radius  : Visual detection radius

path_encounter <- function(
    Ax, Ay,              
    angle_deg,           
    step_length,         
    Tree_matrix,            
    detection_radius     
) {
  
  # Convert movement angle to radians
  angle_rad <- angle_deg * pi / 180
  
  # Unit vector in movement direction
  dx <- sin(angle_rad)  # x component
  dy <- cos(angle_rad)  # y component
  
  # Vector from each Tree to A
  dxATree <- Ax - Tree_matrix[,1]  # x-component difference
  dyATree <- Ay - Tree_matrix[,2]  # y-component difference
  
  
  # Quadratic coefficients for intersection of line with circle
  # Circle: (x - Treex)^2 + (y - Treey)^2 = detection_radius^2
  # Line: A moves along (Ax + s*dx, Ay + s*dy), s >= 0
  
  a <- 1  # dx^2 + dy^2 = 1 (unit vector)
  b <- 2 * (dxATree * dx + dyATree * dy)
  c <- dxATree^2 + dyATree^2 - detection_radius^2
  
  # Discriminant to check real intersection
  disc <- b^2 - 4*a*c
  
  # Keep only Trees with real intersections
  valid_disc <- disc >= 0
  if (!any(valid_disc)) return(NULL)  # No intersections at all
  
  # Subset to valid Trees
  dxATree <- dxATree[valid_disc]
  dyATree <- dyATree[valid_disc]
  b <- b[valid_disc]
  disc <- disc[valid_disc]
  Tree_matrix_valid <- Tree_matrix[valid_disc,,drop=FALSE]
  Tree_indices <- which(valid_disc)  # original row indices of valid Trees
  
  
  # Solve quadratic for intersection points (distance along path)
  
  sqrt_disc <- sqrt(disc)
  s1 <- (-b - sqrt_disc)/2
  s2 <- (-b + sqrt_disc)/2
  
  # Only keep forward intersections (s >= 0)
  s1[s1 < 0] <- NA
  s2[s2 < 0] <- NA
  
  # Take first positive intersection per Tree
  s_first <- pmin(s1, s2, na.rm=TRUE)
  
  # Only keep intersections within step_length
  keep <- !is.na(s_first) & (s_first <= step_length)
  if (!any(keep)) return(NULL)  # No Tree detected within step length
  
  # Subset to those Trees
  s_first <- s_first[keep]
  Tree_matrix_final <- Tree_matrix_valid[keep,,drop=FALSE]
  Tree_indices_final <- Tree_indices[keep]  # keep original indices
  
  # Compute entry coordinates and distance to Tree
  entry_x <- Ax + s_first * dx
  entry_y <- Ay + s_first * dy
  distance_entry_to_Tree <- sqrt((entry_x - Tree_matrix_final[,1])^2 + 
                                   (entry_y - Tree_matrix_final[,2])^2)
  
  # Find first encountered Tree along the path
  first_idx <- which.min(s_first)
  
  # Return list with :
  list(
    Tree_index = Tree_indices_final[first_idx],        # Row of Tree in original Tree_matrix
    distance_along_path = s_first[first_idx],    # Distance traveled along path to detection
    entry_point = c(entry_x[first_idx], entry_y[first_idx]), # Coordinates where Tree is detected
    distance_entry_to_Tree = distance_entry_to_Tree[first_idx]     # Distance from entry point to Tree (= detection_radius)
  )
}


####### Compiling Functions #######
# The following code compiles all the functions used in the model to optimize performance.

# Compile individual functions using 'cmpfun'
distance                <- cmpfun(distance)
anglefun                <- cmpfun(anglefun)
ripen                   <- cmpfun(ripen)
fruittipening           <- cmpfun(fruitripening)
fruitdecay              <- cmpfun(fruitdecay)
fruiting                <- cmpfun(fruiting)
ripeness_estim          <- cmpfun(ripeness_estim)
decay                   <- cmpfun(decay)
Remembertemporal        <- cmpfun(Remembertemporal)
forgetting              <- cmpfun(forgetting)
update_mem              <- cmpfun(update_mem)
move_primate            <- cmpfun(move_primate)
timepassage             <- cmpfun(timepassage)
timecap                 <- cmpfun(timecap)
linedistances           <- cmpfun(linedistances)
initialize_env_agent    <- cmpfun(initialize_env_agent)
ABM                     <- cmpfun(ABM)
new_rtruncnorm          <- cmpfun(new_rtruncnorm)
generate_centered_trees <- cmpfun(generate_centered_trees)
generate_het_env        <- cmpfun(generate_het_env)
generate_env            <- cmpfun(generate_env)
random_movement         <- cmpfun(random_movement)
collision               <- cmpfun(collision)
safe_angle_segment      <- cmpfun(safe_angle_segment)
refl_boundary           <- cmpfun(refl_boundary)
move_2target            <- cmpfun(move_2target)
targeting               <- cmpfun(targeting)
path_encounter          <- cmpfun(path_encounter)
