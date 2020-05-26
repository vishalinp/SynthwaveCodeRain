library(dplyr)
library(ggplot2)
library(gganimate)
library(purrr)

#code rain character set
katakana <- c("\u30a0", "\u30a1", "\u30a2", "\u30a3", "\u30a4", "\u30a5", "\u30a6", "\u30a7", "\u30a8",
              "\u30a9", "\u30aa", "\u30ab", "\u30ac", "\u30ad", "\u30ae", "\u30af", "\u30b0", "\u30b1",
              "\u30b2", "\u30b3", "\u30b4", "\u30b5", "\u30b6", "\u30b7", "\u30b8", "\u30b9", "\u30ba",
              "\u30bb", "\u30bc", "\u30bd", "\u30be", "\u30bf", "\u30c0", "\u30c1", "\u30c2", "\u30c3",
              "\u30c4", "\u30c5", "\u30c6", "\u30c7", "\u30c8", "\u30c9", "\u30ca", "\u30cb", "\u30cc",
              "\u30cd", "\u30ce", "\u30cf", "\u30d0", "\u30d1", "\u30d2", "\u30d3", "\u30d4", "\u30d5",
              "\u30d6", "\u30d7", "\u30d8", "\u30d9", "\u30da", "\u30db", "\u30dc", "\u30dd", "\u30de",
              "\u30df", "\u30e0", "\u30e1", "\u30e2", "\u30e3", "\u30e4", "\u30e5", "\u30e6", "\u30e7",
              "\u30e8", "\u30e9", "\u30ea", "\u30eb", "\u30ec", "\u30ed", "\u30ee", "\u30ef", "\u30f0",
              "\u30f1", "\u30f2", "\u30f3", "\u30f4", "\u30f5", "\u30f6", "\u30f7", "\u30f8", "\u30f9",
              "\u30fa", "\u30fb", "\u30fc", "\u30fd", "\u30fe", "\u30ff")

#parameters and behaviour
total_col <- 100
total_col_padded <- round(total_col * 1.1, digits=0)
active_col_perc <- 0.85
initial_col_perc <- 0.3
num_initial_col <- round(total_col_padded * initial_col_perc, digits=0)
col_min <- round(1 - total_col * 0.05, digits=0)
col_max <- round(total_col + total_col * 0.05, digits=0)

total_row <- 50
start_pos_min <- total_row * 1.1
start_pos_max <- (total_row * 1.1) + 10
row_bot_cutoff <- round(0 - total_row * 0.05, digits=0)

lifespan_min = round(total_row * 0.9)
lifespan_max = round(total_row * 1.5)

ring_buffer <- 10
ring_active <- FALSE

res_height <- 1080
res_width <- 2560
x_scale_adj <- ifelse((res_width/total_col)/(res_height/total_row) > 1, (res_width/total_col)/(res_height/total_row), 1)
y_scale_adj <- ifelse((res_height/total_row)/(res_width/total_col) > 1, (res_heigth/total_row)/(res_width/total_col), 1)


#HELPER FUNCTIONS
gen_base_val <- function()
{
  roll <- runif(1, 0, 1)
  
  if (roll < 0.05)
  {
    return(runif(1, 0.4, 0.6))
  }else
  {
    if (roll < 0.3)
    {
      return(runif(1, 0.6, 0.8))
    }else
    {
      return(runif(1, 0.8, 1))
    }
  }
}

sat_aging <- function(age)
{
  sat <- ifelse(age < 5, pmax(age, 1)*0.1 + (runif(1, -0.05, 0.05)), 1)
  
  return(sat)
}

val_aging <- function(fade_length, base_val, val)
{
  return(pmax(val - base_val/fade_length, 0))
}


#Initialize code rain data frame
rainframe <- data.frame(frame = 0,
                        col = sample(col_min:col_max, num_initial_col), 
                        h = sample(start_pos_min:start_pos_max, num_initial_col, replace = T), 
                        char = sample(katakana, num_initial_col, replace = T), 
                        leading = T,
                        speed = ifelse(sample(runif(num_initial_col, 0, 1), num_initial_col) > 0.85, 2, 1),
                        age = 1,
                        lifespan = sample(lifespan_min:lifespan_max, num_initial_col, replace=T),
                        fade_length = sample(6:10, num_initial_col, replace=T),
                        expired = F,
                        display = T,
                        special = F,
                        base_val = replicate(num_initial_col, gen_base_val()), 
                        hue = 0.525, 
                        sat = 1) %>%
  mutate(val = base_val)


#Iterate frames
for(i in 1:500)
{
  
  working_frame <- rainframe %>%
    filter(frame == i-1, special == F) %>%
    mutate(display = T)
  
  #mark rain elements that have faded, moved off-screen, or otherwise expired as EXPIRED
  working_frame <- working_frame %>%
    mutate(expired = ifelse(h < row_bot_cutoff | age == lifespan | val < 0.01, T, F))
  
  #Check num active columns, initialize new columns if below threshold
  active_col <- working_frame %>%
    filter(h > 0.15*total_row, !expired) %>%
    distinct(col) %>%
    .$col
  
  avail_col <- (col_min:col_max)[! col_min:col_max %in% active_col]
  
  #randomly select new columns to activate if total is below threshold
  if(length(active_col)/total_col_padded < active_col_perc)
  {
    #new_col_activations <- sample(avail_col, sample(1:length(avail_col), 1))
    new_col_activations <- sample(avail_col, min(sample(1:length(avail_col)), 0.3 * total_col))
    
    #generate data rows for new columns
    new_cols <- data.frame(frame = i,
                           col = new_col_activations, 
                           h = sample(start_pos_min:start_pos_max, length(new_col_activations), replace = T), 
                           char = sample(katakana, length(new_col_activations), replace = T), 
                           leading = T,
                           speed = ifelse(sample(runif(length(new_col_activations), 0, 1), length(new_col_activations)) > 0.85, 2, 1),
                           age = 1,
                           lifespan = sample(lifespan_min:lifespan_max, length(new_col_activations), replace=T),
                           fade_length = sample(6:10, length(new_col_activations), replace=T),
                           expired = F,
                           display = T,
                           special = F,
                           base_val = replicate(length(new_col_activations), gen_base_val()), 
                           hue = 0.525, 
                           sat = 1) %>%
      mutate(val = base_val)
    
    #append to working frame
    working_frame <- working_frame %>%
      rbind(new_cols)
  }
    
  
  #persist trailing chars from previous frame with glitch chance (randomly change chars)
  num_trailing <- working_frame %>%
    filter(frame == i-1, !leading, !expired) %>%
    nrow()
  
  still_trailing <- working_frame %>%
    filter(frame == i-1, !leading, !expired) %>%
    mutate(frame = i,
           char = ifelse(runif(num_trailing, 0, 1) > 0.98, sample(katakana, num_trailing, replace=T), as.character(char)),
           age = age+speed,
           sat = sat_aging(age))
  
  #append to working frame
  working_frame <- working_frame %>%
    rbind(still_trailing)
  
  #simulate 'fall' - move leading chars down by speed variable
  num_leading_chars <- working_frame %>%
    filter(leading == T, frame == i-1, !expired) %>%
    nrow()
    
  leading_chars <- working_frame %>%
    filter(leading == T, frame == i-1, !expired) %>%
    mutate(frame = i,
           h = h - speed,
           char = sample(katakana, num_leading_chars, replace = T),
           age = age+speed,
           sat = 0)
    
  #append to working frame
  working_frame <- working_frame %>%
    rbind(leading_chars)
  
  
  #generate trailing chars in wake of leading chars
  max_speed <- working_frame %>%
    filter(leading == T & frame == i-1 & !expired) %>%
    .$speed %>%
    max()
  
  if(max_speed > 0)
  {
    for(j in 1:max_speed)
    {
      num_trailing <- working_frame %>%
        filter(leading == T, frame == i-1, !expired) %>%
        nrow()
      
      trailing_chars <- working_frame %>%
        filter(leading == T, frame == i-1, !expired) %>%
        mutate(frame = i,
               h = h - j + 1,
               char = ifelse(j > 1, sample(katakana, length(active_col), replace = T), as.character(char)),
               leading = F,
               age = 1,
               sat = sat_aging(age))
      
      #append to working frame
      working_frame <- working_frame %>%
        rbind(trailing_chars)
    }
  }
  
  
  #fade chars approaching lifespan age
  working_frame <- working_frame %>%
    mutate(val = ifelse(!leading & frame == i & !expired & lifespan - age < fade_length, val_aging(fade_length, base_val, val), val))
  
  
  #magenta ring module
  if(i > total_row)
  {
    if(!ring_active)
    {
      ring_buffer <- ring_buffer - 1
    }
    
    if(!ring_active && ring_buffer == 0)
    {
      center_x <- sample(round(total_col * 0.2, digits = 0):round(total_col * 0.8, digits = 0), 1)
      center_y <- sample(round(total_row * 0.2, digits = 0):round(total_row * 0.8, digits = 0), 1)
      
      radius <- sample(50:100, 1) %>% ifelse(. %% 2 > 0, . + 1, .)
      ring_active <- TRUE
    }
    
    if(ring_active)
    {
      #deactivate chars in place of ring chars
      working_frame <- working_frame %>%
        mutate(display = ifelse(between(sqrt( ((center_x - col)*x_scale_adj)^2 + ((center_y - h)*y_scale_adj)^2 ), radius - 0.5, radius + 3), F, display))
      
      ring_chars <- working_frame %>%
        filter(between(sqrt( ((center_x - col)*x_scale_adj)^2 + ((center_y - h)*y_scale_adj)^2 ), radius - 0.5, radius + 3))
      
      ring_chars <- ring_chars %>%
        mutate(display = T,
               special = T,
               sat = pmax(sat - 0.4, 0),
               hue = ifelse(runif(nrow(ring_chars), -0.5, sqrt( ((center_x - col)*x_scale_adj)^2 + ((center_y - h)*y_scale_adj)^2 ) - radius) < 0.7, 0.8, 0.525))
      
      working_frame <- working_frame %>%
        rbind(ring_chars)
        
      radius <- radius - 2
      
      if(radius == 0)
      {
        ring_active <- FALSE
        ring_buffer <- sample(7:15, 1)
      }
    }

  }
  
  #append working frame to rainframe
  rainframe <- rainframe %>%
    rbind(working_frame)
}


rainplot <- ggplot(filter(rainframe, display), aes(x=col, y=h)) +
  geom_text(aes(label = char, colour=hsv(hue, sat, val)), size=2, show.legend = F) +
  scale_colour_identity() +
  coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
  theme(axis.title = element_text(colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_manual(frames = frame)

#test animatjion
#animate(rainplot)

#quality animation
animate(rainplot, nframes = max(rainframe$frame) + 1, fps=15, width=2560, height=1080, res=300)
animate(rainplot, nframes = 100, fps=15, width=2560, height=1080, res=300)

#save animation
anim_save('code_rainv6.gif', animation = last_animation(), path = '.')


ggplot(filter(rainframe, display, frame == 85), aes(x=col, y=h)) +
  geom_text(aes(label = char, colour=hsv(hue, sat, val)), size=2, show.legend = F) +
  scale_colour_identity() +
  coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
  theme(axis.title = element_text(colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

