library(dplyr)
library(ggplot2)
library(gganimate)
library(purrr)

code_frame <- data.frame(col=5, height=seq(55, -5, length.out=50), char=sample(katakana, 1), frame=1:50)

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


ggplot(code_frame, aes(x=col, y=height)) +
  geom_text(aes(label = char), colour=hsv(0.5, 0.8, 0.8)) +
  coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
  theme(plot.background = element_rect(fill = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_states(states = frame, transition_length = 0.5, state_length = 0)


#active columns

#column activator
#random char changes
#leading chars (white colour)
#base colour value for column darkness

total_col <- 100
total_col_padded <- round(total_col * 1.1, digits=0)
active_col_perc <- 0.7
num_initial_col <- round(total_col_padded * active_col_perc, digits=0)
col_min <- round(1 - total_col * 0.05, digits=0)
col_max <- round(total_col + total_col * 0.05, digits=0)

total_row <- 50
start_pos_min <- total_row * 1.1
start_pos_max <- (total_row * 1.1) + 10
row_bot_cutoff <- round(0 - total_row * 0.05, digits=0)

lifespan_min = round(total_row * 0.6)
lifespan_max = round(total_row * 1.3)

fade_sequence_length = 10

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

val_aging <- function(age, lifespan, base_val, val)
{
  return(pmax(val - base_val/fade_sequence_length, 0))
}


#Initialize code rain data frame
rainframe <- data.frame(frame = 0,
                        col = sample(col_min:col_max, num_initial_col), 
                        h = sample(start_pos_min:start_pos_max, num_initial_col, replace = T), 
                        char = sample(katakana, num_initial_col, replace = T), 
                        leading = T,
                        speed = ifelse(sample(runif(num_initial_col, 0, 1), num_initial_col) > 0.95, 2, 1),
                        age = 1,
                        lifespan = sample(lifespan_min:lifespan_max, num_initial_col, replace=T),
                        expired = F,
                        base_val = replicate(num_initial_col, gen_base_val()), 
                        hue = 0.5, 
                        sat = 1) %>%
  mutate(val = base_val)


for(i in 1:1000)
{
  
  #mark rain elements that have faded, moved off-screen, or otherwise expired as EXPIRED
  rainframe <- rainframe %>%
    mutate(expired = ifelse(h < row_bot_cutoff | age == lifespan | val < 0.01, T, F))
  
  #Check num active columns, initialize new columns if below threshold
  active_col <- rainframe %>%
    filter(h > 0.15*total_row, frame == i-1, !expired) %>%
    distinct(col) %>%
    .$col
  
  avail_col <- (col_min:col_max)[! col_min:col_max %in% active_col]
  
  #randomly select new columns to activate if total is below threshold
  if(length(active_col)/total_col_padded <= 0.7)
  {
    new_col_activations <- sample(avail_col, sample(1:length(avail_col), 1))
    
    #generate data rows for new columns
    new_cols <- data.frame(frame = i,
                           col = new_col_activations, 
                           h = sample(start_pos_min:start_pos_max, length(new_col_activations), replace = T), 
                           char = sample(katakana, length(new_col_activations), replace = T), 
                           leading = T,
                           speed = ifelse(sample(runif(length(new_col_activations), 0, 1), length(new_col_activations)) > 0.95, 2, 1),
                           age = 1,
                           lifespan = sample(lifespan_min:lifespan_max, length(new_col_activations), replace=T),
                           expired = F,
                           base_val = replicate(length(new_col_activations), gen_base_val()), 
                           hue = 0.5, 
                           sat = 1) %>%
      mutate(val = base_val)
    
    #append to overall rainframe
    rainframe <- rainframe %>%
      rbind(new_cols)
    
  }else
  {

  }
  
  #persist trailing chars from previous frame
  still_trailing <- rainframe %>%
    filter(frame == i-1, !leading, !expired) %>%
    mutate(frame = i,
           age = age+speed,
           sat = sat_aging(age))
  
  #append to overall rainframe
  rainframe <- rainframe %>%
    rbind(still_trailing)
  
  #simulate 'fall' - move leading chars down by speed variable
  num_leading_chars <- rainframe %>%
    filter(leading == T, frame == i-1, !expired) %>%
    nrow()
    
  leading_chars <- rainframe %>%
    filter(leading == T, frame == i-1, !expired) %>%
    mutate(frame = i,
           h = h - speed,
           #char = ifelse(runif(length(active_col), 0, 1) > 0.8, sample(katakana, length(active_col), replace = T), as.character(char)),
           char = sample(katakana, num_leading_chars, replace = T),
           age = age+speed,
           sat = 0)
    
  #append to overall rainframe
  rainframe <- rainframe %>%
    rbind(leading_chars)
  
  
  #generate trailing chars in wake of leading chars
  max_speed <- rainframe %>%
    filter(leading == T & frame == i-1 & !expired) %>%
    .$speed %>%
    max()
  
  for(j in 1:max_speed)
  {
    num_trailing <- rainframe %>%
      filter(leading == T, frame == i-1, !expired) %>%
      nrow()
    
    trailing_chars <- rainframe %>%
      filter(leading == T, frame == i-1, !expired) %>%
      mutate(frame = i,
             h = h - j + 1,
             char = ifelse(j > 1, sample(katakana, length(active_col), replace = T), as.character(char)),
             leading = F,
             age = 1,
             sat = sat_aging(age))
    
    #append to rainframe
    rainframe <- rainframe %>%
      rbind(trailing_chars)
  }
  
  #fade chars approaching lifespan age
  rainframe <- rainframe %>%
    mutate(val = ifelse(!leading & frame == i & !expired & lifespan - age < fade_sequence_length, val_aging(age, lifespan, base_val, val), val))
}


rainplot <- ggplot(rainframe, aes(x=col, y=h)) +
  geom_text(aes(label = char, colour=hsv(hue, sat, val)), size=1.5, show.legend = F) +
  scale_colour_identity() +
  coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
  theme(plot.background = element_rect(fill = "black"),
        axis.title = element_text(colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_manual(frames = frame)

#test animatjion
animate(rainplot)

#quality animation
animate(rainplot, nframes = 1001, fps=20, width=1920, height=1080, res=300)

#save animation
anim_save('code_rainv3.gif', animation = last_animation(), path = '.')

gganimate(rainplot, nframes=500, ani.width=1920, ani.height=1080, ani.res=2000)


rainplot_test <- ggplot(filter(rainframe, frame < 40), aes(x=col, y=h)) +
  geom_text(aes(label = char, colour=hsv(hue, sat, val)), size=1.5, show.legend = F) +
  scale_colour_identity() +
  coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
  theme(plot.background = element_rect(fill = "black"),
        axis.title = element_text(colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_manual(frames = frame)

animate(rainplot_test, fps=20, width=1920, height=1080, res=300)


for(i in 45:75)
{
  ggplot(filter(rainframe, frame == i), aes(x=col, y=h)) +
    geom_text(aes(label = char, colour=hsv(hue, sat, val)), size=3, show.legend = F) +
    scale_colour_identity() +
    coord_cartesian(xlim=c(1,100), ylim=c(0,50)) +
    theme(plot.background = element_rect(fill = "black"),
          axis.title = element_text(colour = 'white'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(filename = paste0(i, '.png'), device = 'png', path = 'man_saved_images/', width = 16, height = 9, dpi = 300)
}

