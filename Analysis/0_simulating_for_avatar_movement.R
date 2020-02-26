#### Moving truck ####

# packages 
library(tidyverse)

# some constants 
# screen x res 
x_res <- 1920

# first make sigmoid function
delta <- seq(1, x_res/2, 1)
a <- (15/max(delta))*-1
c <- max(delta)/2
success_rate <- 1/(1+exp(-a*(delta-c)))
plot(delta, success_rate, type = "l")

# tidy
rm(a,c, delta)

# make list of deltas 
delta_list = seq(100, x_res/2, 100)

# get a distance to travel
#### Testing ideas: #### 
#### Idea 1 ####
# using the sigmoid funtion as to whether the truck moves on or not 
# make data frame
inc_frame <- data.frame(iter = numeric(),
                         delta = numeric(),
                         chance = numeric(),
                         max_value = numeric())

# loop through this 
for(d in unique(delta_list)){
  for(i in 1:1000){
    for(iter in 1:d){
      # get chance of success for that step
      chance = success_rate[iter] 
      
      # produce 1 or 0 with chance probabilty 
      move_on = rbinom(1,1,chance)
      
      # do we move on?
      if(move_on == 0 | iter == d){
        max_value = iter
        break
      }
    }
    chance = success_rate[d]
    inc_frame <- rbind(inc_frame, data.frame(iter = i,
                                               delta = d,
                                               chance = chance,
                                               max_value = max_value))
  }
}

# tidy
rm(chance, d, i, iter, max_value, move_on)

# plot this to check dist for each delta 
inc_frame %>% ggplot(aes(max_value)) +
  geom_density() + 
  facet_wrap(~delta)

# check the numbers 
inc_frame$est_cha <- 0
inc_frame$est_cha[inc_frame$max_value == inc_frame$delta] <- 1

# now compare 
inc_frame %>% 
  group_by(delta, chance) %>%
  summarise(est_cha = mean(est_cha))

# NB: Doesn't look like idea 1 works very well...
# both the plot and the table confirm that this means the truck is very unlikely
# to reach the destination with the right amount of chance...

#### Idea 2 ####
# try the same as above... but backwards?
# count down from the end value

# make data frame
dec_frame <- data.frame(iter = numeric(),
                          delta = numeric(),
                          chance = numeric(),
                          min_value = numeric())

# loop through this 
for(d in unique(delta_list)){
  for(i in 1:1000){
    for(iter in d:1){
      # get chance of success for that step
      chance = success_rate[iter] 
      
      # produce 1 or 0 with chance probabilty 
      stop = rbinom(1,1,chance)
      
      # do we move on?
      if(stop == 1 | iter == 1){
        min_value = iter
        break
      }
    }
    chance = success_rate[d]
    dec_frame <- rbind(dec_frame, data.frame(iter = i,
                                                 delta = d,
                                                 chance = chance,
                                                 min_value = min_value))
  }
}

# tidy
rm(chance, d, i, iter, min_value, stop)

# plot
dec_frame$delta <- as.factor(dec_frame$delta)
dec_frame %>%
  ggplot(aes(min_value)) + 
  geom_density() + 
  facet_wrap(~delta)

# summary
dec_frame$est_cha <- 0 
dec_frame$est_cha[dec_frame$delta == dec_frame$min_value] <- 1

dec_frame %>%
  group_by(delta, chance) %>%
  summarise(est_cha = mean(est_cha),
            min_val = min(min_value),
            max_val = max(min_value))

# This looks like it works a bit better?
# plot doesn't help much... but the summary looks good
# Thought it does mean that values close to the target will 
# always be quite likely


#### Idea 3 ####
# The previous ones have been based on the distance to travel, but...
# We could give the avatar a variable speed?
# So draw it's speed from a binomial distribution so on average it 
# will cover x distance with sig variation?
# might be more complicated to make work if we want to keep it so that
# the distances have some chance of success?
# Would need to make sure the timing was all fine as well...

# So, for this one we just need the screen resolution (x_res)
# and some way to define success rates...
# making a normal distribution to set speed
mu <- max(delta_list)*0.003
sigma <- max(delta_list)*0.001
N <- seq(-20, 20, 0.1)

a <- dnorm(N, mu, sigma)
plot(N, a, type = "l")

# looks reasonable, so can we sample from that 
# generate list of distances from that dist 
# probably a nicer way to do this to avoid it having negative speed
ppt <- abs(rnorm(1, mu, sigma))

# could use the sigmoid function... but that could get complicated...

# need a way to time each trial... for now, just num_steps
num_steps = 100

# temp frame 
dist_frame <- data.frame(iter = numeric(),
                         delta = character(),
                         # chance = numeric(),
                         end_dist = numeric(),
                         success = numeric())

# try to loop for now 
for(d in delta_list){
  for(iter in 1:1000){
    # get psuedo random setp_size
    step_size = abs(rnorm(1, mu, sigma))
    # step_size = mu + step_size
    
    # get total dist
    end_dist = step_size * num_steps
    
    # check this is greater than curdelta 
    if(end_dist > d){
      success = 1
      end_dist = d
    } else {
      success = 0
    }
    
    
    
    # what are the odds?
    # chance = a[d]
    
    # make data.frame 
    dist_frame <- rbind(dist_frame, data.frame(iter = iter,
                                               delta = d, 
                                               # chance = chance,
                                               end_dist = end_dist,
                                               success = success))
  }
}

# tidy 
rm(a, b, chance, d, distance, end_dist, iter, N, num_steps, ppt, step_size, success)

# check this
dist_frame %>% 
  group_by(delta) %>%
  summarise(success = mean(success))

# make a plot to see how often they were successful?
dist_frame %>%
  mutate(norm_dist = end_dist/delta) %>%
  ggplot(aes(norm_dist)) +
  geom_histogram() + 
  facet_wrap(~delta)

# This is close, but not quite what I want...
# try another way 

#### Idea 4 ####
# similar to above, but using another way to calculate chance 
# of reaching the end 
# could probably use a beta distribution and just define the chance
# based on distance?
x <- seq(0,1,0.01)
y <- dbeta(x,3,2)
plot(x,y)

# looks okayish... could be used to give a percentage of max speed?
# and we have control over the shape of the distribution


# try dlnorm instead... if we can figure it out 
x <- seq(0,10,0.01)
y <- dlnorm(x, 2, 2)
plot(x,y)


# back to dnorm but get this for a percentage of avg_spd 
avg_spd <- 10
x <- seq(0,2,0.1)
mu <- 1
sigma <- 0.15
y <- dnorm(x, mu, sigma)
num_steps = 100
plot(x*avg_spd*num_steps,y)

# now loop again using avg_speed


# temp frame 
dist_frame2 <- data.frame(iter = numeric(),
                          delta = character(),
                          # chance = numeric(),
                          end_dist = numeric(),
                          success = numeric())

# try to loop for now 
for(d in delta_list){
  for(iter in 1:1000){
    # get psuedo random setp_size
    step_size = round(rnorm(1,mu, sigma)*avg_spd, digits = 0)
    # step_size = mu + step_size
    
    # get total dist
    end_dist = step_size * num_steps
    
    # check this is greater than curdelta 
    if(end_dist > d){
      success = 1
      end_dist = d
    } else {
      success = 0
    }
    
    
    
    # what are the odds?
    # chance = a[d]
    
    # make data.frame 
    dist_frame2 <- rbind(dist_frame2, data.frame(iter = iter,
                                                 delta = d, 
                                                 # chance = chance,
                                                 end_dist = end_dist,
                                                 success = success))
  }
}

# tidy 
rm(d, iter, success, end_dist, step_size, num_steps, mu, sigma, x, y, avg_spd)

# check this
dist_frame2 %>% 
  group_by(delta) %>%
  summarise(success = mean(success))

# plot  
dist_frame2 %>%
  mutate(norm_dist = end_dist/delta) %>%
  ggplot(aes(norm_dist)) + 
  geom_density() + 
  facet_wrap(~delta)

# make curve plot 
dist_frame2 %>%
  group_by(delta) %>%
  summarise(success = mean(success)) %>%
  ggplot(aes(delta, success)) + 
  geom_line()

#### Idea 5 ####
# just make a normal distribution about the mid point of the range we'll be 
# testing in, and make it sort of wide?
# This is still travel distance as oppossed to speed though
x <- seq(min(delta_list), max(delta_list), 10)
mu <- median(x)
sigma <- 150
y <- dnorm(x, mu, sigma)
plot(x,y)

# setup data frame
norm_dist_frame <- data.frame(iter = numeric(),
                              delta = character(),
                              dist_travelled = numeric(),
                              success = numeric())

# loop through using this idea
for(d in delta_list) {
  for(iter in 1:1000) {
    # get distance travelled
    dist_travelled = round(rnorm(1,mu,sigma),digits = 0)
    
    # success?
    if(dist_travelled > d){
      dist_travelled <- d
      success <- 1
    } else {
      success <- 0
    }
    norm_dist_frame <- rbind(norm_dist_frame, data.frame(iter = iter,
                                                         delta = d,
                                                         dist_travelled = dist_travelled,
                                                         success = success))
  }
}

# tidy 
rm(d, iter, dist_travelled, success)

# plot raw distance
norm_dist_frame %>%
  ggplot(aes(dist_travelled)) + 
  stat_density() + 
  facet_wrap(~delta)

# normalised distance plot 
norm_dist_frame %>%
  mutate(norm_dist = dist_travelled / delta) %>%
  ggplot(aes(norm_dist)) + 
  geom_histogram() + 
  facet_wrap(~delta)

# get a summary 
norm_dist_frame %>%
  group_by(delta) %>%
  summarise(success = mean(success)) %>%
  # make nice plot
  ggplot(aes(delta,success)) +
  geom_line()

# tidy 
rm(mu, sigma, x, y, success_rate)

#### Idea 6 ####
# Try now using the beta distribution with a maximum speed instead of average?
# set max speed  
travel_time <- 100 # for the sake of this experiment, t is constant
max_speed <- max(delta_list)/travel_time

# define beta distribution 
x <- seq(0,1,0.01)
beta_1 <- 30
beta_2 <- 30
# beta formula is a/(a + b)
# where a is number of successes and b is number of failures
# larger numbers makes this more certain
y <- dbeta(x, beta_1, beta_2)
plot(x*max_speed,y)

# setup data frame 
beta_frame <- data.frame(iter = numeric(),
                         delta = numeric(),
                         end_dist = numeric(),
                         success = numeric())

# sim experiment results 
for(d in delta_list){
  for(i in 1:1000){
    # set speed for this trial 
    trial_speed <- rbeta(1,beta_1,beta_2)*max_speed
    
    # end distance
    end_dist <- trial_speed*travel_time
    
    # get success or not
    if(end_dist > d){
      success <- 1
      end_dist <- d
    } else {
      success <- 0
    }
    
    # add to data frame 
    beta_frame <- rbind(beta_frame, data.frame(iter = i,
                                               delta = d,
                                               end_dist = end_dist,
                                               success = success))
  }
}

# tidy 
rm(beta_1,beta_2,d,end_dist,i,max_speed,travel_time,trial_speed,x,y)

# look at data 
beta_frame %>%
  group_by(delta) %>%
  summarise(success = mean(success)) %>%
  ggplot(aes(delta, success)) + geom_line()

# plot norm_dist 
beta_frame %>% 
  mutate(norm_dist = end_dist/delta) %>%
  ggplot(aes(norm_dist)) +
  geom_histogram() + 
  facet_wrap(~delta)


#### Idea 6.1 ####
# doing the same as the above but this time with discrete speed values since we 
# will need to use integers for this...
travel_time <- 100 # for the sake of this experiment, t is constant
max_speed <- (max(delta_list)/travel_time) - 1

# define beta distribution 
x <- seq(0,1,0.01)
beta_1 <- 10
beta_2 <- 10
# beta formula is a/(a + b)
# where a is number of successes and b is number of failures
# larger numbers makes this more certain
y <- dbeta(x, beta_1, beta_2)
plot(x*max_speed,y)

# setup data frame 
beta_frame_2 <- data.frame(iter = numeric(),
                           delta = numeric(),
                           speed = numeric(),
                           end_dist = numeric(),
                           success = numeric())

for(delta in delta_list){
  for(iter in 1:1000){
    # first get a speed
    speed <- round(rbeta(1, beta_1, beta_2)*max_speed) + 1
    
    # get dist travelled 
    end_dist <- speed*travel_time
    
    success <- 0
    if(end_dist >= delta){
      success <- 1
      end_dist <- delta
    }
    
    # add to dataframe 
    beta_frame_2 <- rbind(beta_frame_2, data.frame(iter = iter,
                                                   delta = delta,
                                                   speed = speed,
                                                   end_dist = end_dist,
                                                   success = success))
    
  }
}

# check data 
beta_frame_2 %>%
  group_by(delta) %>%
  summarise(success = mean(success)) %>%
  ggplot(aes(delta,success)) + geom_line() + 
  scale_x_continuous(breaks = c(0,200,400,600,800))

# plot 
beta_frame_2 %>%
  mutate(norm_dist = end_dist/delta) %>%
  ggplot(aes(norm_dist)) + 
  geom_histogram() + 
  facet_wrap(~delta)



#### Check different distributions ####
# Decided to use idea 6.1 for this experiment
# Now to check for appropriate distributions 

# set paramaters for setting speed
travel_time <- 100 
max_speed <- (max(delta_list)/travel_time) - 1

# different distribution combinations 
betas <- data.frame(skew = c("left", "centre", "right"),
                    beta1 = c(3, 3, 7),
                    beta2 = c(7, 3, 3))
                    
spread_settings <- c(1, 10)

# setup data frame for loop 
beta_frame <- data.frame(iter = numeric(),
                         skew = character(),
                         spread = numeric(),
                         delta = numeric(),
                         end_dist = numeric(),
                         success = numeric())

# now loop
for(skew in unique(betas$skew)){
  # temp frame
  temp <- betas[betas$skew == skew,]

  # now loop through sizes
  for(spreads in unique(spread_settings)){
    beta1 <- temp$beta1 * spreads
    beta2 <- temp$beta2 * spreads
    
    # loop through dists
    for(dist in unique(delta_list)){
      for(iter in 1:1000){
        # set speed 
        end_dist <- (round(rbeta(1,beta1,beta2)*max_speed) + 1) * travel_time
        
        # set success 
        success <- 0
        # check 
        if(end_dist >= dist){
          success <- 1
          end_dist <- dist
        }
        
        # add to data frame 
        beta_frame <- rbind(beta_frame, data.frame(iter = iter,
                                                   skew = skew,
                                                   spread = spreads,
                                                   delta = dist,
                                                   end_dist = end_dist,
                                                   success = success))
      }
    }
  }
}

# tidy 
rm(beta1,beta2,dist,end_dist,iter,skew,spreads,success,temp)

# plot these with different lines for skew and spread 
beta_frame %>%
  mutate(spread = as.factor(spread)) %>%
  group_by(skew, spread, delta) %>%
  mutate(avg_success = mean(success)) %>%
  ggplot(aes(delta, avg_success, colour = skew)) +
  geom_point() + 
  stat_smooth(method = glm,
              method.args = list(family = "binomial"),
              aes(y = success),
              se = F) +
  scale_x_continuous(breaks = seq(100,900,200)) +
  facet_wrap(~spread)

# histograms 
beta_frame %>%
  mutate(spread = as.factor(spread),
         norm_dist = end_dist/delta) %>%
  ggplot(aes(norm_dist, fill = spread)) +
  geom_histogram(position = "dodge", binwidth = 0.075) + 
  scale_x_continuous(breaks = c(0.25, 0.75)) + 
  facet_grid(skew ~ delta)
  

# just plotting things 
distr_plots <- data.frame(skew = character(),
                          spread = character(),
                          x = numeric(),
                          y = numeric())

# set x 
x <- seq(0,1,0.01)
for(skew in unique(betas$skew)){
  temp <- betas[betas$skew == skew,]
  # now spread
  for(spread in unique(spread_settings)){
    y <- dbeta(x, temp$beta1*spread, temp$beta2*spread)
    # add to frame
    distr_plots <- rbind(distr_plots, data.frame(skew = skew,
                                                 spread = spread,
                                                 x = x,
                                                 y = y))
  }
}

# make plots of this 
distr_plots %>%
  mutate(spread = as.factor(spread)) %>%
  ggplot(aes(x*max_speed + 1,y, colour = skew)) +
  geom_line() + 
  scale_x_continuous(breaks = c(2,4,6,8)) +
  facet_wrap(~spread)

#### Final idea ####
# clear everything from before 
rm(list = ls()) 

# set some constants
travel_time <- 100
betas <- data.frame(skew = c("rand_uniform", "hard_cutoff", "broad"),
                    beta1 = c(1,10000, 10),
                    beta2 = c(1,10000, 10))

# setup data frame for loop
beta_frame <- data.frame(skew = character(),
                         Delta = numeric(),
                         estimate = numeric())

# make same loop as above
for(skew in unique(betas$skew)){
  # temp frame
  temp <- betas[betas$skew == skew,]

  # sim
  y <- (round(rbeta(1000000, temp$beta1, temp$beta2)*8)+1)*travel_time
  # data frame
  delta_frame <- data.frame(skew = skew,
                            Delta = seq(100,900,100))

  delta_frame <- delta_frame %>%
    group_by(skew, Delta) %>%
    mutate(estimate = sum(y >= Delta)/length(y))

  # bind to frame
  beta_frame <- rbind(beta_frame, as.data.frame(delta_frame))
}

# tidy
rm(betas, skew, y, temp)

# now make plot
plt <- beta_frame %>%
  ggplot(aes(Delta, estimate, colour = skew)) +
  geom_point() +
  stat_smooth(method = glm,
              method.args = list(family = "binomial"),
              aes(y = estimate),
              se = F) +
  scale_x_continuous(breaks = seq(100,900,100)) + 
  see::scale_color_flat() 
plt$labels$x = "Delta (Pixels)"
plt$labels$y = "Estimated success rate"
plt$labels$colour = "Shape"
plt

