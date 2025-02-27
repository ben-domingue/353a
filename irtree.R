library("irtrees")
library("lme4")


####################################
## linresp examples

# define mapping matrix and prepare data for analysis
linmap <- cbind(c(0, 1, 1), c(NA, 0, 1)) 
linrespT <- dendrify(linresp, linmap)





# The multidimensional model for linear response trees
(M1linTree <- glmer(value ~ 0 + item:node + (0 + node | person), 
  family = binomial, data = linrespT))
ranef(M1linTree)

# The unidimensional model for linear response trees
(M2linTree <- glmer(value ~ 0 + item:node + (1 | person), 
  family = binomial, data = linrespT))

## # The unidimensional rating scale model for linear response trees
## (M3linTree <- glmer(value ~ 0 + item + node + (1 | person), 
##   family = binomial, data = linrespT))

## # The multidimensional random item model for linear response trees
## (M4linTree <- glmer(value ~ 1 + (0 + node | item) + (0 + node | person),
##   family = binomial, data = linrespT))


#####################################
## nesresp examples

# define mapping matrix and prepare data for analysis
nesmap <- cbind(c(0, 0, 1, 1), c(0, 1, NA, NA), c(NA, NA, 0, 1))
nesrespT <- dendrify(nesresp, nesmap)

# The nested tree model with a dimension per node
(M5nesTree <- glmer(value ~ 0 + item:node + (0 + node | person),
  family = binomial, data = nesrespT))
  
# The nested tree model with lower dimensionality
nesrespT$node2 <- factor(nesrespT$node != "node1")
nesrespT$fs <- as.numeric(nesrespT$node == "node3")
(M6nesTree <- glmer(value ~ 0 + item:node2 + fs + (0 + node2 | person),
  family = binomial, data = nesrespT))

 
#######################################
## VerbAgg3 examples

mapping <- cbind(c(0, 1, 1), c(NA, 0, 1))
VerbAgg3T <- dendrify(VerbAgg3[ , -(1:2)], mapping)

# one-dimensional
(onedim <- glmer(value ~ 0 + item:node  + (1 | person),
  family = binomial, data = VerbAgg3T))

# two-dimensional
(twodim <- glmer(value ~ 0 + item:node + (0 + node | person),
  family = binomial, data = VerbAgg3T))


#######################################
## fsdatT examples

load("fsdatT.rda")

mapping <- cbind(c(0, 0, 1, 1), c(0, 1, NA, NA), c(NA, NA, 0, 1))
fsdatT$node2 <- factor(fsdatT$node != "node1")
fsdatT$fs <- as.numeric(fsdatT$node == "node3")

(doublediff <- glmer(value ~ 0 + item:node + (0 + node | person),
  family = binomial, data = fsdatT))

(diffdiff <- glmer(value ~ 0 + item:node + (0 + node2 | person),
  family = binomial, data = fsdatT))

(abildiff <- glmer(value ~ 0 + item:node2 + fs + (0 + node | person),
  family = binomial, data = fsdatT))

(nodiff <- glmer(value ~ 0 + item:node2 + fs + (0 + node2 | person),
  family = binomial, data = fsdatT))


#####################################
## latlin examples

lmm <- cbind(c(1, 0, 0), c(1, 1, 0), c(0, 1, 0), c(0, 1, 1), c(0, 0, 1))
linlatT <- exogenize(linlat, lmm, 
   endnode = rep(1:3, each = 10), crossitem = rep(1:10, 3))

# The free-covariance integrative model: 
(freecov <- glmer(value ~ 0 + crossitem + endnode + 
  (0 + exo1 + exo3 + exo5 | person), family = binomial, data = linlatT))

# The free MA(1) model
(freeMA1 <- glmer(value ~ 0 + crossitem + endnode + (0 + exo1 | person) +
   (0 + exo2 | person) + (0 + exo3 | person) + (0 + exo4 | person) + 
   (0 + exo5 | person), family = binomial, data = linlatT))

# The linear growth model with free MA(1) component
linlatT$time <- as.numeric(linlatT$endnode) - 1
(linGfreeMA1 <- glmer(value ~ 0 + crossitem + time + (time | person) + 
  (0 + exo1 | person) + (0 + exo2 | person) + (0 + exo3 | person) + 
  (0 + exo4 | person) + (0 + exo5 | person), 
  family = binomial, data = linlatT))


#####################################
## neslat examples

nmm <- cbind(c(1, 1, 1), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
neslatT <- exogenize(neslat, nmm, endnode = rep(1:3, each = 10))

# The bi-factor model
(bifactor <- glmer(value ~ 0 + item + (0 + exo1 | person) + 
  (0 + exo2 | person) + (0 + exo3|  person) + 
  (0 + exo4 | person), family = binomial, data = neslatT))


#####################################
## stressT examples

load("stressT.rda")

(linearplusres <- glmer(value ~ 0 + crossitem + time + (time | person) + 
  (0 + exo1 | person) + (0 + exo3 | person) + (0 + exo5 | person),
  family = binomial, data = stressT))

(linearplusMA1 <- glmer(value ~ 0 + crossitem + time + (time | person) + 
  (0 + exo1 | person) + (0 + exo2 | person) + (0 + exo3 | person) + 
  (0 + exo4 | person) + (0 + exo5 | person), family = binomial,
  data = stressT))


#######################################
## VerbAgg2 examples

map <- cbind(c(1, 1, 1), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
VerbAgg2T <- exogenize(VerbAgg2[, -(1:2)], map, endnode = rep(1:3, 8))
VerbAgg2T$value <- factor(VerbAgg2T$value)

(onedim <- glmer(value ~ 0 + item + (1 | person),
  family = binomial, data = VerbAgg2T))

(threedim <- glmer(value ~ 0 + item + (0 + exo2 + exo3 + exo4 | person),
  family = binomial, data = VerbAgg2T))

(bifactor <- glmer(value ~ 0 + item + (0 + exo1 | person) +
  (0 + exo2 | person) + (0 + exo3 | person) + (0 + exo4 | person),
  family = binomial, data = VerbAgg2T))

