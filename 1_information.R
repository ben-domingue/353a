library(ggplot2)
library(tidyr)
library(dplyr)
library(entropy)
library(mirt)

################################################################
##first, we're going to simulate some data

##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-30
np<-2000
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
##now the item level part.
##this is going to look like logistic regression, meaning we will have a slope and an intercept
a<-rep(1,ni)
a<-exp(rnorm(ni,sd=.3))
b<-rnorm(ni)
a.mat<-matrix(rep(a,np),np,ni,byrow=TRUE)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're going to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(a.mat*(th.mat+b.mat)) #note this is pairwise multiplication not matrix multiplication.
##also, note that i am treating b.mat as item easiness params to make life simpler with mirt output

##we can simulate data using those probabilities
resp<-pr
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])


##so let's estimate parameters 
mod<-mirt(data.frame(resp),1,itemtype="2PL")

##################################################
##Item Information
##get a theta distribution
th.seq<-seq(-5,5,length.out=1000)

## fisher info
n.select = 5
## Compute information for first 5 items
info_list <- lapply(1:n.select, function(i) {
  data.frame(
    theta = th.seq,
    info = iteminfo(extract.item(mod, i), th.seq),
    item = paste0("Item ", i)
  )
})

## Combine the data for all items into a single data frame
df.fisher.info <- as.data.frame(bind_rows(info_list)) %>% 
  mutate(type = "fisher")


## Plot fisher info per item
plot1 <- ggplot(df.fisher.info , aes(x = theta, y = info, color = item)) +
  geom_line(size = 1) +
  labs(title = "Fisher Information per item",
       x = expression(theta), y = "Item information") +
  guides(color = guide_legend(title = "Items"))

plot1

##KL divergence 
##KL divergence for a single item
kl_item <- function(th0, th, item_params) {
  # Compute probabilities for correct responses
  P_th0 <- probtrace(th0, item_params)
  P_th <- probtrace(th, item_params)
  
  # Compute KL divergence
  KL <- sum(P_th0 * log(P_th0 / P_th), na.rm = TRUE)
  return(KL)
}

## Helper to get item probabilities
probtrace <- function(th, item_params) {
  a <- item_params[1]
  b <- item_params[2]
  exp_term <- exp(a * (th - b))
  P <- exp_term / (1 + exp_term)
  return(c((1-P), P))
}

## Extract item parameters
item_params <- coef(mod, simplify = TRUE)$items

## Define ground true theta (note: in practice, th0 is unknown)
th0 <- 0  # This could be adjusted based on prior knowledge about the true distribution of theta

## Compute KL information for first 5 items
kl_matrix <- sapply(1:n.select, function(item) {
  sapply(th.seq, function(th) {
    kl_item(th0, th, item_params[item, ])
  })
})

df.kl.info <- data.frame(
  theta = rep(th.seq, each =n.select),
  info= as.vector(t(kl_matrix)),
  item = rep(paste0("Item ", 1:n.select), length(th.seq))
) %>% 
  mutate(type = "KL")

plot2 <- ggplot(df.kl.info, aes(x = theta, y = info, color = item)) +
  geom_line(size = 1) +
  labs(title = "KL Information per item", x = expression(theta), y = "Item Information")

plot2

## Compare fisher vs. KL information
df.combined <- df.kl.info %>% 
  rbind(df.fisher.info)

plot3 <- ggplot(df.combined, aes(x = theta, y = info, color = item, linetype = type)) +
  geom_line(size = 1) +
  labs(title = "Item information", x = expression(theta), y = "Information")

plot3

##################################################
## Test Information

## compute fisher test info for all items
test.information = testinfo(mod, th.seq)
df.testinfo.fisher <- tibble(theta = th.seq, testinfo = test.information) %>% 
  mutate(type = "fisher test")
  
## computer kl test info for all items 
compute_kl_info_test <- function(th.seq, item_params, kl_item) {
  n.items <- nrow(item_params)  # Number of items
  
  # Compute KL information for all items
  kl_matrix <- sapply(1:ni, function(item) {
    sapply(th.seq, function(th) {
      kl_item(th0, th, item_params[item, ])
    })
  })
  
  # Aggregate KL information for the entire test at each theta
  test_kl_info <- rowSums(kl_matrix)  # Sum across items
  
  # Create a dataframe for the aggregated test KL information
  df.test.kl.info <- data.frame(
    theta = th.seq,
    testinfo = test_kl_info
  ) %>%
    mutate(type = "KL test")
  
  return(df.test.kl.info)
}

df.testinfo.kl <- compute_kl_info_test(th.seq, item_params, kl_item)

## plot test information
df.combined.test.info <- df.testinfo.kl %>% rbind(df.testinfo.fisher)

plot4 <- ggplot(df.combined.test.info, aes(x = theta, y = testinfo, linetype = type)) +
  geom_line(size = 1) +
  labs(title = "Test Information",
       x = expression(theta), 
       y = "Item information") 

plot4
