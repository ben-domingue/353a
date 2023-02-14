
# https://rpubs.com/lijinzhang/irtrees

library(irtrees)


# Estimation: Multidimensional Item Response Model
## Data Transformation
library(mirt)
mapping <- cbind(c(0, 1, 1), c(NA, 0, 1))

head(VerbAgg3[ , -(1:2)])
VerbAgg3T_wide <- WtoW_single.tree(VerbAgg3[ , -(1:2)], mapping)
head(VerbAgg3T_wide)


## Unidimensional model
IRTree.mod1 = 'degree = 1-48'
tree.mirt1 = mirt(data = VerbAgg3T_wide,
  model = IRTree.mod1,
  itemtype = 'Rasch')
tree.mirt1
coef(tree.mirt1, simplify = TRUE)


## Multidimension model
IRTree.mod2 = 'admission = 1-24
  affirmation = 25-48
  COV = admission*affirmation'
tree.mirt2 = mirt(data = VerbAgg3T_wide,
  model = IRTree.mod2,
  itemtype = 'Rasch')
tree.mirt2
coef(tree.mirt2, simplify = TRUE)

anova(tree.mirt1, tree.mirt2)

## Save effects
d = coef(tree.mirt2, simplify = TRUE)$item[,'d']
fscores = fscores(tree.mirt2)





# Estimation: Generalized Linear Mixed Model

## Data Transformation
library(lme4)
head(VerbAgg3[ , -(1:2)])
VerbAgg3T <- dendrify(VerbAgg3[ , -(1:2)], mapping)
head(VerbAgg3T)


## Unidimensional model for linear response trees
onedim <- glmer(value ~ 0 + item:node + (1 | person),
    family = binomial, data = VerbAgg3T)


The model did not converge, find the optimization method that makes the model converge. (Note: This part is very time-consuming, you can just skip this.)

### Model Convergence Check
library(optimx)

optimx_options <- c("L-BFGS-B", "nlminb", "nlm", "bobyqa", "nmkb", "hjkb")
for(i in 1:length(optimx_options)){
  print(paste0("Testing optimx: ", optimx_options[i]))
  onedim <- glmer(value ~ 0 + item:node + (1 | person),
                    family = binomial, 
                    data = VerbAgg3T,
                    control = glmerControl(optimizer = "optimx",
                                           optCtrl = list(method = optimx_options[i])))
  if(is.null(onedim@optinfo$conv$lme4$messages)){
    print(paste0("One of the optimx options, ", optimx_options[i],", worked!"))
    print(onedim)
    break
  }
}


The 'nlimnb' optimization method was selected.

### Final Model
onedim <- glmer(value ~ 0 + item:node + (1 | person),
    family = binomial, data = VerbAgg3T,
    control = glmerControl(optimizer = "optimx",
                           optCtrl = list(method = 'nlminb')))
summary(onedim)
head(ranef(onedim)$person)


## Multidimensional model for linear response trees
twodim <- glmer(value ~ 0 + item:node + (0 + node | person),
    family = binomial, data = VerbAgg3T,
    control = glmerControl(optimizer = "optimx",
                           optCtrl = list(method = 'nlminb')))

summary(twodim)
fixef = summary(twodim)$coef[, 1, drop = FALSE]
fixef
head(ranef(twodim)$person)
ranef = ranef(twodim)$person



# Relationships between the estimates from MIRT and GLMM
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(fixef, d) # node
plot(ranef[,1], fscores[,1]) # latent trait 1
plot(ranef[,2], fscores[,2]) # latent trait 2


