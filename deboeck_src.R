### R code to accompany "The Estimation of Item Response Models with the
### lmer Function from the lme4 Package in R" by Paul De Boeck et al. (2010)

## load the lme4 package
library("lme4")

## our example data set is bundled with lme4
data("VerbAgg")

## A first example: A Rasch model with fixed item effects and random person effects
m1 = glmer(r2 ~ 0 + item + (1|id), data=VerbAgg, family="binomial")

## Item covariate models

## The regular LLTM
m2 = glmer(r2 ~ 0 + btype + mode + situ + (1|id), data=VerbAgg, family="binomial")

## The LLTM plus homoscedastic error
m3 = lmer(r2 ~ 0 + btype + mode + situ + (1|id) + (1|item), data=VerbAgg, family="binomial")

## The LLTM plus heteroscedastic error for want and do
m4 = lmer(r2 ~ 0 + btype + mode + situ + (1|id) + (0 + mode|item), 
     data=VerbAgg, family="binomial")

## The multidimensional one-parameter logistic model

m5 = lmer(r2 ~ 0 + item + (1 + btype + mode + situ|id), 
     data=VerbAgg, family="binomial")
m6 = lmer(r2 ~ 0 +  item + (0 + btype + mode + situ|id), 
     data=VerbAgg, family="binomial")
m7 = lmer(r2 ~ 0 + item + (0 + btype|id) + (0 + mode|id) + (0 + situ|id), 
     data=VerbAgg, family="binomial")
m8 = lmer(r2 ~ 0 + item + (btype:mode:situ|id), 
     data=VerbAgg, family="binomial")

## multidimensional random-weight LLTM

m9  = lmer(r2 ~ 0 + btype + mode + situ + (1 + btype + mode + situ|id), 
     data=VerbAgg, family="binomial")
m10 = lmer(r2 ~ 0 + btype + mode + situ + (0 + btype + mode + situ|id), 
     data=VerbAgg, family="binomial")
m11 = lmer(r2 ~ 0 + btype + mode + situ + (0 + btype|id) + (0 + mode|id) + (0 + situ|id), 
     data=VerbAgg, family="binomial")
m12 = lmer(r2 ~ 0 + btype + mode + situ + (btype:mode:situ|id), 
     data=VerbAgg, family="binomial")

## Person covariate models

## Four versions of the 1PL / Rasch model

## Random persons and random items (crossed random effects) 
m13 = lmer(r2 ~ 0 + (1|id) + (1|item), data=VerbAgg, family="binomial")

## Random persons and fixed items (MML) 
m14 = lmer(r2 ~ 0 + item + (1|id), data=VerbAgg, family="binomial")

## Fixed persons and random items 
m15 = lmer(r2 ~ 0 + id + (1|item), data=VerbAgg, family="binomial")

## Fixed persons and fixed items (JML) 
m16 = lmer(r2 ~ 0 + id + item + (1|item), data=VerbAgg, family="binomial")

## Model with a subpopulation (Gender) and homoscedastic error 
m17 = glmer(r2 ~ 0 + item + Anger + Gender + (1|id), data=VerbAgg, family="binomial")

## As previous, but with heteroscedasticity depending on gender
m18 = lmer(r2 ~ 0 + item + Anger + Gender + (0 + Gender|id), data=VerbAgg, family="binomial")

## A model with a differential effect of gender on the want-dimension and the do-dimension
m19 = lmer(r2 ~ 0 + btype + mode + situ + Gender:mode + Gender + (0 + mode|id), 
      data=VerbAgg, family="binomial")

## Multilevel models
## For an imaginary factor, group, assigning persons to higher level units (clusters),
## a model could be specified as
## m20 = lmer(r2 ~ 0 + + item + (1|id) + (1|group), data=VerbAgg, family="binomial")

## Person-by-item covariates

## Model-based uniform DIF model
dif = with(VerbAgg, factor(0 + (Gender=="F" & mode=="do" & btype!="shout")))
m21 = lmer(r2 ~ item + dif + Gender + (1|id), data=VerbAgg, family="binomial")

## As previous, but with DIF random across persons
m22 = lmer(r2 ~ 0 + item + dif + Gender + (dif|id), data=VerbAgg, family="binomial")

## A model for local dependency
dep = with(VerbAgg, factor((mode=="do")*(r2[mode=="want"]=="Y")))
m23 = lmer(r2 ~ 0 + item + dep + (1|id), data=VerbAgg, family="binomial")

## As previous, but allowing individual differences in the dependency
m24 = lmer(r2 ~ 0 + item + dep + (dep|id), data=VerbAgg, family="binomial")

## The dynamic 1PL model
long = data.frame(id=VerbAgg$id, item=VerbAgg$item, r2=VerbAgg$r2)
wide = reshape(long, timevar=c("item"), idvar=c("id"), dir="wide")[,-1]=="Y"
prosum = as.vector(t(apply(wide, 1, cumsum)))
m25 = lmer(r2 ~ 0 + item + prosum + (1|id), data=VerbAgg, family="binomial")

## As previous, but with individual differences in learning
m26 = lmer(r2 ~ 0 + item + prosum + (prosum|id), data=VerbAgg, family="binomial")

## Demonstrate DIF and model comparision with anova()
ModelWithoutDIF = lmer(r2 ~ 0 + item + Gender + (1|id), data=VerbAgg, family=binomial)
dif = with(VerbAgg, factor(0 + (Gender=="F"&mode=="do"&btype!="shout")))
ModelWithDIF = lmer(r2 ~ 0 + item + dif + Gender + (1|id), data=VerbAgg, family=binomial)
anova(ModelWithoutDIF, ModelWithDIF)

## Full code to reshape the LSAT data from package ltm to long shape and fit Rasch model:
library("ltm")                 # load the ltm package
data("LSAT")                   # open the LSAT data set
LSAT$person = rownames(LSAT)   # construct an ID variable
library("reshape")    
LSATlong = melt(LSAT, id = "person")  # melt to long shape
library("lme4")       
RaschModel = lmer(value ~ 0 + variable + (1|person), data=LSATlong, family="binomial")

