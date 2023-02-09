library(mirt)

set.seed(31337)
resp <- expand.grid(item_1 = 0:2,
                    item_2 = 0:2,
                    item_3 = 0:2,
                    item_4 = 0:2,
                    item_5 = 0:2)

d <- resp
d$sum_score = rowSums(resp)

params <- data.frame(a = 1,
                     b1 = runif(5, 0, 2),
                     b2 = runif(5, -2, 0)) |> as.matrix() 

sim_resp <- simdata(params[,1], params[,2:3], N=1000, itemtype='graded')

spec = mirt.model(paste0("F = 1-5 
                          FIXED = (1-5, a1)
                          START = (1-5, a1, 1.0)"))

###########
m <- mirt(sim_resp, spec, itemtype='graded',)
d$theta <- fscores(m, response.pattern=resp)[,1]
plot(d$sum_score, d$theta)

############
sim_resp <- simdata(params[,1], params[,2:3], N=1000, itemtype='gpcm')
m <- mirt(sim_resp, spec, itemtype='Rasch',)
plot(rowSums(sim_resp),fscores(m))
