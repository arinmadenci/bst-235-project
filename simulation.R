# setup
source("estimator-functions.R")

    # Plug-in
set.seed(1)
plugin.Sigma.known.sample <- list()
plugin.Sigma.known.sample$n10000d10 <-pbreplicate(300, plugin.Sigma.known.fun(n=10000, d=10)) # 300 reps, 13s
plugin.Sigma.known.sample$n10000d50 <- pbreplicate(300, plugin.Sigma.known.fun(n=10000, d=50)) # 300 reps, 44s
plugin.Sigma.known.sample$n10000d500 <- pbreplicate(300, plugin.Sigma.known.fun(n=10000, d=500)) # 300 reps, 38 minutes
plugin.Sigma.known.sample$n1000d10 <- pbreplicate(300, plugin.Sigma.known.fun(n=1000, d=10))
plugin.Sigma.known.sample$n1000d50 <- pbreplicate(300, plugin.Sigma.known.fun(n=1000, d=50))
plugin.Sigma.known.sample$n1000d500 <- pbreplicate(300, plugin.Sigma.known.fun(n=1000, d=500)) # 300 reps, 5 minutes
plugin.Sigma.known.sample$n1000d1200 <- pbreplicate(300, plugin.Sigma.known.fun(n=1000, d=1200, mp=1)) # does not work
save(plugin.Sigma.known.sample, file=here("files","plugin-Sigma-known-sample.Rda"))

    # Dicker (sigma known)
set.seed(1)
dicker.Sigma.known.sample <- list()
dicker.Sigma.known.sample$n10000d10 <-pbreplicate(300, dicker.Sigma.known.fun(n=10000, d=10)) # 300 reps, 5s 
dicker.Sigma.known.sample$n10000d50 <- pbreplicate(300, dicker.Sigma.known.fun(n=10000, d=50)) # 300 reps, 24s
dicker.Sigma.known.sample$n10000d500 <- pbreplicate(300, dicker.Sigma.known.fun(n=10000, d=500)) # 300 reps, 15 minutes
dicker.Sigma.known.sample$n1000d10 <- pbreplicate(300, dicker.Sigma.known.fun(n=1000, d=10))
dicker.Sigma.known.sample$n1000d50 <- pbreplicate(300, dicker.Sigma.known.fun(n=1000, d=50))
dicker.Sigma.known.sample$n1000d500 <- pbreplicate(300, dicker.Sigma.known.fun(n=1000, d=500)) # 300 reps, 2 minutes
dicker.Sigma.known.sample$n1000d1200 <- pbreplicate(100, dicker.Sigma.known.fun(n=1000, d=1200)) # 100 reps, 7 minutes [] could run at 300 reps*
save(dicker.Sigma.known.sample, file=here("files","dicker-Sigma-known-sample.Rda"))

# Eigenprism (sigma known)
set.seed(1)
eigenprism.Sigma.known.sample <- list()
eigenprism.Sigma.known.sample$n1000d1200_tau <- pbreplicate(100, eigenprism.Sigma.known_sigma.fun(n=1000, d=1200, estimator="beta2")) # 100 reps, 2 hrs
eigenprism.Sigma.known.sample$n1000d1200_sigma <- pbreplicate(100, eigenprism.Sigma.known_sigma.fun(n=1000, d=1200)) # 100 reps, 2.5 hrs
save(eigenprism.Sigma.known.sample, file=here("files","eigenprism-Sigma-known-sample.Rda"))

    # Dicker (sigma unknown: sample)
set.seed(1)
dicker.Sigma.unknown_sample.sample <- list()
dicker.Sigma.unknown_sample.sample$n10000d10 <-pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=10000, d=10)) # 300 reps, 13s
dicker.Sigma.unknown_sample.sample$n10000d50 <- pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=10000, d=50)) # 300 reps, 1 minute
dicker.Sigma.unknown_sample.sample$n10000d500 <- pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=10000, d=500)) # 300 reps, 2.5 hours
dicker.Sigma.unknown_sample.sample$n1000d10 <- pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=1000, d=10)) 
dicker.Sigma.unknown_sample.sample$n1000d50 <- pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=1000, d=50)) 
dicker.Sigma.unknown_sample.sample$n1000d500 <- pbreplicate(300, dicker.Sigma.unknown.sample.fun(n=1000, d=500)) # 300 reps, 1.75 hours
dicker.Sigma.unknown_sample.sample$n1000d1200 <- pbreplicate(10, dicker.Sigma.unknown.sample_MP.fun(n=1000, d=1200)) # (use Moore-Penrose) # 10 reps, 1 hour
# m-p inverse
save(dicker.Sigma.unknown_sample.sample, file=here("files","dicker-Sigma-unknown-sample.Rda"))

  # Dicker (sigma unknown: banded)
set.seed(1)
dicker.Sigma.unknown_banded.sample <- list()
dicker.Sigma.unknown_banded.sample$n10000d10 <-pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=10000, d=10)) # 300 reps, 8s 
dicker.Sigma.unknown_banded.sample$n10000d50 <- pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=10000, d=50)) # 300 reps, 1 minute
dicker.Sigma.unknown_banded.sample$n10000d500 <- pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=10000, d=500)) # 300 reps, 2.5 hours
dicker.Sigma.unknown_banded.sample$n1000d10 <- pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=1000, d=10))
dicker.Sigma.unknown_banded.sample$n1000d50 <- pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=1000, d=50))
dicker.Sigma.unknown_banded.sample$n1000d500 <- pbreplicate(300, dicker.Sigma.unknown.banded.fun(n=1000, d=500)) # 300 reps, 2 hours
dicker.Sigma.unknown_banded.sample$n1000d1200 <- pbreplicate(100, dicker.Sigma.unknown.banded.fun(n=1000, d=1200)) # 300 reps, 8 hours [] need to run*
save(dicker.Sigma.unknown_banded.sample, file=here("files","dicker-Sigma-unknown-banded.Rda"))




# SETUP FOR FIGURES
  # Plugin
plugin.Sigma.known.list <- lapply(setNames(names(plugin.Sigma.known.sample), names(plugin.Sigma.known.sample)),
                                  function(x) plugin.Sigma.known.sample[[x]] %>% 
                                    {cbind("mean"=rowMeans(.), 
                                           "lower"=rowQuantiles(., probs=0.025), "upper"=rowQuantiles(., probs=0.975))}) 
plugin.Sigma.known <- reshape2::melt(plugin.Sigma.known.list) %>% rename(estimand=Var1, statistic=Var2, scenario=L1)

plugin.Sigma.known.plot <- list()
plugin.Sigma.known.plot$tau <- plugin.Sigma.known %>% filter(estimand=="tau") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")
plugin.Sigma.known.plot$sigma <- plugin.Sigma.known %>% filter(estimand=="sigma") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")


  #Dicker / Sigma known
dicker.Sigma.known.list <- lapply(setNames(names(dicker.Sigma.known.sample), names(dicker.Sigma.known.sample)),
                                  function(x) dicker.Sigma.known.sample[[x]] %>% 
                                    {cbind("mean"=rowMeans(.), 
                                           "lower"=rowQuantiles(., probs=0.025), "upper"=rowQuantiles(., probs=0.975))}) 
dicker.Sigma.known <- reshape2::melt(dicker.Sigma.known.list) %>% rename(estimand=Var1, statistic=Var2, scenario=L1)

dicker.Sigma.known.plot <- list()
dicker.Sigma.known.plot$tau <- dicker.Sigma.known %>% filter(estimand=="tau") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")
dicker.Sigma.known.plot$sigma <- dicker.Sigma.known %>% filter(estimand=="sigma") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")


  #Eigenprism / Sigma known: sigma
eigenprism.Sigma.known.plot <- list()
eigenprism.Sigma.known.plot$tau <- c("mean"=mean(eigenprism.Sigma.known.sample$n1000d1200_tau["estimate",] %>% unlist),
                                     "lower"=mean(matrix(eigenprism.Sigma.known.sample$n1000d1200_tau["CI",] %>% unlist, 
                                                         nrow=2, ncol=100, byrow=FALSE) %>% t() %>% .[,1]),
                                     "upper"=mean(matrix(eigenprism.Sigma.known.sample$n1000d1200_tau["CI",] %>% unlist, 
                                                         nrow=2, ncol=100, byrow=FALSE) %>% t() %>% .[,2]))
eigenprism.Sigma.known.plot$sigma <- c("mean"=mean(eigenprism.Sigma.known.sample$n1000d1200_sigma["estimate",] %>% unlist),
                                     "lower"=mean(matrix(eigenprism.Sigma.known.sample$n1000d1200_sigma["CI",] %>% unlist, 
                                                         nrow=2, ncol=100, byrow=FALSE) %>% t() %>% .[,1]),
                                     "upper"=mean(matrix(eigenprism.Sigma.known.sample$n1000d1200_sigma["CI",] %>% unlist, 
                                                         nrow=2, ncol=100, byrow=FALSE) %>% t() %>% .[,2]))


#Dicker / Sigma unknown: sample
dicker.Sigma.unknown_sample.list <- lapply(setNames(names(dicker.Sigma.unknown_sample.sample), names(dicker.Sigma.unknown_sample.sample)),
                                  function(x) dicker.Sigma.unknown_sample.sample[[x]] %>% 
                                    {cbind("mean"=rowMeans(.), 
                                           "lower"=rowQuantiles(., probs=0.025), "upper"=rowQuantiles(., probs=0.975))}) 
dicker.Sigma.unknown_sample <- reshape2::melt(dicker.Sigma.unknown_sample.list) %>% rename(estimand=Var1, statistic=Var2, scenario=L1) %>% 
  mutate(value=real.fun(value))

dicker.Sigma.unknown_sample.plot <- list()
dicker.Sigma.unknown_sample.plot$tau <- dicker.Sigma.unknown_sample %>% filter(estimand=="tau") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")
dicker.Sigma.unknown_sample.plot$sigma <- dicker.Sigma.unknown_sample %>% filter(estimand=="sigma") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")



#Dicker / Sigma unknown: banded
dicker.Sigma.unknown_banded.list <- lapply(setNames(names(dicker.Sigma.unknown_banded.sample), names(dicker.Sigma.unknown_banded.sample)),
                                           function(x) dicker.Sigma.unknown_banded.sample[[x]] %>% 
                                             {cbind("mean"=rowMeans(.), 
                                                    "lower"=rowQuantiles(., probs=0.025), "upper"=rowQuantiles(., probs=0.975))}) 
dicker.Sigma.unknown_banded <- reshape2::melt(dicker.Sigma.unknown_banded.list) %>% rename(estimand=Var1, statistic=Var2, scenario=L1)

dicker.Sigma.unknown_banded.plot <- list()
dicker.Sigma.unknown_banded.plot$tau <- dicker.Sigma.unknown_banded %>% filter(estimand=="tau") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")
dicker.Sigma.unknown_banded.plot$sigma <- dicker.Sigma.unknown_banded %>% filter(estimand=="sigma") %>% dplyr::select(-estimand) %>% spread(key="statistic", value="value")










# other things to consider for future scenarios
# Eigenprism
eigen.fun3 <- function(n=1000, d=2000){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  eigenprism(y=y, X=x, sigma2=TRUE)
}
eigen.fun3()
 # [] d>n, Sigma=I known

# check claims jansen vs. dicker
  # d > n, sigma^2 unknown, Sigma known = I
  # n=1000, d=1500