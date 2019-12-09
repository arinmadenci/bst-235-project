if (!require("pacman")) install.packages("pacman")
pacman::p_load(pbapply, mvtnorm, tidyverse, expm, ggsci, here)

norm_vec <- function(x) sqrt(sum(x^2))
normsq_vec <- function(x) sum(x^2)

#### REGIME 1
# Fuction for plugin estimator w/ known Sigma
plugin.Sigma.known.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
  tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
  sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
  
  return(c("tau"=tau, "sigma"=sigma))
}

plugin.Sigma.known.MP.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
  tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
  sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
  
  return(c("tau"=tau, "sigma"=sigma))
}


replicate(10, plugin.Sigma.known.fun(n=1000, d=60))


pbreplicate(10, ols.mse.fun(n=1000, d=60, estimator="sigma")) %>% mean
pbreplicate(100, ols.mse.fun2(n=1000, d=60)) %>% mean

# (1/(n-d))*( normsq_vec(y) ) - (1/(n-d)) * ( y %*% x %*% solve( t(x) %*% x ) %*% t(x) %*% y ) # identical to function

mse.ols.sample <- list()
mse.ols.sample$d10 <-pbreplicate(100, ols.mse.fun(n=10000, d=10)) 
mse.ols.sample$d50 <- pbreplicate(100, ols.mse.fun(n=10000, d=50))
mse.ols.sample$d500 <- pbreplicate(100, ols.mse.fun(n=10000, d=500))
mse.ols.sample$d900 <- pbreplicate(100, ols.mse.fun(n=10000, d=900))

mse.ols.list <- lapply(setNames(names(mse.ols.sample), names(mse.ols.sample)),
                       function(x) mse.ols.sample[[x]] %>% {c("mean"=mean(.), quantile(., probs=c(0.025, 0.975)))}) 
mse.ols <- data.frame(mse.ols.list %>% data.frame %>% t(),
                      category=names(mse.ols.list))
save(mse.ols.list, file=here("files","mse-ols-list.Rda"))
save(mse.ols, file=here("files","mse-ols.Rda"))

fig1.ols <- ggplot(data=mse.ols) + 
  geom_pointrange(mapping=aes(y=mean, ymin=`X2.5.`,ymax=`X97.5.`, x=category, color=category)) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("d=10","d=50","d=500","d=900")) +
  ggtitle("OLS") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.ols, file=here("figures","fig1-ols.pdf"),
       width=7, height=7)

fig1.ols.hist <- ggplot(data=mse.ols.sample %>% data.frame %>% gather %>% mutate(key=case_when(key=="d10"~"d=10",
                                                                                               key=="d50"~"d=50",
                                                                                               key=="d500"~"d=500",
                                                                                               key=="d900"~"d=900"))) +
  geom_histogram(mapping=aes(x=value), bins=40, fill="grey", color="black") +
  facet_grid(key ~ .) + 
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  ggtitle("OLS") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.ols.hist, file=here("figures","fig1-ols-hist.pdf"),
       width=7, height=7)