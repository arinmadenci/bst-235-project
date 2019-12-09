if (!require("pacman")) install.packages("pacman")
pacman::p_load(pbapply, mvtnorm, tidyverse, expm, ggsci, here)

norm_vec <- function(x) sqrt(sum(x^2))
normsq_vec <- function(x) sum(x^2)

dicker.Sigma.known.fun <- function(n, d, estimator){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  if (estimator=="tau"){
  -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )}
  else if (estimator=="sigma"){
    ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
  }
}

mse.dicker.sample <- list()
mse.dicker.sample$d10 <- pbreplicate(100, dicker.tau.fun(n=10000, d=10)) 
mse.dicker.sample$d50 <- pbreplicate(100, dicker.tau.fun(n=10000, d=50))
mse.dicker.sample$d500 <- pbreplicate(10, dicker.tau.fun(n=10000, d=500))
mse.dicker.sample$d500n1000 <- pbreplicate(10, dicker.tau.fun(n=1000, d=500))
mse.dicker.sample$d900n1000 <- pbreplicate(10, dicker.tau.fun(n=1000, d=900))
mse.dicker.sample$d1200n1000 <- pbreplicate(10, dicker.tau.fun(n=1000, d=1200))
mse.dicker.sample$d2000n1000 <- pbreplicate(10, dicker.tau.fun(n=1000, d=2000))

mse.dicker.list <- lapply(setNames(names(mse.dicker.sample), names(mse.dicker.sample)),
                          function(x) mse.dicker.sample[[x]] %>% {c("mean"=mean(.), quantile(., probs=c(0.025, 0.975)))}) 
mse.dicker <- data.frame(mse.dicker.list %>% data.frame %>% t(),
                         category=names(mse.dicker.list))
save(mse.dicker.list, file=here("files","mse-dicker-list.Rda"))
save(mse.dicker, file=here("files","mse-dicker.Rda"))

fig1.dicker <- ggplot(data=mse.dicker) + 
  geom_pointrange(mapping=aes(y=mean, ymin=`X2.5.`,ymax=`X97.5.`, x=category, color=category)) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("d=10","d=50","d=500","d=900")) +
  ggtitle("Dicker") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.dicker, file=here("figures","fig1-dicker.pdf"),
       width=7, height=7)

fig1.dicker.hist <- ggplot(data=mse.dicker.sample %>% data.frame %>% gather %>% mutate(key=case_when(key=="d10"~"d=10",
                                                                                                     key=="d50"~"d=50",
                                                                                                     key=="d500"~"d=500",
                                                                                                     key=="d900"~"d=900"))) +
  geom_histogram(mapping=aes(x=value), bins=40, fill="grey", color="black") +
  facet_grid(key ~ .) + 
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  ggtitle("Dicker") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.dicker.hist, file=here("figures","fig1-dicker-hist.pdf"),
       width=7, height=7)