if (!require("pacman")) install.packages("pacman")
pacman::p_load(pbapply, mvtnorm, tidyverse, expm, ggsci, here)

norm_vec <- function(x) sqrt(sum(x^2))
normsq_vec <- function(x) sum(x^2)

#### REGIME 1: \tau^2 (beta)
# Fuction for regime 1: OLS
ols.tau.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <-  rnorm(n, mean=x %*% b, sd=1)
  #t(b)  %*% b
  #m.ols <- lm(y ~ 0 + x)
  #normsq_vec(m.ols$coef)
  ( 1/ (n-d) ) * ( t(y) %*% x %*% solve( t(x) %*% x ) %*% t(x) %*% y) - ( d / (n*(n-d) ) )*normsq_vec(y) 
}
pbreplicate(100, ols.tau.fun(n=100, d=90)) %>% mean


d <- 5
n <- 1000
y <- rnorm(n, mean=0, sd=1)
x <- rmvnorm(n, rep(0,d))
m.ols <- lm(y ~ 0 + x)
(m.ols$coef)^2 %*% diag(d) %*% (m.ols$coef)
normsq_vec( (diag(d))^(1/2) %*% m.ols$coef)

(1/n)*normsq_vec(y) - (1/(n-d))*( normsq_vec(m.ols$residuals) )







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
