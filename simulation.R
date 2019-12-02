# setup
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pbapply, mvtnorm, tidyverse, expm, ggsci, here)

norm_vec <- function(x) sqrt(sum(x^2))
normsq_vec <- function(x) sum(x^2)

#### REGIME 1
# Fuction for regime 1: OLS
ols.fun <- function(n, d){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  m.ols <- lm(y ~ x)
  (1/(n-d))*( normsq_vec(m.ols$residuals) )
}

mse.ols.sample <- list()
mse.ols.sample$d10 <-pbreplicate(100, ols.fun(n=10000, d=10)) 
mse.ols.sample$d50 <- pbreplicate(5, ols.fun(n=10000, d=50))
mse.ols.sample$d500 <- pbreplicate(5, ols.fun(n=10000, d=500))
mse.ols.sample$d900 <- pbreplicate(5, ols.fun(n=10000, d=900))

mse.ols.list <- lapply(setNames(names(mse.ols.sample), names(mse.ols.sample)),
                  function(x) mse.ols.sample[[x]] %>% {c("mean"=mean(.), quantile(., probs=c(0.025, 0.975)))}) 
mse.ols <- data.frame(mse.ols.list %>% data.frame %>% t(),
                      category=names(mse.ols.list))


fig1.ols <- ggplot(data=mse.ols) + 
  geom_pointrange(mapping=aes(y=mean, ymin=`X2.5.`,ymax=`X97.5.`, x=category, color=category)) +
  scale_y_continuous(name="Residual variance") + 
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
  geom_histogram(mapping=aes(x=value), bins=100) +
  facet_grid(key ~ .) + 
  scale_y_continuous(name="Count") + scale_x_continuous(name="Residual variance") + 
  ggtitle("OLS") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.ols.hist, file=here("figures","fig1-ols-hist.pdf"),
       width=7, height=7)


# Fuction for regime 1: Dicker
dicker.fun <- function(n, d){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
}

mse.dicker.sample <- list()
mse.dicker.sample$d10 <- pbreplicate(100, dicker.fun(n=10000, d=10)) 
mse.dicker.sample$d50 <- pbreplicate(5, dicker.fun(n=10000, d=50))
mse.dicker.sample$d500 <- pbreplicate(5, dicker.fun(n=10000, d=500))
mse.dicker.sample$d900 <- pbreplicate(5, dicker.fun(n=10000, d=900))

mse.dicker.list <- lapply(setNames(names(mse.dicker.sample), names(mse.dicker.sample)),
                       function(x) mse.dicker.sample[[x]] %>% {c("mean"=mean(.), quantile(., probs=c(0.025, 0.975)))}) 
mse.dicker <- data.frame(mse.dicker.list %>% data.frame %>% t(),
                      category=names(mse.dicker.list))

fig1.dicker <- ggplot(data=mse.dicker) + 
  geom_pointrange(mapping=aes(y=mean, ymin=`X2.5.`,ymax=`X97.5.`, x=category, color=category)) +
  scale_y_continuous(name="Residual variance") + 
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
  geom_histogram(mapping=aes(x=value), bins=100) +
  facet_grid(key ~ .) + 
  scale_y_continuous(name="Count") + scale_x_continuous(name="Residual variance") + 
  ggtitle("Dicker") +
  theme_classic() + scale_color_aaas()
ggsave(fig1.dicker.hist, file=here("figures","fig1-dicker-hist.pdf"),
       width=7, height=7)







#### REGIME 3
  ## Sigma estimator #1
dicker.fun3 <- function(n=1000, d=500){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  S <- (1/n) * (t(x) %*% x)
  ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S)) %*% t(x) %*% y )
}
num3.sample <- pbreplicate(5, dicker.fun3())

num3 <- num3.sample %>% {c("mean"=mean(.), quantile(., probs=c(0.025, 0.975)))}



# Banded matrix
band.fun3 <- function(n=1000, d=500){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  S <- (1/n) * (t(x) %*% x)
  S.band <- band(S, k1=-2, k2=2)
  ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S.band)) %*% t(x) %*% y )
}
num3.band.sample <- pbreplicate(5, band.fun3())

# Eigenprism
eigen.fun3 <- function(n=1000, d=2000){
  y <- rnorm(n, mean=0, sd=1)
  x <- rmvnorm(n, rep(0,d))
  eigenprism(y=y, X=x, sigma2=TRUE)
}
eigen.fun3()


dat3 <- bind_rows(num3.band.sample %>% as.data.frame() %>% mutate(category="Banded Covariance Matrix"),
                   num3.sample %>% as.data.frame() %>% mutate(category="Sample Covariance Matrix")) %>% 
  rename(value=".")

fig3a <- ggplot(dat3 %>% group_by(category) %>% 
                  summarise(mean=mean(value), lower=quantile(value, probs=0.025), upper=quantile(value, probs=0.975))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=category)) +
  scale_y_continuous(name="Residual variance") + 
  scale_x_discrete(name=NULL) + 
  ggtitle("Sigma Unknown (Estimable)") +
  theme_classic() + scale_color_aaas()
ggsave(fig3a, file=here("figures","fig3a.pdf"),
       width=7, height=7)

fig3a.hist <- ggplot(data=dat3) +
  geom_histogram(mapping=aes(x=value), bins=100) +
  scale_y_continuous(name="Count") + scale_x_continuous(name="Residual variance") + 
  facet_grid(category ~ .) + 
  ggtitle("Sigma Unknown (Estimable)") +
  theme_classic() + scale_color_aaas()
ggsave(fig3a.hist, file=here("figures","fig3a-hist.pdf"),
       width=7, height=7)




# 
# d <- 5
# b.pre <- c(runif(d/2), rnorm(n=d/2))
# b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
# 
# t(b)  %*% b
# solve(x %*% t(x)) %*% t(x) %*% y