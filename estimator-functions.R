# functions
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS, pbapply, mvtnorm, tidyverse, expm, ggsci, here, matrixStats)

# misc
norm_vec <- function(x) sqrt(sum(x^2))
normsq_vec <- function(x) sum(x^2)
real.fun <- function(x) {
  if (all(Im(z <- zapsmall(x))==0)) as.numeric(z) else x
}

# plugin estimator with known Sigma
plugin.Sigma.known.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
    tau <- ( 1/(n-d) ) * ( t(y) %*% x %*% solve( t(x) %*% x ) %*% t(x) %*% y ) - ( d / (n*(n-d)) ) * ( normsq_vec(y) )
    sigma <- ( 1/(n-d) ) * ( sum(y^2) )  -  ( 1 / (n-d) )*  ( t(y) %*% x %*% solve( t(x) %*% x ) %*% t(x) %*% y )
  
  return(c("tau"=tau, "sigma"=sigma))
}

# Dicker estimator with known Sigma
dicker.Sigma.known.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
    tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
    sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * ( normsq_vec((t(x) %*% y)) )
    
  return(c("tau"=tau, "sigma"=sigma))
}

# EigenPrism estimator with known Sigma
source("eigenprism.R")
eigenprism.Sigma.known_tau.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
  EigenPrism(y=y, X=x, target="beta2")
}
eigenprism.Sigma.known_sigma.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  
  EigenPrism(y=y, X=x, target="sigma2")
}

# Dicker estimator: sample estiamte for unknown Sigma
dicker.Sigma.unknown.sample.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  S <- (1/n) * (t(x) %*% x)
  
    tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S)) %*% t(x) %*% y )
    sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S)) %*% t(x) %*% y )
  
  return(c("tau"=tau, "sigma"=sigma))
}

dicker.Sigma.unknown.sample_MP.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  S <- (1/n) * (t(x) %*% x)
  
  tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(ginv(S)) %*% t(x) %*% y )
  sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(ginv(S)) %*% t(x) %*% y )
  
  return(c("tau"=tau, "sigma"=sigma))
}




# Dicker estimator: Banded matrix for unknown Sigma
dicker.Sigma.unknown.banded.fun <- function(n, d){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  S <- (1/n) * (t(x) %*% x)
  S.band <- band(S, k1=-2, k2=2)
  
    tau <- -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S.band)) %*% t(x) %*% y )
    sigma <- ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S.band)) %*% t(x) %*% y )

  return(c("tau"=tau, "sigma"=sigma))
}
