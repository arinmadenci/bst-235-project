# sample Sigma (Sigma unknown)
dicker.Sigma.unknown.sample.fun <- function(n, d, estimator){
  x <- rmvnorm(n, mean=rep(0,d))
  b.pre <- c(runif(floor(d/2)), rnorm(n=ceiling(d/2)))
  b <- b.pre / sqrt(as.vector(t(b.pre) %*% b.pre))
  y <- x %*% b + rnorm(n, mean=0, sd=1)
  S <- (1/n) * (t(x) %*% x)
  if(estimator=="tau"){
    -( d/(n*(n+1)) ) * ( sum(y^2) ) + ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S)) %*% t(x) %*% y )
  }
  else if(estimator=="sigma"){
    ( (d+n+1)/(n*(n+1)) ) * ( sum(y^2) )  -  ( 1 / (n*(n+1)) ) * normsq_vec( sqrtm(solve(S)) %*% t(x) %*% y )
  }
}


