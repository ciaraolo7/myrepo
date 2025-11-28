#1)

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


#2)

par <- model.matrix(y ~ x1 + x2 + x3, data=df)
nll_lm <- function(df, par) {
  
  sig = var(y)
  
  (nrow(df)/2) * log(1/(2*pi*sig))- 1/(2*sig) * sum(dnorm(par, sig) - (par))
}

nll_lm(df, par)


#3)

fit <- optim(dnorm(mean(df$y), var(df$y)), fn = nll_lm(df,par))


#4)
#Optim minimizes functions by default so by minimizing the negative log-likelihood we are maximizing the log-likelihood.

#new

mod <- lm(y ~ x1 + x2 + x3, data=df)
coefficients(mod)
