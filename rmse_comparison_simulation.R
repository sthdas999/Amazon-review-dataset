set.seed(123)
N <- 500
n <- 200

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

rmse_ols <- rmse_kernel <- rmse_pls <- numeric(N)

for(r in 1:N){
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  Z1 <- runif(n)
  Z2 <- runif(n)
  eps <- rnorm(n)

  Y <- 2*X1 + 3*X2 + sin(Z1) + Z2^2 + eps

  # OLS
  fit_ols <- lm(Y ~ X1 + X2)
  yhat_ols <- predict(fit_ols)

  # Kernel (simple NW)
  library(np)
  fit_np <- npreg(Y ~ Z1 + Z2)
  yhat_np <- fitted(fit_np)

  # PLS (linear part only for illustration)
  library(pls)
  fit_pls <- plsr(Y ~ X1 + X2, ncomp = 2)
  yhat_pls <- predict(fit_pls, ncomp = 2)

  rmse_ols[r] <- rmse(Y, yhat_ols)
  rmse_kernel[r] <- rmse(Y, yhat_np)
  rmse_pls[r] <- rmse(Y, yhat_pls)
}

mean(rmse_ols)
mean(rmse_kernel)
mean(rmse_pls)
