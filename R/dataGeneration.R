#' Generate multilevel dataset
#'
#' This function returns a dataframe with a multilevel structure. It generates a dataframe using a varying
#' intercepts/varying slopes linear regression with a single target variable y.
#'
#' @param nClass number of classes
#' @param nVars number of independent variables (X)
#' @param classMean average number of observations per class
#' @param classSD standard deviation of the number of observations per class
#' @param beta0 intercept parameter
#' @param tau0 variance of the parameter between classes
#' @param beta vector with the slope parameters, one for each independent variable
#' @param tau vector with the variance of the slope parameters, one for each independent variable
#' @param sigma2 error variance
#' @importFrom stats rnorm
#' @importFrom magrittr %>%
#' @return A dataframe with the multilevel dataset
#' @export
#'
#' @examples
#' df <- create.multilevel(nClass = 20,
#'                           nVars = 1,
#'                           classMean = 10,
#'                           classSD = 2)
create.multilevel <- function(nClass = 10, nVars = 1, classMean = 10, classSD = 0, beta0 = 0, tau0 = 1, beta = c(1), tau = c(1), sigma2 = 1) {

  sizeCluster <- as.integer(rnorm(nClass, mean = classMean, sd = classSD))
  sizeCluster[sizeCluster<=5] <- 5
  class <- c()
  beta0_j <- c()
  n <- sum(sizeCluster)
  X <- matrix(nrow = n, ncol = nVars)


  i <- 1
  for(k in sizeCluster){
    class <- c(class, rep(i, k))
    beta0_j <- c(beta0_j, rep(rnorm(1, beta0, tau0), k))
    i <- i + 1
  }

  error <- rnorm(n, mean = 0, sd = sigma2)
  y <- beta0_j + error

  for(j in 1:nVars){
    beta_j <- c()
    for(k in sizeCluster){
      beta_j <- c(beta_j, rep(rnorm(1, beta[j], tau[j]), k))
    }
    x <- rnorm(n)
    X[ ,j] <- x
    y <- y + beta_j*X

  }

  df <- data.frame(class, y, X) %>% dplyr::mutate(class = as.factor(class))
  df

}




