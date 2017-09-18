#' Simulation function
#'
#' @param reps
#' @param n
#' @param beta
#' @param eps.dist
#' @param ...
#'
#' @return a list of legnth 3 containing the distribution of beta hat, estimates of the power of the test to detect the effect beta and the distribution of R2.
#'
#' @export
#'
#' @import rmutil
#'
#' @examples
#' regsim(reps = 1000, n = 100, beta = 1.2, eps.dist = "uniform", min = -1, max = 1)
#' regsim(reps = 1000, n = 100, beta = 1.2, eps.dist = "t")
#' regsim(reps = 1000, n = 100, beta = 1.2, eps.dist = "laplace", m = 0 , s = 1)
regsim <- function(reps = 1000, n = 100, beta = 1.2, eps.dist = "uniform", ...){
    library(rmutil)
    lis <- vector(length = 3, mode = "list")
    names(lis) <- c("beta_hat", "power", "rsq")
    # beta containers
    lis$beta_hat <- matrix(NA, nrow = reps, ncol = 2)
    colnames(lis$beta_hat) <- c("sim", "norm")
    # rsq containers
    lis$rsq <- matrix(NA, nrow = reps, ncol = 2)
    colnames(lis$rsq) <- c("sim", "norm")
    # power containers
    lis$power <- matrix(NA, nrow = 1, ncol = 2)
    colnames(lis$power) <- c("sim", "norm")

    save_sim <- vector(length = reps)
    save_norm <- vector(length = reps)

    for(i in 1:reps){
        err_dist <- switch(eps.dist,
                           "uniform" = runif(n, ...) * sample(c(-1, 1), n, replace = TRUE),
                           "t" = rt(n, df = n-2, ...),
                           "laplace" = rlaplace(n, ...) * sample(c(-1, 1), n, replace = TRUE))
        x <- rnorm(n)
        err_norm <- rnorm(n, sd = sd(err_dist))
        beta0 <- 0.6
        # generate responses
        y <- beta0 + beta * x + err_dist
        y_norm <- beta0 + beta * x + err_norm

        # run lm
        mod <- lm(y ~ x)
        mod_norm <- lm(y_norm ~ x)

        lis$beta_hat[i, 1] <- mod$coefficients[2]
        lis$beta_hat[i, 2] <- mod_norm$coefficients[2]

        # find rejection region using asymptotic distn
        z <- sqrt(n)*(abs(mod$coefficients[2] - beta))
        save_sim[i] <- ifelse(z >= qnorm(0.975), FALSE, TRUE)
        z <- sqrt(n)*(mod_norm$coefficients[2] - beta)
        save_norm[i] <- ifelse(z >= qnorm(0.975), FALSE, TRUE)

        lis$rsq[i, 1] <- summary(mod)$r.squared
        lis$rsq[i, 2] <- summary(mod_norm)$r.squared
    }

    lis$power[1,1] <- mean(save_sim)
    lis$power[1,2] <- mean(save_norm)

    lis$beta_hat <- as.data.frame(lis$beta_hat)
    lis$power <- as.data.frame(lis$power)
    lis$rsq <- as.data.frame(lis$rsq)

    return(lis)
}


#' Simulation plotting function
#'
#' @param reps
#' @param n
#' @param beta
#' @param eps.dist
#' @param which.plot
#' @param ...
#' @return a series of plots comparing parameters simulated from different error distributions
#' @export
#' @import ggplot2
#' @import reshape2
#' @import gridExtra
#' @import stringr
#' @examples
#' plotRegsim(reps = 1000, n = c(50, 100, 500), beta = c(.4,  1, 1.5), eps.dist = "uniform", min = -1, max = 1, which.plot = "r2")
#' plotRegsim(reps = 1000, n = c(50, 100, 500), beta = c(.4, .8, 1, 1.5), eps.dist = "laplace", m = -1, s = 1, which.plot = "power")
#' plotRegsim(reps = 1000, n = c(50, 100, 500), beta = c(.4, .8, 1.5), eps.dist = "t", which.plot = "beta hat")
plotRegsim <- function(reps = 1000,
                       n = c(50, 100, 500),
                       beta = c(0.4, 0.8, 1, 1.5),
                       eps.dist = "uniform",
                       which.plot = "r2",
                       ...){
    library(rmutil)
    library(ggplot2)
    library(reshape2)
    library(gridExtra)
    library(stringr)
    # create containers
    indx <- seq(1,reps, by= 1)
    power_box <- rep(NA, each = 4)
    beta_box<- rep(NA, each = 4)
    r2_box <- rep(NA, each = 4)
    # loop through combinations of betas and ns
    for(j in seq_along(n)){
        for(k in seq_along(beta)){
            lis <- regsim(reps = reps,
                          n = n[j],
                          beta = beta[k],
                          eps.dist = eps.dist,
                          ... = ... )
            if(which.plot == "power"){
                power_box <- rbind(power_box, cbind(lis$power,
                                                    n = rep(n[j], each = reps),
                                                    beta = rep(beta[k], each = reps)))
            }

            if(which.plot == "beta hat"){
                beta_box <- rbind(beta_box, cbind(lis$beta_hat,
                                                      n = rep(n[j], each = reps),
                                                      beta = rep(beta[k], each = reps)))
            }

            if(which.plot == "r2"){
                r2_box <- rbind(r2_box, cbind(lis$rsq,
                                               n = rep(n[j], each = reps),
                                               beta = rep(beta[k], each = reps)))
            }
        }
    }
    # plot which.plot
    if(which.plot == "power"){
        names(power_box) <- c("sim", "norm", "n", "betak")
        power_box <- melt(power_box[-1,], id = c("n", "betak"))
        power_box$betak <- as.factor(power_box$betak)
        levels(power_box$betak) <- paste0(rep("beta = ", each = length(beta)), levels(power_box$betak))
        p <- ggplot(power_box, aes(x = n, y = value)) +
            geom_line(aes(group = variable, color = variable,
                          linetype = variable),
                      position=position_dodge(width=0.9)) +
            facet_wrap(~betak) +
            labs(x = "Sample size", y = "Power",
                 title = "Simulated Power")
        return(p)
    }else if(which.plot == "beta hat"){
        beta_box <- beta_box[-1,]
        beta_box$n <- as.factor(beta_box$n)
        levels(beta_box$n) <- paste0(rep("n = ", each = length(n)), levels(beta_box$n))
        beta_box$beta <- as.factor(beta_box$beta)
        levels(beta_box$beta) <- paste0(rep("beta = ", each = length(beta)), levels(beta_box$beta))
        names(beta_box) <- c("sim", "norm", "n", "betak")
        p <-ggplot(beta_box, aes(x = norm, y = sim)) +
            geom_point(size = 0.4) +
            labs(y = str_c("Estimated Coefficients (", eps.dist, " error distribution)"),
                 x = "Estimated Coefficients  (normal error distribution)",
                 title ="Simulated Estimated Coefficients") +
            geom_abline(slope = 1, intercept = 0) +
            facet_wrap(n~betak)
        return(p)
    }else if(which.plot == "r2"){
        r2_box <- r2_box[-1,]
        r2_box$n <- as.factor(r2_box$n)
        levels(r2_box$n) <- paste0(rep("n = ", each = length(n)), levels(r2_box$n))
        r2_box$beta <- as.factor(r2_box$beta)
        levels(r2_box$beta) <- paste0(rep("beta = ", each = length(beta)), levels(r2_box$beta))
        names(r2_box) <- c("sim", "norm", "n", "betak")
        p <- ggplot(r2_box, aes(x = norm, y = sim)) +
            geom_point(size = 0.4) +
            labs(x = "R-squared (normal error distribution)",
                 y = str_c("R-squared (", eps.dist, " error distribution)"),
                 title = "Simulated R-squared") +
            geom_abline(slope = 1, intercept = 0)+
            facet_wrap(n~betak)
        return(p)
    }
}
