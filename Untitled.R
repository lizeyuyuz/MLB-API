reps <- 1000
ns <- c(10, 25, 50, 100)
effect_size <- seq(.1, 1, by = .1)
# We'll save the power at each combination here.
# Each row represents one combination of n and effect size,
# the 2nd and 3rd column will save the values.
results <- matrix(nrow = 0,
                  ncol = 3)
# Loop over each combination and perform `reps`.
for (i in seq_along(ns)) {
    n <- ns[i]
    for (j in seq_along(effect_size)) {
        es <- effect_size[j]
        # We'll save whether each test rejects.
        save <- vector(length = reps)
        for (r in 1:reps) {
            # Burger temps can range from 125 (rare) to 165 (well-done).
            temp <- sample(125:165, n, TRUE)
            temp <- scale(temp)
            # Widths can vary between .5 to 1.5 inches, with a normal distribution.
            width <- rnorm(n, 1, .25)
            # careful, min and max don't work with vectors!
            width <- pmax(pmin(width, 1.5), .5)
            width <- scale(width)
            # Generate the responses.
            verdict <- 1*temp + (1 + es)*width + rnorm(n)
            # Run the model
            mod <- lm(verdict ~ temp + width)
            # This function from the car package test whether temp = width
            save[r] <- car::linearHypothesis(mod, "temp = width")$Pr[2] < .05
        }
        # Now that we have the results for this choice of n and effect size,
        # let's save it and move on.
        results <- rbind(results, c(mean(save), n, es))
    } }
resultsdf <- data.frame(results)
names(resultsdf) <- c("Power", "n", "Effect.size")

# Convert n to a factor to make plotting nicer
resultsdf$n <- as.factor(resultsdf$n)



plotRegsim <- function(reps = 1000,
                       n = c(50, 100, 500),
                       beta = c(0.4, 0.8, 1, 1.5),
                       eps.dist = "uniform",
                       which.plot = "r2",
                       ...){
    P <- list()
    indx <- seq(1,reps, by= 1)

    for(j in seq_along(n)){
        for(k in seq_along(beta)){
            lis <- regsim(reps = reps,
                          n = n[j],
                          beta = beta[k],
                          eps.dist = eps.dist,
                          ... = ... )
            if(which.plot == "beta hat"){
                beta_box <- cbind(lis$beta_hat, num = indx)
                beta_box <- melt(beta_box, id = "num")
                p <- ggplot(beta_box, aes(x = num, y = value)) +
                    geom_line(aes(color= variable)) +
                    labs(x = "Index", y = "Estimated Coefficient",
                         title = str_c("Simulated Estimated coefficients (n = ",
                                       n[j], ", beta = ", beta[k], ")")) +
                    scale_colour_discrete(name="error\ndistribution",
                                          labels=c(sim=eps.dist, norm="normal")) +
                    geom_hline(yintercept = beta[k])
                P <- c(P, list(p))

            }

            if(which.plot == "power"){
                suppressMessages(power_box <- melt(lis$power))
                power_box$variable <- c(eps.dist, "normal")
                p <- ggplot(power_box, aes(x=variable, y=value)) +
                    geom_bar(stat="identity", width=0.5) +
                    labs(x = "", y = "Power", title = str_c("Simulated Power (n = ", n[j],
                                                            ", beta = ", beta[k], ")"))
                P <- c(P, list(p))
            }

            if(which.plot == "r2"){
                r2_box <- cbind(lis$rsq, num = indx)
                r2_box <- melt(r2_box, id = "num")
                p <- ggplot(r2_box, aes(x = num, y = value)) +
                    geom_line(aes(color= variable)) +
                    labs(x = "Index", y = expression(R^2),
                         title = str_c("Simulated R-squared (n = ", n[j],
                                       ", beta = ", beta[k], ")")) +
                    scale_colour_discrete(name="error\ndistribution",
                                          labels=c(sim=eps.dist, norm="normal"))
                P <- c(P, list(p))
            }
        }
    }
    return(grid.arrange(grobs = P, ncol = 2))
}

plotRegsim(reps = 1000,
           n = c(50, 100, 500),
           beta = c(.4, .8, 1, 1.5),
           eps.dist = "uniform",
           min = -1,
           max = 1,
           which.plot = "r2")

plotRegsim(reps = 1000,
           n = c(50, 100, 500),
           beta = c(.4, .8, 1, 1.5),
           eps.dist = "uniform",
           min = -1,
           max = 1,
           which.plot = "power")


plotRegsim <- function(reps = 1000,
                       n = c(50, 100, 500),
                       beta = c(0.4, 0.8, 1, 1.5),
                       eps.dist = "uniform",
                       which.plot = "r2",
                       ...){
    P <- list()
    indx <- seq(1,reps, by= 1)
    power_box <- c(NA, NA, NA, NA)
    for(j in seq_along(n)){
        for(k in seq_along(beta)){
            lis <- regsim(reps = reps,
                          n = n[j],
                          beta = beta[k],
                          eps.dist = eps.dist,
                          ... = ... )

            if(which.plot == "power"){
                power_box <- rbind(power_box, cbind(lis$power, n[j], beta[k]))
            }

            if(which.plot == "beta hat"){
                beta_box <- lis$beta_hat
                p <-ggplot(beta_box, aes(x = norm, y = sim)) +
                    geom_point() +
                    labs(y = str_c("Estimated Coefficients \n (", eps.dist, " error distribution)"),
                         x = "Estimated Coefficients \n (normal error distribution)",
                         title = str_c("Simulated Est coefficients \n (n = ",
                                       n[j], ", beta = ", beta[k], ")")) +
                    geom_abline(slope = 1, intercept = 0)
                P <- c(P, list(p))

            }

            if(which.plot == "r2"){
                r2_box <- lis$rsq
                p <- ggplot(r2_box, aes(x = norm, y = sim)) +
                    geom_point() +
                    labs(x = "R-squared (normal error distribution)",
                         y = str_c("R-squared (", eps.dist, " error distribution)"),
                         title = str_c("Simulated R-squared (n = ", n[j],
                                       ", beta = ", beta[k], ")")) +
                    geom_abline(slope = 1, intercept = 0)
                P <- c(P, list(p))
            }
        }
    }
    if(which.plot == "power"){
        names(power_box) <- c("sim", "norm", "n", "betak")
        power_box <- melt(power_box[-1,], id = c("n", "betak"))
        Pp <- ggplot(power_box, aes(x = n, y = value)) +
            geom_line(aes(group = variable, color = variable,
                          linetype = variable),
                      position=position_dodge(width=0.9)) +
            facet_wrap(~betak) +
            labs(x = "Sample size", y = "Power",
                 title = "Simulated Power")
        return(Pp)
    }else{
        return(grid.arrange(grobs = P, ncol = ceiling(sqrt(length(P)))))
    }

}
