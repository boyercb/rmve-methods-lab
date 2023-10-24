# R script for J&J workshop

library(ggplot2)

# example 1 ---------------------------------------------------------------

# In this example we have a dataset with three covariates, X1, X2, and X3, as
# well as an indicator of treatment over follow up, A. We would like to build a
# model for treatment naïve Y ~ X1 only as X2 and X3 are available to us for
# training but unavailable to the clinician at the time predictions are made.

# load data
factual <- readRDS("factual.rds")
counterfactual <- readRDS("counterfactual.rds")

# plot observed data
ggplot(df, aes(x = X1, y = Y, shape = factor(A))) +
  geom_point() +
  scale_shape_manual(values = c(16, 1), name = "A") +
  theme_minimal() +
  theme(legend.position = "top")

# fit naïve prediction model ignoring treatment A
naive <- lm(Y ~ X1, data = factual)

# building a treatment naïve model: outcome approach
om1 <- lm(Y ~ X1 + X2 + X3 + A, data = factual)
factual$Yhat <- predict(om1, newdata = transform(factual, A = 0))
om2 <- lm(Yhat ~ X1, data = factual)

# building a treatment naïve model: ipw approach
wfit <- glm(
  formula = A ~ X1 + X2 + X3,
  family = binomial(link = "logit"),
  data = factual
)
pA <- predict(wfit, type = 'response') 
w <- (1 - factual$A)/(1 - pA)

ipw <- lm(Y ~ X1, data = factual, weights = w)

# compare fits in counterfactual data
counterfactual$Yhat <- NA
df <- rbind(factual, counterfactual)
df$S <- 0
df$S[1:nrow(factual)] <- 1

df$naive <- predict(naive, newdata = df)
df$om[df$S==0] <- predict(om2, newdata = df)[df$S==0]
df$ipw[df$S==0] <- predict(ipw, newdata = df)[df$S==0]

ggplot(df, aes(x = X1, y = Y, shape = factor(A))) +
  facet_wrap(~ factor(S, labels = c("Counterfactual (A=0)", "Factual"))) +
  geom_point() +
  geom_line(aes(x = X1, y = naive),
            color = "blue",
            linewidth = 1.25) +
  geom_line(
    aes(x = X1, y = om),
    color = "blue",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  geom_line(
    aes(x = X1, y = ipw),
    color = "blue",
    linewidth = 1.25,
    linetype = "dotted"
  ) +
  scale_shape_manual(values = c(16, 1), guide = "none") +
  theme_minimal()


# estimate naïve MSEs
mean((factual$Y - predict(naive))^2)
mean((factual$Y - predict(om2))^2)
mean((factual$Y - predict(ipw))^2)

# estimate counterfactual MSE: ipw approach
mean(I(factual$A == 0) * w * (factual$Y - predict(naive))^2)
mean(I(factual$A == 0) * w * (factual$Y - predict(om2))^2)
mean(I(factual$A == 0) * w * (factual$Y - predict(ipw))^2)

# truth
mean((counterfactual$Y - predict(naive, newdata = counterfactual))^2)
mean((counterfactual$Y - predict(om2, newdata = counterfactual))^2)
mean((counterfactual$Y - predict(ipw, newdata = counterfactual))^2)
