# STM for background jobs -------------------------------------------------
# topicN <- seq(from = 10, to = 100, by = 10)
library(stm)
topicN <- c(3, 10)

storage <- searchK(out$documents, out$vocab, K = topicN)

