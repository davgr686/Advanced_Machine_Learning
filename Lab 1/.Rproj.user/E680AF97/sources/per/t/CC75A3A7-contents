library(bnlearn)
set.seed(1234)

#Note that hill-climbing always returns a DAG, not a CPDAG; so the
#correct way of comparing it with another graph is to take the CPDAG
#for both.
bnet1 <- hc(x = asia, start = random.graph(names(asia)) , score = "bic")
bnet1 <- cpdag(bnet1)
plot(bnet1)

bnet2 <- hc(x = asia, start = random.graph(names(asia)), score = "bic")
bnet2 <- cpdag(bnet2)
plot(bnet2)
all.equal(bnet1, bnet2)
# "Different number of directed/undirected arcs"
hamming(bnet1, bnet2)
## hamming distance return 6. zero means a perfect match


# part 2
n=dim(asia)[1] 
id=sample(1:n, floor(n*0.8))  
train=asia[id,]  
test=asia[-id,] 

predict_smoke_asia <- function(fitted, data) {
  preds <- c()
  for (i in 1:dim(data)[1]) {
    sample <- unlist(data[i, -2])
    prob <- cpquery(fitted, 
                    event=eval(parse(text="(S=='yes')")), 
                    evidence=list(A=sample[1], 
                                  T = sample[2], 
                                  L = sample[3], 
                                  B = sample[4], 
                                  E = sample[5], 
                                  X = sample[6], 
                                  D = sample[7]),
                    method="lw")
    if (prob >= 0.5) {
      preds[i] <- 'yes'
    }
    else {
      preds[i] <- 'no'
    }
  }
  return (preds)
}

predict_smoke_asia_mb <- function(fitted, data) {
  preds <- c()
  for (i in 1:dim(data)[1]) {
    sample <- unlist(data[i, -2])
    prob <- cpquery(fitted, 
                    event=eval(parse(text="(S=='yes')")), 
                    evidence=list(L = sample[3], B = sample[4]),
                    method="lw")
    if (prob >= 0.5) {
      preds[i] <- 'yes'
    }
    else {
      preds[i] <- 'no'
    }
  }
  return (preds)
}

bnet <- hc(x = train, score = "bic")
bnet_fitted <- bn.fit(bnet, train)
plot(bnet)
preds <- predict_smoke_asia(bnet_fitted, test)
mean(preds == test$S)
prop.table(table("true" = test$S, "preds" = preds))


dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
bnet_true_fitted <- bn.fit(dag, train)
plot(dag)
preds_true <- predict_smoke_asia(bnet_true_fitted, test)
mean(preds_true == test$S)
prop.table(table("true" = test$S, "preds" = preds_true))


## part 3
preds <- predict_smoke_asia_mb(bnet_fitted, test)
mean(preds == test$S)
prop.table(table("true" = test$S, "preds" = preds))


preds <- predict_smoke_asia_mb(bnet_true_fitted, test)
mean(preds == test$S)
prop.table(table("true" = test$S, "preds" = preds))
## the accuracy is the same as to when using the whole BN

## part 4
## Xi, Xj independent | S, for all Xi,Xj
## P(s,X1...Xn) = P(S)P(X1|S)*P(X2|S)...P(Xn|S)
n_bn <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]")
n_bn_fitted <- bn.fit(n_bn, train)
plot(n_bn)
preds <- predict_smoke_asia(n_bn_fitted, test)
mean(preds == test$S)
prop.table(table("true" = test$S, "preds" = preds))
