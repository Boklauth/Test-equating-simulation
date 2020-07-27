library(equate)
# NEAT equating #####

# Coding Overview #####
# 1-prepare data #####
# 2-equating #####
# 3-calculate se and RSMD #####
# 4-Graphing #####



# 1-prepare data #####
# recap: 
# length of anchor test is
length(bv)
# length of test form is
formlength

# get bivariate freq. table for Form X and anchor test
library(equate)
nx1<-freqtab(df$x1, scales=list(0:formlength, 0:length(bv)))

# get bivariate freq. table for Form Y and anchor test from pop2
ny2<-freqtab(df$y2, scales=list(0:formlength, 0:length(bv)))

# get bivariate freq. table for Form Y and anchor test from pop2
ny3<-freqtab(df$y3, scales=list(0:formlength, 0:length(bv)))
ny3
plot(nx1)
plot(ny2)
plot(ny3)

# Form X with pop1, and Form Y with pop2 #####
# Linear Methods #####
# 1-Tucker linear #####
le1 <- equate(nx1, ny2, type = "linear", method = "tuck", ws =  1)
# view equated scores
le1$concordance

# 2-Levine-observed score-external anchor test
le2<-equate(nx1, ny2, type = "linear", method = "levine", ws = 1, 
            internal=FALSE)
# view equated scores
le2$conc

# 3-Levine-true score-external anchor test
le3<-equate(nx1, ny2, type = "linear", method = "levine", lts=TRUE, ws = 1, 
            internal=FALSE)
# view equated scores
le3$conc

# 4-chained
le4 <- equate(nx1, ny2, type = "linear", method = "chain", ws = 1)
# view equated scores
le4$conc 

# 5-Braun/Holland linear
le5 <- equate(nx1, ny2, type = "linear", method = "braun", ws = 1)
le5$conc

# combined scores form linear equating methods
tb.le<-round(cbind(xscale=0:formlength, 
             tucker=le1$conc$yx, 
             levos=le2$conc$yx, 
             levts=le3$conc$yx, 
             chain=le4$conc$yx, 
             BH=le5$conc$yx), 4)
View(tb.le)


# Equipercentile methods #####
# 5-frequency estimation #####
ep1 <- equate(nx1, ny2, type = "equip", method = "freq", ws = 1)
ep1$conc


# 6-chained frequency estimation #####
ep2<-equate(nx1, ny2, type = "equip", method = "chain")
ep2$concordance
# plot equated score and base form score
plot(le1, le2,le3, le4, le5, ep1, ep2, addident=FALSE)


# plot the score difference-simple graph
# will make it look nicer later
plot(0:formlength, le1$conc$yx-0:formlength, type='b', pch=1, 
     ylim=c(-6, 20), ylab="(varphi(x)-x)", xlab="Form X Score")
lines(0:formlength, le2$conc$yx-0:formlength, type='b', pch=2)
lines(0:formlength, le3$conc$yx-0:formlength, type='b', pch=3)
lines(0:formlength, le4$conc$yx-0:formlength, type='b', pch=4)
lines(0:formlength, le5$conc$yx-0:formlength, type='b', pch=5)
lines(0:formlength, ep1$conc$yx-0:formlength, type='b', pch=6)
lines(0:formlength, ep2$conc$yx-0:formlength, type='b', pch=7)
legend("topright", c("Tucker", "Levine-OS", "Levine-TS", "Chain", 
                     "Braun/Holland", "FE", "Chained FE"), lty=1,
       pch=c(1, 2, 3, 4, 5, 6, 7))

# IRT-equating #####
# IRT true score equating #####
library(SNSequate)
# reading on scale transformation first

