#Feral pig stage structured model
# Direct matrix entry
# Gopher model- 2x2 matrix with just sub-adults and adults. Assumes that all fecundity makes it to sub-adult stage
stages = c("Sub-adult", "Adult")
##p11 = probability of remaining a piglet, very low because move 
##stage classes or die
p12 =0.468 ##probability of sub-adult becoming an adult
p22 = 0.450 ##probability of adult staying an adult

f11 = 2.4 ##birth rate from sub-adults- low estimate
f21 = 5.35 ##birth rate from reproductive adults- low estimate

Gophers = matrix(data=c(f11, f21, p12, p22),
            nrow=2, ncol=2, byrow=TRUE,
            dimnames=list(stages,stages))

##These numbers based on min number found per hectare in alfalfa (13.5- Smallwood et al. 2001)
##and based on one year of normal breeding at given fecundities above. 
#Nsub-adults=104
#Nadults = 13.5
NSubadults = 0
NAdults = 7


N = c(NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers %*% N

##project population using popbio package, projection out 10 years
library(popbio)
gopher_project = pop.projection(Gophers, N,11)
stage.vector.plot(gopher_project$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gopher Populations/ha")

###to determine the stable stage distribution
stable.stage(Gophers)

###Sub-adult     Adult 
###0.8582509 0.1417491 

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers)
#3.283608

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers)
#         Sub-adult     Adult
#Sub-adult 0.5571624 0.2051306
#Adult     0.2051306 0.0325764
##transition from piglet to yearling is the largest,
##therefore it is the most proportionally influential in
##determining growth rate


#####################################################
#Running again with a 3x3 matrix including a low initial juvenile survival
#####################################################

stages = c("Juvenile", "SubAdult", "Adult")
p11 = 0.00 #probability of remaining a juvenile, very low because move 
##stage classes or die
p12 =0.09 ##probability of juvenile becoming a sub adult (low value!)
p22 = 0.00 ##probability of subadult staying a subadult
p23 = 0.468 ##probability of a subadult becoming reproductive adult
p33 = 0.450 ##probability of staying adult

f21 = 2.4 ##birth rate from subadults
f31 = 5.35 ##birth rate from reproductive adults

Gophers3 = matrix(data=c(p11, f21, f31, p12, p22, 0, 0, p23, p33),
              nrow=3, ncol=3, byrow=TRUE,
              dimnames=list(stages,stages))

#            Juvenile SubAdult Adult
#Juvenile     0.00    2.400  5.35
#SubAdult     0.09    0.000  0.00
#Adult        0.00    0.468  0.45

##These numbers taken from very preliminary results
#Npiglets = 57
#Nyearlings = 6.5
#Nadults = 6.5

##for illustration purposes, let's look at a population with 50 adults(reproducing)
##50 yearlings, and 250 piglets (assume an average of 5 piglets per litter, then
###2.5 female piglets per litter)
Njuveniles=0
NSubadults = 0
NAdults = 7


N = c(Njuveniles, NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers3 %*% N

##project population using popbio package, projection out 10 years
library(popbio)
gophers3_projection = pop.projection(Gophers3, N,11)
stage.vector.plot(gophers3_projection$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gophers/ha 3x3 matrix output")

gophers3_projection

###to determine the stable stage distribution
stable.stage(Gophers3)

###most abundant class is juveniles

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers3)

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers3)
##transition from piglet to yearling is the largest,
##therefore it is the most proportionally influential in
##determining growth rate


#####################################################
#Running again with a 3x3 matrix including a low initial juvenile survival, but high fecundity
#####################################################

stages = c("Juvenile", "SubAdult", "Adult")
p11 = 0.00 #probability of remaining a juvenile, very low because move 
##stage classes or die
p12 =0.09 ##probability of juvenile becoming a sub adult (low value!)
p22 = 0.00 ##probability of subadult staying a subadult
p23 = 0.468 ##probability of a subadult becoming reproductive adult
p33 = 0.450 ##probability of staying adult

f21 = 2.67 ##birth rate from subadults
f31 = 10.2 ##birth rate from reproductive adults

Gophers4 = matrix(data=c(p11, f21, f31, p12, p22, 0, 0, p23, p33),
                  nrow=3, ncol=3, byrow=TRUE,
                  dimnames=list(stages,stages))

#            Juvenile SubAdult Adult
#Juvenile     0.00    2.400  5.35
#SubAdult     0.09    0.000  0.00
#Adult        0.00    0.468  0.45

##These numbers taken from very preliminary results
#Npiglets = 57
#Nyearlings = 6.5
#Nadults = 6.5

##for illustration purposes, let's look at a population with 7 reproducing adult females. 
Njuveniles=0
NSubadults = 0
NAdults = 70


N = c(Njuveniles, NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers4 %*% N

##project population using popbio package, projection out 10 years
library(popbio)
gophers4_projection = pop.projection(Gophers4, N,11)
stage.vector.plot(gophers4_projection$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gophers/ha 3x3 matrix output")

gophers4_projection

###to determine the stable stage distribution
stable.stage(Gophers4)

###most abundant class is juveniles

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers4)

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers4)
##transition from piglet to yearling is the largest,
##therefore it is the most proportionally influential in
##determining growth rate


#####################################################
#Running again with a 3x3 matrix including a higher initial juvenile survival and high fecundity
#####################################################

stages = c("Juvenile", "SubAdult", "Adult")
p11 = 0.00 #probability of remaining a juvenile, very low because move 
##stage classes or die
p12 =0.18 ##probability of juvenile becoming a sub adult (double previous model)
p22 = 0.00 ##probability of subadult staying a subadult
p23 = 0.468 ##probability of a subadult becoming reproductive adult
p33 = 0.450 ##probability of staying adult

f21 = 2.67 ##birth rate from subadults
f31 = 10.2 ##birth rate from reproductive adults

Gophers5 = matrix(data=c(p11, f21, f31, p12, p22, 0, 0, p23, p33),
                  nrow=3, ncol=3, byrow=TRUE,
                  dimnames=list(stages,stages))

#            Juvenile SubAdult Adult
#Juvenile     0.00    2.400  5.35
#SubAdult     0.09    0.000  0.00
#Adult        0.00    0.468  0.45

##These numbers taken from very preliminary results
#Npiglets = 57
#Nyearlings = 6.5
#Nadults = 6.5

##for illustration purposes, let's look at a population with 7 reproducing adult females. 
Njuveniles=0
NSubadults = 0
NAdults = 7


N = c(Njuveniles, NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers5 %*% N

##project population using popbio package, projection out 10 years
library(popbio)
gophers5_projection = pop.projection(Gophers5, N,11)
stage.vector.plot(gophers5_projection$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gophers/ha, moderate juvenile survival, high fecundity")

gophers5_projection

###to determine the stable stage distribution
stable.stage(Gophers5)

###most abundant class is juveniles

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers5)

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers5)
##transition from piglet to yearling is the largest,
##therefore it is the most proportionally influential in
##determining growth rate

#####################################################
#Running again with a 3x3 matrix including a high initial juvenile survival and high fecundity
#####################################################

stages = c("Juvenile", "SubAdult", "Adult")
p11 = 0.00 #probability of remaining a juvenile, very low because move 
##stage classes or die
p12 =0.50 ##probability of juvenile becoming a sub adult (half of all juveniles survive to sub-adult)
p22 = 0.00 ##probability of subadult staying a subadult
p23 = 0.468 ##probability of a subadult becoming reproductive adult
p33 = 0.450 ##probability of staying adult

f21 = 2.67 ##birth rate from subadults
f31 = 10.2 ##birth rate from reproductive adults

Gophers6 = matrix(data=c(p11, f21, f31, p12, p22, 0, 0, p23, p33),
                  nrow=3, ncol=3, byrow=TRUE,
                  dimnames=list(stages,stages))

#            Juvenile SubAdult Adult
#Juvenile     0.00    2.400  5.35
#SubAdult     0.09    0.000  0.00
#Adult        0.00    0.468  0.45


##for illustration purposes, let's look at a population with 7 reproducing adult females. 
Njuveniles=0
NSubadults = 0
NAdults = 7


N = c(Njuveniles, NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers6 %*% N

##project population using popbio package, projection out 10 years
library(popbio)
gophers6_projection = pop.projection(Gophers6, N,11)
stage.vector.plot(gophers6_projection$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gophers/ha, moderate juvenile survival, high fecundity")

gophers6_projection

###to determine the stable stage distribution
stable.stage(Gophers6)

###most abundant class is juveniles

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers6)

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers6)
##transition from juvenile to subadult is the largest,
##therefore it is the most proportionally influential in
##determining growth rate

#####################################################
#Running again with a 3x3 matrix including a high initial juvenile survival and high fecundity
#####################################################

stages = c("Juvenile", "SubAdult", "Adult")
p11 = 0.00 #probability of remaining a juvenile, very low because move 
##stage classes or die
p12 =0.50 ##probability of juvenile becoming a sub adult (half of all juveniles survive to sub-adult)
p22 = 0.00 ##probability of subadult staying a subadult
p23 = 0.468 ##probability of a subadult becoming reproductive adult
p33 = 0.450 ##probability of staying adult

f21 = 2.4 ##birth rate from subadults
f31 = 5.35 ##birth rate from reproductive adults

Gophers7 = matrix(data=c(p11, f21, f31, p12, p22, 0, 0, p23, p33),
                  nrow=3, ncol=3, byrow=TRUE,
                  dimnames=list(stages,stages))

#            Juvenile SubAdult Adult
#Juvenile     0.00    2.400  5.35
#SubAdult     0.09    0.000  0.00
#Adult        0.00    0.468  0.45


##for illustration purposes, let's look at a population with 7 reproducing adult females. 
Njuveniles=0
NSubadults = 0
NAdults = 7


N = c(Njuveniles, NSubadults, NAdults)

# Projection
# Look at one step, using the sandpiper model:
N1 = Gophers7 %*% N

##project population using popbio package, projection out 10 years
library(popbio)
Gophers7_projection = pop.projection(Gophers7, N,11)
stage.vector.plot(Gophers7_projection$stage.vectors,proportions = FALSE, 
                  legend.coords="topleft", main = "10 Year Projected Gophers/ha, moderate juvenile survival, low fecundity")

Gophers7_projection

###to determine the stable stage distribution
stable.stage(Gophers7)

###most abundant class is juveniles

##determine lambda, telling us that once the population is
##at a stable stage distribution, then it will grow at x% 
##per year
lambda(Gophers7)

##to determine elasticities to see which stage is the 
##most important
elasticity(Gophers7)
##transition from juvenile to subadult is the largest,
##therefore it is the most proportionally influential in
##determining growth rate
