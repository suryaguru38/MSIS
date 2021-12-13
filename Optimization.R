library(lpSolve)

####PART1###############

## Objective
C <- c(1.3, 0.6, 0.5)


## Constraints
A <- matrix(c(0.3, 0.15, 0.1,
              0.09, 0.03, 0.04,
              1, 0, 0,
              1.2,0.2,0,
              0.5,0.2,0.2,
              0,0.04,0.12), nrow=6, byrow=TRUE)


## Limits
B <- c(4, 1, 5,5,5,1.49)


## Equalities/Inequalities
constranints_direction  <- c("<=", "<=", "<=",">=",">=","<=")


## Optimum
optimum <-  lp(direction="max",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

print(optimum$status)


## Solution
best_sol <- optimum$solution
names(best_sol) <- c("TV", "Magazine", "Newspaper") 
print(best_sol)


## Result
print(paste("Total Exposure: ", optimum$objval, sep=""))

####PART2###############

## profits using part a solutiom
tv_profit = -0.1 * (best_sol[1])^2 + 1.13*best_sol[1] -0.04
mag_profit = -0.002 * (best_sol[2])^2 + 0.124 * best_sol[2] + 0.14
news_profit = -0.0321*(best_sol[3])^2 + 0.706*best_sol[3] -0.09
profit = (tv_profit+mag_profit+news_profit)*5 
print(paste("Profit using part a optimization: ", profit, sep=""))
library(CVXR)



# Problem definition
x <- Variable(1,integer=TRUE)
y <- Variable(1,integer=TRUE)
z <- Variable(1,integer=TRUE)
objective <- Maximize( (-0.1 * (x)^2 + 1.13*x -0.04 -0.002 * (y)^2 + 0.124 * y + 0.14 -0.0321*(z)^2 + 0.706*z -0.09)*5)
constraints <- list(x <= 5, 
                    0.3*x + 0.15*y + 0.1*z <= 4,
                    0.09*x+ 0.03*y+ 0.04*z <=1,
                    1.2*x+0.2*y>=5,
                    0.5*x + 0.2*y + 0.2*z >=5,
                    0.04*y + 0.12*z <= 1.49)
prob2.1 <- Problem(objective, constraints)

# Problem solution
solution2.1 <- solve(prob2.1)
solution2.1$status

best_sol2 <- c(solution2.1$getValue(x),solution2.1$getValue(y),solution2.1$getValue(z))
names(best_sol2) <- c("TV", "Magazine", "Newspaper") 
print(round(best_sol2,3))

tv_profit = -0.1 * (best_sol2[1])^2 + 1.13*best_sol2[1] -0.04
mag_profit = -0.002 * (best_sol2[2])^2 + 0.124 * best_sol2[2] + 0.14
news_profit = -0.0321*(best_sol2[3])^2 + 0.706*best_sol2[3] -0.09
profit2 = (tv_profit+mag_profit+news_profit)*5 
print(paste("Profit using part b optimization: ", profit2, sep=""))
