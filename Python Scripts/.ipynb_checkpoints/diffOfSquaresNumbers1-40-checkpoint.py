"""
Find the numbers between 1 - 40 that can be represented as the difference of two perfect squares
e.g. 7^2 - 6^2 = 13, so 13 is one of the numbers
"""
from timeit import default_timer as timer

# empty list for candidates
candidates = [];

# counter starts at 20
i=20

# while counter is at least 0, calculate the difference between squares
# of the counter and another index that runs from 0 to 20
start=timer()
while(i >= 0):
    for j in range(20):
        candidates.append(pow(i,2)-pow(j,2))
    i-=1 # decrease counter
end=timer()
print("time for pow version = ", end-start)
# generate the unique list of results in candidates
result = list(set(candidates))

# print those that are between 1 and 40
print("The numbers 1 - 40 that can be determined via a difference of squares are: ", 
      [x for x in result if x in range(1,41)])   

# print number that were in the result list above
print("How many of them? ", len([x for x in result if x in range(1,41)]))    

#print the ones that CANNOT be written as a difference of squares
print("The ones that cannot :", [x for x in range(1,41) if x not in result])     
