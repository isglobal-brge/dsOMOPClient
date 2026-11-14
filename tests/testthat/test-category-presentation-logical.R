# Complete the category axis with logical flags; the other shards cover
# integer flags, factor flags, and nominal categorical defaults.
for(date in c("day","month","relative")) .equivalence_matrix(date,"logical")
