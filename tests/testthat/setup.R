# create variables used elsewhere in tests

# factor variable
fv <- factor(letters[1:10])

# numeric variable
nv <- 1:10

# hclust object for hclust and dendrogram methods
hc <- hclust(dist(UScitiesD), "ave")
