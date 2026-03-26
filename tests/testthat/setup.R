# create variables used elsewhere in tests

# factor variable
fv <- factor(letters[1:10])

# numeric variable
nv <- 1:10

# hclust object for hclust and dendrogram methods
hc <- hclust(dist(UScitiesD), "ave")

#test phylogeny for phylo and phylo4 methods
tr <- ape::read.tree(text ="test(((sp1:1,sp2:1):2,sp3:2):2,sp4:5);")

tr4 <- phylobase::phylo4(tr)

