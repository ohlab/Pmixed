isadmixed <- function(tree, n, tip_AIP_type){
  clade_AIP_types <- tip_AIP_type[clade.members(n, tree)]
  clade_AIP_types <- clade_AIP_types[which(clade_AIP_types!="NA" & clade_AIP_types!="")]
  
  return(length(unique(clade_AIP_types))>1)
}

computePmixed_topological <- function(tree, tip_AIP_type){
  # go through each edge
  Uniquelength = 0
  Admixedlength = 0
  
  for(i in 1:nrow(tree$edge)){
    tonode = tree$edge[i,2]
    if(tonode <= Ntip(tree)){
      next
    }
    if(isadmixed(tree, tonode, tip_AIP_type)){
      Admixedlength = Admixedlength + 1
    } else {
      Uniquelength = Uniquelength + 1
    }
  }
  return(Admixedlength/(Admixedlength+Uniquelength))
}

computePmixed_heightadjusted <- function(tree, tip_AIP_type){
  # go through each edge
  Uniquelength = 0
  Admixedlength = 0
  
  for(i in 1:nrow(tree$edge)){
    tonode = tree$edge[i,2]
    if(tonode <= Ntip(tree)){
      next
    }
    if(isadmixed(tree, tonode, tip_AIP_type)){
      Admixedlength = Admixedlength + nodeheight(tree, tonode)
    } else {
      Uniquelength = Uniquelength + nodeheight(tree, tonode)
    }
  }
  return(Admixedlength/(Admixedlength+Uniquelength))
}
