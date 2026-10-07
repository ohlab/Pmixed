isadmixed <- function(speciestree, n, tipagrD){
  agrDtypes <- tipagrD[clade.members(n, speciestree)]
  agrDtypes <- agrDtypes[which(agrDtypes!="NA" & agrDtypes!="")]
  
  return(length(unique(agrDtypes))>1)
}

computePmixed_topological <- function(speciestree, tipagrD){
  # go through each edge
  Uniquelength = 0
  Admixedlength = 0
  
  for(i in 1:nrow(speciestree$edge)){
    tonode = speciestree$edge[i,2]
    if(tonode <= Ntip(speciestree)){
      next
    }
    if(isadmixed(speciestree, tonode, tipagrD)){
      Admixedlength = Admixedlength + 1
    } else {
      Uniquelength = Uniquelength + 1
    }
  }
  return(Admixedlength/(Admixedlength+Uniquelength))
}

computePmixed_heightadjusted <- function(speciestree, tipagrD){
  # go through each edge
  Uniquelength = 0
  Admixedlength = 0
  
  for(i in 1:nrow(speciestree$edge)){
    fromnode = speciestree$edge[i,1]
    tonode = speciestree$edge[i,2]
    if(tonode <= Ntip(speciestree)){
      next
    }
    if(isadmixed(speciestree, tonode, tipagrD)){
      Admixedlength = Admixedlength + nodeheight(speciestree, tonode)
    } else {
      Uniquelength = Uniquelength + nodeheight(speciestree, tonode)
    }
  }
  return(Admixedlength/(Admixedlength+Uniquelength))
}
