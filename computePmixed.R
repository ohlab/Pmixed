is_admixed <- function(tree, n, tip_AIP_type){
  clade_AIP_types <- tip_AIP_type[clade.members(n, tree)]
  clade_AIP_types <- clade_AIP_types[which(clade_AIP_types!="NA" & clade_AIP_types!="")]
  
  return(length(unique(clade_AIP_types))>1)
}

compute_Pmixed_topological <- function(tree, tip_AIP_type){
  unique_length = 0
  admixed_length = 0
  
  for(i in 1:nrow(tree$edge)){
    to_node = tree$edge[i,2]
    if(to_node <= Ntip(tree)){
      next
    }
    if(is_admixed(tree, to_node, tip_AIP_type)){
      admixed_length = admixed_length + 1
    } else {
      unique_length = unique_length + 1
    }
  }
  return(admixed_length/(admixed_length+unique_length))
}

compute_Pmixed_heightadjusted <- function(tree, tip_AIP_type){
  unique_length = 0
  admixed_length = 0
  
  for(i in 1:nrow(tree$edge)){
    to_node = tree$edge[i,2]
    if(to_node <= Ntip(tree)){
      next
    }
    if(is_admixed(tree, to_node, tip_AIP_type)){
      admixed_length = admixed_length + nodeheight(tree, to_node)
    } else {
      unique_length = unique_length + nodeheight(tree, to_node)
    }
  }
  return(admixed_length/(admixed_length+unique_length))
}
