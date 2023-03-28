write_molecule_muster_version = function(pdb, dba, area.lim = 500,
                                         ids = c(1,1), force = T,
                                         title = T, pdb_out = T, 
                                         pdbpath = "pdb-mol", 
                                         pdbpathout = pdbpath){
  
  
  if (!dir.exists(pdbpathout)){
    dir.create(pdbpathout)
  }
  
  res.tab = dba$exp$residues[[ids[1]]][[ids[2]]]$res.table
  res.tab = res.tab %>% filter((area > area.lim & chain == "A") |
                                 (chain == "B"))
  
  #browser()
  aux1 = atom.select(pdb, resid = res.tab$resn)
  aux2 = atom.select(pdb, resno = as.numeric(res.tab$resi))
  aux12 = combine.select(aux1, aux2, operator="AND")
  
  pdb.out = pdb
  pdb.out$atom = pdb$atom[aux12$atom, ]
  pdb.out$xyz = pdb.out$xyz[aux12$xyz]
  pdb.out$calpha = pdb.out$atom$elety == "CA"
  
  res.out = write_pdb(pdb.out,
                      pdbpath = pdbpath,
                      force = force,
                      title = title,
                      pdb_out = pdb_out,
                      pdbpathout = pdbpathout)
  
  return(res.out)
}