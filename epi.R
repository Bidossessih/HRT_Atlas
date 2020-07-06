
geneName$Epiregio =NA

load("/home/ubuntupc/Repository/Housekeeping and Reference Transcript Atlas/HRT_Atlas/www/EpiRegio_Gene.RData")

for (i in 1:nrow(geneName)) {
  data = data.frame(fromJSON(paste0("https://epiregio.de/REST_API/GeneQuery/",as.character(geneName[i,2]),"/")))
  
  if (nrow(data)>0) {
    geneName[i,3] = "Yes"
  }
  save(geneName, file = "../EpiRegio_Gene.RData")
  save(data, file = paste0("../Epiregio/",geneName[i,1], ".RData"))
  cat("processed", i, "\n")
}

Epigene = c()
for (i in 274:length(list.files("../Epiregio/"))) {
  
  load(paste0("../Epiregio/", list.files("../Epiregio/")[i]))
  
  if (nrow(data)>0) {
    save(data, file = paste0("../External/Data/Epiregio/", list.files("../Epiregio/")[i]))
    Epigene = unique(c(Epigene, gsub(".RData", "", list.files("../Epiregio/")[i])))
  }
  
  cat("processed", i, "\n")
}

geneName2 = unique(na.omit(merge(geneName, data.frame(geneSymbol=Epigene), by = "geneSymbol", all.y = T)))
geneName = geneName2[-1410,]

save(geneName, file = paste0("../External/Data/Epiregio/", "EpiRegio_Gene_HRTAtlas.RData"))
