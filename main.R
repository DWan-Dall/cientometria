# install.packages(c("bibliometrix"))
library(bibliometrix)

# 1) Importar e unir (ex.: Scopus CSV, WoS Plain Text)
scopus <- convert2df("scopus_export.csv", dbsource = "scopus", format = "csv")
wos    <- convert2df("wos_export.txt",     dbsource = "wos",    format = "plaintext")
openalex <- convert2df("openalex.bib",     dbsource = "openalex", format = "bibtex")

M <- mergeDbSources(list(scopus, wos, openalex), remove.duplicated = TRUE)

# 2) Descritivos
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 20, pause = FALSE)

# 3) Redes
# Co-palavra (Keywords Plus + Author Keywords)
Net_kw <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
networkPlot(Net_kw, normalize = "association", type = "fruchterman", size = TRUE, n = 50, labelsize = 0.7)

# Co-citação (referências)
Net_cocit <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
networkPlot(Net_cocit, n = 50, type = "fruchterman", size = TRUE, labelsize = 0.7)

# Acoplamento bibliográfico (documentos)
Net_bibcoupling <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
networkPlot(Net_bibcoupling, n = 50, type = "fruchterman", size = TRUE, labelsize = 0.7)

# 4) Mapa temático (co-palavra)
thematicMap(M, field = "ID", minfreq = 5, stemming = FALSE, nLabels = 4)
