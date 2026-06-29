# Load packages
library(remotes)

# Install packages from GitHub
remotes::install_github("jjesusfilho/tjsp")

library(tjsp)
library(dplyr)


# Define search keywords
busca <- c("abuso OR exploração AND sexual AND infantojuvenil AND internet",
           "estupro AND virtual", "ameaças AND massacres AND escolas",
           "estelionato AND fraude AND eletrônica", "extorsão AND cibernético",
           "furto AND fraude AND eletrônica",
           "invasão AND dispositivos AND informáticos",
           "cyberstalking", "cyberbullying", "revenge AND porn",
           "induzimento OR instigação OR auxílio AND suicídio OR 
           automutilação AND redes AND sociais", "perseguição AND virtual",
           "phishing", "ransomware", "malware", "spyware", "crime AND cibernético")

##### Extraction #####

# Function to search and read court cases
download_and_read_tjsp <- function(keyword) {
  directory <- "~/tjsp"
  
  # Create directory if it does not exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  tjsp_baixar_cjsg(livre = keyword,
                   n = max,
                   diretorio = directory)
  
  cjsg <- tjsp_ler_cjsg(diretorio = directory)
  return(cjsg)
}

# Run search and read court cases
results_tjsp <- lapply(busca, download_and_read_tjsp)

# Combine results into a single dataframe
tj <- do.call(rbind, results_tjsp)
View(tj)

# Remove duplicate rows
tj <- tj |>
  distinct(processo, .keep_all = TRUE)


# New dataframe to store the court rulings
tjsp_ruling <- data.frame()

# Directory to temporarily store the PDF files
directory <- "~/tjsp_acordao"

# Loop to process each court ruling
for (i in 1:nrow(tj)) {
  
  id <- tj$cdacordao[i]
  
  # Download the court ruling
  tjsp_baixar_acordaos_cjsg(id, diretorio = directory)
  
  # Read the downloaded court ruling
  ruling_read <- tjsp_ler_acordaos_cjsg(
    arquivos = NULL,
    diretorio = directory,
    remover_assinatura = TRUE,
    combinar = TRUE
  )
  
  # Append current result to the final dataframe
  tjsp_ruling <- rbind(tjsp_ruling, ruling_read)
  
  # Delete files from the directory
  files <- list.files(path = directory, full.names = TRUE)
  file.remove(files)
  
}

### Merge court rulings with the original dataframe
tjsp_ruling$cdacordao <- as.integer(tjsp_ruling$cdacordao)
tjsp_complete <- left_join(tj, tjsp_ruling, by = "cdacordao")

### Write the dataframe to a CSV file
write.csv(tjsp_complete, file = "tjsp_acordao.csv", row.names = FALSE)