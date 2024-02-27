getEmbedding <- function(x) {
  # set up openai
  org.id <- read_lines("/Dedicated/jmichaelson-sdata/API-keys/OpenAI-organization-id")
  api.key <- read_lines("/Dedicated/jmichaelson-sdata/API-keys/OpenAI")
  
  # Split x into chunks of size 1000 or less
  n <- length(x)
  chunk_size <- 1000
  num_chunks <- ceiling(n / chunk_size)
  
  # Initialize an empty list to store embeddings for each chunk
  embeddings_list <- vector("list", num_chunks)
  
  for (i in 1:num_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n)
    chunk <- x[start_idx:end_idx]
    
    # Get embeddings for the current chunk
    emb_chunk <- openai::create_embedding(input = chunk, 
                                          model = "text-embedding-3-small", 
                                          openai_api_key = api.key, openai_organization = org.id)
    emb_chunk <- do.call('rbind', emb_chunk$data$embedding)
    rownames(emb_chunk) = chunk
    
    # Store the embeddings in the list
    embeddings_list[[i]] <- emb_chunk
  }
  
  # Merge the embeddings from all chunks
  emb <- do.call('rbind', embeddings_list)
  rownames(emb) = x
  return(emb)
}
