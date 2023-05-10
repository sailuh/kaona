#' OSLOM Community Detection
#'
#' @description Wrapper for OSLOM Community Detection \url{http://oslom.org/}
#' @param edgelist An igraph edgelist object
#' @param oslom_bin_undir_path The path to oslom undirected network binary
#' @param seed An integer specifying the seed to replicate the result
#' @export
#' @references Finding statistically significant communities in networks A. Lancichinetti, F. Radicchi, J.J. Ramasco and S. Fortunato PLoS ONE 6, e18961 (2011).
cluster_oslom <- function(edgelist,oslom_bin_undir_path,seed){
  oslom_bin_undir_path <- path.expand(oslom_bin_undir_path)
  mapping_names <- unique(c(as.character(edgelist$id),edgelist$value))
  mapping_ids <- 1:length(mapping_names)
  names(mapping_ids) <- mapping_names
  oslom_edgelist <- data.table(mapping_ids[as.character(edgelist$id)],
                               mapping_ids[edgelist$value])

  fwrite(oslom_edgelist,"/tmp/asrs_oslom_edgelist.txt",
         sep = "\t",
         row.names = FALSE,
         col.names = FALSE)

  system2(
    oslom_bin_undir_path,
    args = c('-f', '/tmp/asrs_oslom_edgelist.txt', '-uw','-seed',seed),
    stdout = FALSE,
    stderr = FALSE
  )
  f_con <- file("/tmp/asrs_oslom_edgelist.txt_oslo_files/tp")
  tp_raw <- readLines(f_con)
  close(f_con)
  cluster_metadata <- str_match(tp_raw,"#module (.+) size: (.+) bs: (.+)")

  #create function on sapply and rep to fill each element as dt

  is_node_line <- which(is.na(cluster_metadata[,1]))
  is_cluster_line <- which(!is.na(cluster_metadata[,1]))

  cluster_id <- cluster_metadata[is_cluster_line,2]
  cluster_size <- as.integer(cluster_metadata[is_cluster_line,3])
  cluster_pvalue <- cluster_metadata[is_cluster_line,4]
  cluster_nodes <- tp_raw[is_node_line]
  #cluster <- data.table(cluster_id,cluster_size,cluster_pvalue,cluster_nodes)

  cluster_nodes_list <- str_split(cluster_nodes," ")
  names(cluster_nodes_list) <- cluster_id
  # Remove "" strings added as ending element, create data.table and assign ids
  prepare_data_table <- function(name,x){
    dt <- data.table(node_id=x[[name]][x[[name]] != ""],cluster_id=name)
    return(dt)
  }
  cluster_assignment <- rbindlist(lapply(names(cluster_nodes_list),
                                         prepare_data_table,
                                         cluster_nodes_list))
  # Replace temporary id by original values
  cluster_assignment$node_id <- mapping_names[as.numeric(cluster_assignment$node_id)]
  cluster <- list()
  cluster[["assignment"]] <- cluster_assignment
  cluster[["info"]] <- data.table(cluster_id,cluster_size,cluster_pvalue)
  return(cluster)
}
