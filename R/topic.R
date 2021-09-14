#' Find Optimal Topic Parameters
#'
#' @param corpus The corpus used for WarpLDA
#' @param lower The lower bound for k, alpha and beta (a 3-element numeric vector)
#' @param upper The upper bound for k, alpha and beta (a 3-element numeric vector)
#'
#' @return Returns a list with the optimal hyperparameters
#' @export
#' @references Mika V. Mantyla, Maelick Claes, and Umar Farooq. 2018. Measuring LDA Topic Stability from Clusters of Replicated Runs. In ACM / IEEE International Symposium on Empirical Software Engineering and Measurement (ESEM) (ESEM ’18), October 11–12, 2018, Oulu, Finland. ACM, New York, NY, USA, Article 4, 4 pages. https://doi.org/10.1145/3239235.3267435
find_optimal_topic_parameters <- function(corpus,lower,upper,stop_words){
  #Find optimal hyper priors alpha and beta that are used later
  optimalLda <- function (params,corpus){
    #t1 = Sys.time()
    # Use sink to hide verbose from text2vec.
    sink("NUL")
    m_k <- round (params[1])
    m_alpha <- params[2]
    m_beta <- params[3]


    sample <- sample.int(n = nrow(corpus), size = floor(.80*nrow(corpus)), replace = F)

    tokens <- corpus$narrative[sample]  %>% tokenize_words (lowercase = TRUE,
                                                            strip_punct = TRUE,
                                                            strip_numeric = TRUE,
                                                            stopwords = c(stop_words,"x","y","z",
                                                                          "xx","yy","X","Y","Z",
                                                                          "ZZZ","ZZZZ","zzz","zzzz",
                                                                          "zzz1"))
    it <- itoken(iterable = tokens,
                 progressbar = FALSE,
                 ids = corpus$ACN[sample])

    v <- create_vocabulary(it, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 10,
                                                                            doc_proportion_max = 0.3)

    vectorizer <- vocab_vectorizer(v)

    dtm <- create_dtm(it, vectorizer, type = "dgTMatrix")

    #Find correct hyper parameters.
    lda_model <- LDA$new(n_topics = m_k, doc_topic_prior = m_alpha, topic_word_prior = m_beta)

    doc_topic_distr <-
      lda_model$fit_transform(x = dtm, n_iter = 1000,
                              convergence_tol = 0.001, n_check_convergence = 25,
                              #convergence_tol = 0.01, n_check_convergence = 25,
                              progressbar = FALSE, verbose=FALSE)

    #apply to training set
    new_dtm <- itoken(corpus$narrative[-sample], tolower, word_tokenizer,
                      ids = corpus$ACN[-sample]) %>%
      create_dtm(vectorizer, type = "dgTMatrix")

    new_doc_topic_distr <- lda_model$transform(new_dtm)

    sink()

    perp <- perplexity(new_dtm,
                       topic_word_distribution = lda_model$topic_word_distribution,
                       doc_topic_distribution = new_doc_topic_distr)

    m_k <- round (params[1])
    m_alpha <- params[2]
    m_beta <- params[3]

    #print(paste("k:", m_k, "alpha:", m_alpha, "beta", m_beta,
    #            "perp:", perp, "time used: ", difftime(Sys.time(), t1, units = 'sec')))
    #difftime(Sys.time(), t1, units = 'sec')
    return(perp)

  }

  #Perform the DE search of optimal parameters.
  #Warning!!! takes time and resulting alpha and beta are documented lower

  # For many problems it is best to set 'NP' (in 'control') to be at
  # least ten times the length of the parameter vector.
  NP <- length(lower)*10
  optim_params <- DEoptim(fn=optimalLda,
                          lower = lower,
                          upper = upper,
                          control = DEoptim.control(strategy = 2, itermax = 10, NP = NP),
                          corpus = corpus)
  return(optim_params)
}

#' Create Topic-Term Matrix
#'
#' @param n_runs The number of runs to perform topic modeling
#' @param corpus The corpus used for WarpLDA
#' @param n_clusters The number of topics/clusters chosen
#' @param stop_words The list of stop words to be used
#' @export
create_topic_term_matrix <- function(n_runs,corpus,n_clusters,alpha,beta,stop_words){

  # Creates a list of character vectors, where each character vector is a tokenized document in the corpusrm
  #tokens <- corpus$narrative  %>%  tokenize_words (strip_numeric = TRUE)

  #tokens <- corpus$narrative  %>%  tokenize_word_stems(language = "english",
  #                                                       stopwords = c(stop_words,"x","y","z",
  #                                                                     "xx","yy","X","Y","Z",
  #                                                                     "ZZZ","ZZZZ","zzz","zzzz",
  #                                                                     "zzz1")

  sink("NUL")
  # Applies ASRS specific stop words
  tokens <- corpus$narrative  %>% tokenize_words (lowercase = TRUE,
                  strip_punct = TRUE,
                  strip_numeric = TRUE,
                  stopwords = c(stop_words,"x","y","z",
                                "xx","yy","X","Y","Z",
                                "ZZZ","ZZZZ","zzz","zzzz",
                                "zzz1"))

  # Create iterator for tokens
  it <- itoken(iterable = tokens,
               progressbar = FALSE,
               ids = corpus$ACN)

  # Create vocabulary (dataframe counting corpus and doc term frequency) and apply external stop words
  vocab <- create_vocabulary(it, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 10,
                                                                              doc_proportion_max = 0.3)

  # Maps Vocabulary to Vector Space - Only used to Create DTM or similar
  vectorizer <- vocab_vectorizer(vocab)

  # Document term matrix
  dtm <- create_dtm(it, vectorizer, type = "dgTMatrix")

  k <- n_clusters
  lda_params <- list(n_iter = 1000,
                     convergence_tol = 0.0001,
                     n_check_convergence = 100,
                     progressbar = FALSE)

  dtm_ttm_all <- create_n_runs_warp_lda(k,n_runs,alpha,beta,dtm,lda_params)

  sink()
  return(dtm_ttm_all)
}


#' WarpLDA Topic Modelling
#'
#' @param k An integer specifying the number of topics
#' @param n_runs The nuber of runs for LDA
#' @param alpha Alpha parameter as specified in WarpLDA
#' @param beta Beta parameter as specified in WarpLDA
#' @param dtm The document term matrix representation of the corpus
#' @param lda_params Additional parameters for WarpLDA
#' @param store_path Optional path to serialize topic modelling result
#' @references Chen, J., Li, K., Zhu, J., & Chen, W. (2016). WarpLDA: a Cache Efficient O(1) Algorithm for Latent Dirichlet Allocation. In VLDB.
create_n_runs_warp_lda <- function(k,n_runs,alpha,beta,dtm,lda_params,store_path = NULL){

  topic_term_distr_all <- NULL #variable to store topic-word matrix
  doc_topic_distr_all <- NULL #variable to store document-topic matrix

  for (i in 1:n_runs){
    lda_model <- LDA$new(n_topics = k,
                         doc_topic_prior = alpha,
                         topic_word_prior = beta)


    doc_topic_distr <- lda_model$fit_transform(x = dtm,
                                               n_iter = lda_params[[1]],
                                               convergence_tol = lda_params[[2]],
                                               n_check_convergence = lda_params[[3]],
                                               progressbar = lda_params[[4]])

    topic_term_distr <- lda_model$topic_word_distribution

    #tw_lift_dist <- metric_topic_term_lift(lda_model$topic_word_distribution,
    #                                       lda_model$components)

    topic_term_distr_all <- rbind(topic_term_distr_all, topic_term_distr)
    #tw_lift_dist_all <<- rbind(tw_lift_dist_all, tw_lift_dist)
    doc_topic_distr_all <- cbind(doc_topic_distr_all, doc_topic_distr)

    dtm_ttm_all <- list(topic_term_distr_all = topic_term_distr_all,
                        doc_topic_distr_all = doc_topic_distr_all)

  }
  if(!is.null(store_path)){saveRDS(file.path(store_path),"dtm_ttm_all.rds")}
  return(dtm_ttm_all)
}


