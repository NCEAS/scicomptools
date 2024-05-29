#' @title Perform Text Mining of a Given Column
#'
#' @description Mines a user-defined column to create a dataframe that is ready for creating a word cloud. It also identifies any user-defined "bigrams" (i.e., two-word phrases) supplied as a vector.
#'
#' @param data (dataframe) Data object containing at least one column
#' @param text_column (character) Name of column in dataframe given to `data` that contains the text to be mined
#' @param word_count (numeric) Number of words to be returned (counts from most to least frequent)
#' @param known_bigrams (character) Vector of all bigrams (two-word phrases) to be mined before mining for single words
#'
#' @importFrom magrittr %>%
#'
#' @return dataframe of one column (named 'word') that can be used for word cloud creation. One row per bigram supplied in `known_bigrams` or single word (not including "stop words")
#'
#' @export
#'
#' @examples 
#' # Create a dataframe containing some example text
#' text <- data.frame(article_num = 1:6,
#'                    article_title = c("Why pigeons are the best birds",
#'                                      "10 ways to show your pet budgie love",
#'                                      "Should you feed ducks at the park?",
#'                                      "Locations and tips for birdwatching",
#'                                      "How to tell which pet bird is right for you",
#'                                      "Do birds make good pets?"))
#'                                      
#' # Prepare the dataframe for word cloud plotting              
#' word_cloud_prep(data = text, text_column = "article_title")
#' 

word_cloud_prep <- function(data = NULL, text_column = NULL,
                            word_count = 50, known_bigrams = c("working group")){
  # Squelch visible bindings note
  text_full <- word <- free_text <- keep <- bigrams <- NULL
  is_number <- response_id <- angle <- NULL

  # Error out if data is not supplied or column isn't supplied
  if(base::is.null(data) | base::is.null(text_column))
    stop("`data` and `text_column` arguments must be supplied.")

  # Perform actual text mining
  cloud_df <- data %>%
    # Split by line
    dplyr::mutate(text_full = stringr::str_split(data[[text_column]], pattern = '\\n')) %>%
    # Make each line a string
    tidyr::unnest(text_full) %>%
    # Remove extra spaces
    dplyr::mutate(text_full = stringr::str_trim(text_full)) %>%
    # Split words apart into bi-grams (2-word phrases)
    tidytext::unnest_tokens(output = word, input = text_full,
                            token = 'ngrams', n = 2, to_lower = T) %>%
    # Identify responses that contain our desired n-grams
    dplyr::mutate(keep = dplyr::case_when(
      word %in% known_bigrams ~ 'yes', T ~ 'no')) %>%
    # **Rename the provided text column for our downstream use**
    dplyr::rename(free_text = dplyr::all_of(text_column)) %>%
    # Identify whether a text entry has any bigrams
    dplyr::group_by(free_text) %>%
    dplyr::mutate(bigrams_in_phrase = dplyr::case_when(
      base::any(keep == 'yes', na.rm = T) ~ 'yes',
      T ~ 'no')) %>%
    # Keep only n-grams from wishlist OR phrases without any n-grams
    dplyr::filter(keep == 'yes' | base::all(keep == 'no',
                                            na.rm = T)) %>%
    # # Remove the "n-grams" that aren't real n-grams
    dplyr::mutate(word = base::ifelse(test = keep == 'no',
                                      yes = NA,
                                      no = word)) %>%
    # And strip down to one row per response
    base::unique() %>%
    # Rename the word column
    dplyr::rename(bigrams = word) %>%
    # Remove the bigrams from their original phrases
    dplyr::mutate(free_text = ifelse(test = (keep == "yes"),
                                     yes = stringr::str_replace(string = tolower(free_text),
                                                                pattern = bigrams, 
                                                                replacement = ""),
                                     no = free_text),
                  .before = dplyr::everything()) %>%
    # Remove grouping
    dplyr::ungroup() %>%
    # Make a column identifying response ID (we'll need it later)
    dplyr::mutate(response_id = as.character(seq_along(free_text))) %>%
    # Split words apart
    tidytext::unnest_tokens(output = word, input = free_text, token = 'words',
                            to_lower = T, drop = T) %>%
    # Keep only needed columns
    dplyr::select(word, bigrams, response_id) %>%
    # Pivot longer
    tidyr::pivot_longer(cols = -response_id, names_to = 'column_names', values_to = 'word') %>%
    # Drop duplicate values (extract bigrams are duplicated by unnesting responses)
    base::unique() %>%
    # And remove NA entries
    dplyr::filter(!is.na(word)) %>%
    # Keep only text columns
    dplyr::select(word) %>%
    # Remove stop words (that data object provided by `tidytext`)
    dplyr::anti_join(tidytext::stop_words, by = 'word') %>%
    # Identify numbers
    dplyr::mutate(is_number = base::ifelse(
      test = !base::is.na(base::suppressWarnings(base::as.numeric(word))),
      yes = "yes", no = "no")) %>%
    # Remove numbers
    dplyr::filter(is_number == "no") %>%
    # Drop the 'is number' column
    dplyr::select(-is_number) %>%
    # Make all words singular
    dplyr::rowwise() %>%
    dplyr::mutate(word = SemNetCleaner::singularize(word)) %>%
    dplyr::ungroup() %>%
    ## And then change some back to plural where it makes sense to do so
    dplyr::mutate(word = dplyr::case_when(
      word == "datum" ~ "data",
      word == "statistic" ~ "statistics",
      word == "ncea" ~ "nceas",
      # word == "" ~ "",
      T ~ as.character(word) ) ) %>%
    # Remove words (and "words") that aren't caught by the `anti_join`
    dplyr::filter(word != "" & word != "i.e.," & word != "i.e." & word != "i.e" &
                    word != "ie" & word != "e.g.," & word != "e.g." & word != "e.g" &
                    word != "eg" & word != "pre" & word != "lot" & word != "many") %>%
    # Count all words
    dplyr::count(word) %>%
    # Take only the first X many
    dplyr::slice(1:word_count) %>%
    # Add random angles to words
    dplyr::mutate(angle = 45 * sample(-2:2, dplyr::n(),
                                      replace = T,
                                      prob = c(1, 1, 4, 1, 1))) %>%
    # Add a column for use as color factor
    dplyr::mutate(color_groups = factor(sample.int(10, length(angle),
                                                   replace = T))) %>%
    # Make sure that the most frequent word isn't rotated
    dplyr::mutate(angle = dplyr::case_when(n == base::max(n) ~ 0,
                                    T ~ base::as.numeric(angle)))

  # Return that data object
  return(cloud_df)
}


#' @title Text Mine a Given Column and Create a Word Cloud
#'
#' @description Mines a user-defined column of text and creates a word cloud from the identified words and bigrams.
#'
#' @param data dataframe containing at least one column
#' @param text_column character, name of column in dataframe given to `data` that contains the text to be mined
#' @param word_count numeric, number of words to be returned (counts from most to least frequent)
#' @param known_bigrams character vector, all bigrams (two-word phrases) to be mined before mining for single words
#'
#' @importFrom magrittr %>%
#'
#' @return dataframe of one column (named 'word') that can be used for word cloud creation. One row per bigram supplied in `known_bigrams` or single word (not including "stop words")
#'
#' @export
#'

word_cloud_plot <- function(data = NULL, text_column = NULL,
                            word_count = 50, known_bigrams = c("working group")){
  # Squelch visible bindings note
  word <- n <- color_groups <- angle <- NULL

  # Prepare text for word cloud creation
  cloud_df <- word_cloud_prep(data = data, text_column = text_column,
                           word_count = word_count,
                           known_bigrams = known_bigrams)
    # Make plot
  cloud <- cloud_df %>%
    ggplot2::ggplot(ggplot2::aes(label = word, size = n,
                                 color = color_groups,
                                 angle = angle)) +
    ggwordcloud::geom_text_wordcloud(shape = 'circle',
                                     rm_outside = T,
                                 na.rm = T) +
    ggplot2::scale_size_area(max_size = 5) +
    ggplot2::theme_minimal()

  # And give the plot back
  return(cloud)
}
