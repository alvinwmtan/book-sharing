# Lexical diversity - MTLD ####
make_krp_text <- function(data_df) {
  data_formatted <- data_df |>
    mutate(lttr = sapply(gloss, str_length),
           desc = NA,
           stop = NA,
           stem_ = NA,
           uID = uID + 1) |>
    select(c(doc_id = file_name, 
             token = gloss, 
             tag = part_of_speech,
             lemma = stem,
             lttr,
             wclass = part_of_speech,
             desc,
             stop,
             stem = stem_,
             idx = token_no,
             sntc = uID))
  
  kRp_text(lang = "en", token = data_formatted)
}

get_krp_ld_vals <- function(krp_text_ld) {
  sapply(krp_text_ld, summary) |> t() |> 
    as.data.frame() |> rownames_to_column() |> 
    unnest(cols = c(index, value)) |>
    rename(file_name = rowname) |>
    mutate(index = as.factor(index),
           value = as.numeric(value))
}

get_ld_vals <- function(data_df, measure = c("MATTR", "MTLD")) {
  data_krp <- make_krp_text(data_df)
  data_ld <- lex.div(data_krp, measure = measure, char = "", quiet = TRUE)
  data_ld_vals <- get_krp_ld_vals(data_ld)
  return(list(data_krp, data_ld, data_ld_vals))
}

get_ld <- function(data_df, measure = c("MATTR", "MTLD")) {
  get_ld_vals(data_df, measure)[[3]] |>
    rename(metric = index)
}

# Lexical density - Lexicality ####
get_lexicality <- function(data_df) {
  data_df |> 
    mutate(pos_min = sub(":.*", "", .data$part_of_speech),
           is_lexical = pos_min %in% c("n", "v", "adj", "part", "imp") & 
             !(part_of_speech %in% c("n:prop", "n:let")) &
             !(stem %in% c("be", "do", "get", "have", "need", "want"))) |>
    group_by(file_name) |>
    summarise(value = sum(is_lexical) / n()) |> 
    mutate(metric = "lexical_density")
}

# Lexical sophistication - Frequency and contextual diversity ####
SUBTLEX_N_TOKENS = 51010983
SUBTLEX_N_MOVIES = 8388
subtlex_en <- read_xlsx(here("resources", "SUBTLEXusExcel2007.xlsx")) |> 
  mutate(Word = tolower(Word),
         language = "English") |> 
  select(c(gloss = Word, freq = SUBTLWF, cd = SUBTLCD, language)) |> 
  mutate(log_freqn = log10(freq + 1e6 / SUBTLEX_N_TOKENS),
         log_cdn = log10(cd + 100 / SUBTLEX_N_MOVIES))

ESPAL_N_TOKENS = 462611693
ESPAL_N_MOVIES = 40444
subtlex_es <- read_csv(here("resources", "EsPal.csv")) |> 
  mutate(Word = tolower(Word),
         language = "Spanish") |> 
  select(c(gloss = Word, cnt = SUBTLWF, doc_cnt = SUBTLCD, language)) |> 
  mutate(freq = 1e6 * cnt / ESPAL_N_TOKENS,
         cd = 100 * doc_cnt / ESPAL_N_MOVIES,
         log_freqn = log10(freq + 1e6 / ESPAL_N_TOKENS),
         log_cdn = log10(cd + 100 / ESPAL_N_MOVIES)) |> 
  select(-cnt, -doc_cnt)

subtlex <- bind_rows(subtlex_en, subtlex_es)

get_sophistication <- function(data_df) {
  data_df |> 
    left_join(subtlex, by = c("gloss", "language")) |>
    group_by(file_name) |>
    summarise(freq = mean(log_freqn, na.rm = TRUE),
              cd = mean(log_cdn, na.rm = TRUE)) |> 
    pivot_longer(cols = c(freq, cd), names_to = "metric", values_to = "value")
}

# Grammatical complexity - MLU-w and proportion complex utterances ####
get_lu <- function(data_df) {
  lu <- data_df |>
    group_by(uID, file_name) |>
    summarise(lu = n())
  data_df |>
    left_join(lu, by = c("uID", "file_name"))
}

get_mlu <- function(data_df) {
  token_mlu <- data_df |>
    get_lu() |>
    group_by(gloss) |>
    summarise(mlu = mean(lu))
  data_df |>
    left_join(token_mlu, by = "gloss") |>
    group_by(file_name) |>
    summarise(mlu = mean(mlu))
}

get_mlu_nontoken <- function(data_df) {
  lu <- data_df |>
    group_by(uID, file_name) |>
    summarise(lu = n()) |>
    group_by(file_name) |>
    summarise(value = mean(lu)) |> 
    mutate(metric = "mlu")
  return(lu)
}

get_complexity <- function(data_df) {
  data_df |> 
    mutate(pos_min = sub(":.*", "", .data$part_of_speech),
           is_lexical_v = pos_min %in% c("v") & 
             !(stem %in% c("be", "do", "get", "have", "need", "want"))) |>
    group_by(uID, file_name) |>
    summarise(is_complex = sum(is_lexical_v) > 1) |>
    group_by(file_name) |>
    summarise(value = sum(is_complex) / n()) |> 
    mutate(metric = "prop_complex")
}

make_descriptives <- function(vals) {
  vals |> 
    filter(is.finite(value)) |> 
    group_by(metric, language, source) |> 
    summarise(median = median(value, na.rm = TRUE),
              mad = median(abs(value - median(value, na.rm = TRUE)), na.rm = TRUE),
              min = min(value),
              max = max(value))
}
