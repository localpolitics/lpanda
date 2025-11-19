# --------------------------------------------------------------------------- #
# Kontrola, ze verbose funguje ve vsech pripadech
# --------------------------------------------------------------------------- #

data(sample_data, package = "lpanda");
df <- sample_data;

test_that("verbose = FALSE produces no console output", {
  expect_silent(prepare_network_data(df, verbose = FALSE, quick = TRUE))
})

test_that("verbose = TRUE produces some output", {
  expect_output(suppressMessages(suppressWarnings(prepare_network_data(df, verbose = TRUE,
                                                                       quick = TRUE))))
})

# --------------------------------------------------------------------------- #
# Uvodni cast kontrolujici vstupni data
# --------------------------------------------------------------------------- #

result <- prepare_network_data(df, verbose = FALSE, quick = TRUE);

test_that("it will stop if any required variable is missing", {
  
  df_without_elections <- df[,!(names(df) %in% "elections")];
  expect_error(prepare_netowork_data(df_without_elections, skip_groups = TRUE,
                                     verbose = FALSE));
  
  df_without_candidates <- df[,!(names(df) %in% "candidate")];
  expect_error(prepare_network_data(df_without_candidates, skip_groups = TRUE,
                                    verbose = FALSE));
  
  df_without_list_name <- df[,!(names(df) %in% "list_name")];
  expect_error(prepare_network_data(df_without_list_name, skip_groups = TRUE,
                                    verbose = FALSE));
  
})

# --------------------------------------------------------------------------- #

test_that("it stops when required variables contains any missing or empty value", {
  
  required_cols <- variables_meta$var_name[variables_meta$required == TRUE
                                           & variables_meta$role == "input"];
  
  for (sloupec in required_cols) {
    
    df_test <- df;
    df_test[[sloupec]][sample(1:nrow(df_test),1)] <- "";
    expect_error(prepare_network_data(df_test, skip_groups = TRUE,
                                      verbose = FALSE));
    
    df_test <- df;
    df_test[[sloupec]][sample(1:nrow(df_test),1)] <- NA;
    expect_error(prepare_network_data(df_test, skip_groups = TRUE,
                                      verbose = FALSE));
    
  }
})

# --------------------------------------------------------------------------- #
# Testy vystupu vzhledem k edgelistum
# --------------------------------------------------------------------------- #

test_that("prepare_network_data() returns expected list structure", {
  
  expect_type(result, "list");
  expect_named(result, c("bipartite", "candidates", "lists", "continuity",
                         "parties", "elections"),
               ignore.order = TRUE);
})

test_that("Each network contains edgelist and node_attr", {
  
  for (net in c("bipartite", "candidates", "lists", "continuity", "elections")) {
    expect_named(result[[net]], c("edgelist", "node_attr"));
    expect_s3_class(result[[net]]$edgelist, "data.frame");
    expect_s3_class(result[[net]]$node_attr, "data.frame");
  };
})

test_that("Bipartite edgelist has required columns", {
  
  expect_true(all(c("from", "to", "elections") %in% names(result$bipartite$edgelist)))
  
})

test_that("Other edgelists have from, to, weight", {
  
  for (net in c("candidates", "lists", "continuity", "elections")) {
    expect_true(all(c("from", "to", "weight") %in% names(result[[net]]$edgelist)))
  }
})

test_that("List, continuity and elections networks are directed forward in time", {
  
  lists <- result$lists$edgelist;
  cont <- result$continuity$edgelist;
  elect <- result$elections$edgelist;
  
  # list_id se nahradi volbami
  for (el in list(lists, cont)) {
    
    el$from <- df$elections[match(el$from,
                                  paste0(df$list_name, " (", df$elections, ")"))];
    el$to <- df$elections[match(el$to,
                                paste0(df$list_name, " (", df$elections, ")"))];
  }
  
  expect_true(all(lists$from < lists$to))
  expect_true(all(cont$from < cont$to))
  expect_true(all(elect$from < elect$to))
})


test_that("No NA values in edgelists", {
  
  for (net in names(result)) {
    expect_false(any(is.na(result[[net]]$edgelist)))
  }
})

# --------------------------------------------------------------------------- #
# Testovani, ze zadani jader nezpusobi error
# --------------------------------------------------------------------------- #

df_cores <- sample_data[, c("elections", "candidate", "list_name")];

test_that("including 'cores' will not cause an error", {
  
  expect_no_error(prepare_network_data(df, include_cores = TRUE,
                                       verbose = FALSE, quick = TRUE));
  expect_no_error(prepare_network_data(df, include_cores = TRUE, core_type = 1,
                                       verbose = FALSE, quick = TRUE));
  expect_no_error(prepare_network_data(df, include_cores = TRUE, core_type = 2,
                                       verbose = FALSE, quick = TRUE));
  expect_no_error(prepare_network_data(df, include_cores = TRUE, core_type = 3,
                                       verbose = FALSE, quick = TRUE));
  expect_no_error(prepare_network_data(df, include_cores = TRUE, core_type = 4,
                                       verbose = FALSE, quick = TRUE));
  
})
