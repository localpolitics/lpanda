# Pomocna funkce, ktera vykresluje graf do docasneho PDF (aby se nezobrazovaly)
quiet_plot <- function(fun) {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({
    try(grDevices::dev.off(), silent = TRUE)
    unlink(tf, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  fun()
  invisible(NULL)
}

# --------------------------------------------------------------------------- #

df <- lpanda::sample_data;
nd <- suppressMessages(suppressWarnings(prepare_network_data(df, verbose = FALSE)));

# --------------------------------------------------------------------------- #

test_that("function returns NULL invisibly", {
  res <- quiet_plot(function() plot_continuity(nd, do_not_print_to_console = TRUE));
  testthat::expect_null(res)
})

test_that("it accepts data frame as input data", {
  expect_no_error(quiet_plot(function() plot_continuity(df, do_not_print_to_console = TRUE)));
})

test_that("it accepts list as input data", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, do_not_print_to_console = TRUE)));
})

test_that("it fails if you put a non-existing range of elections", {
  expect_error(quiet_plot(function() plot_continuity(nd, elections = "-13", do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, elections = "13-15", do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, elections = c(14,22), do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, elections = "18-", do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, elections = "-2002", do_not_print_to_console = TRUE)));
  expect_error(quiet_plot(function() plot_continuity(nd, elections = "2002-", do_not_print_to_console = TRUE)));
})

test_that("links accepts only allowed values", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, links = "continuity",
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, links = "all",
                                                        do_not_print_to_console = TRUE)));
  expect_error(plot_continuity(nd, links = "nope"))
})

test_that("lists accepts only allowed values", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, lists = "all",
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, lists = "elected",
                                                        do_not_print_to_console = TRUE)));
  expect_error(plot_continuity(nd, lists = "nope"));
})

test_that("parties does not error if not existed party IDs are entered", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, parties = 1000,
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, parties = c(1,2,5),
                                                        do_not_print_to_console = TRUE)));
})

test_that("order_lists_by and order_groups_by validate values and skip those that are not in data", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, order_lists_by = "votes",
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, separate_groups = TRUE,
                                                        order_groups_by = c("elections","votes","seats"),
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, separate_groups = TRUE,
                                                        order_groups_by = "none",
                                                        do_not_print_to_console = TRUE)));
  expect_error(plot_continuity(nd, order_lists_by = "nope", do_not_print_to_console = TRUE));
  expect_error(plot_continuity(nd, separate_groups = TRUE, order_groups_by = "nope",
                               do_not_print_to_console = TRUE));
})

test_that("mark='parties' runs and that invalid group_colour length produce warning", {
  expect_no_error(quiet_plot(function() plot_continuity(nd, mark = "parties",
                                                        show_legend = FALSE,
                                                        do_not_print_to_console = TRUE)));
  expect_warning(quiet_plot(function() plot_continuity(nd, mark = "parties", group_colours = c("red"),
                                                       do_not_print_to_console = TRUE)));
})

test_that("candidate highlight does not error if candidate is not specified or does not exist", {
  
  expect_no_error(quiet_plot(function() plot_continuity(nd, mark = c("candidate"),
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, mark = c("candidate",
                                                                     sample(nd$candidates$node_attr$vertices, 1)),
                                                        do_not_print_to_console = TRUE)));
  expect_no_error(quiet_plot(function() plot_continuity(nd, mark = c("candidate", "Kevin the Minion"),
                                                        do_not_print_to_console = TRUE)));
})
