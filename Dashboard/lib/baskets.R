# Market-basket analysis.
#
# Basket definition (a proxy, not a verified order): all line items sold at
# the same store at the same second. Evidence for the proxy: in every such
# group with 2+ lines the transaction IDs are consecutive, i.e. the lines were
# rung up back to back. It can still merge two customers served in the same
# second or split one order rung up across seconds.

#' Build presence/absence baskets from line items.
#'
#' A product rung up on several lines of one basket counts once (presence,
#' not quantity), which is what support and confidence are defined on.
#'
#' @param min_items Minimum number of distinct products for a basket to be
#'   eligible. 1 (default) keeps single-item baskets so that support is a
#'   share of all baskets and lift compares against overall prevalence.
build_baskets <- function(data, min_items = 1) {
  stopifnot(min_items >= 1)
  if (nrow(data) == 0) {
    return(list(n_baskets = 0L, n_multi = 0L, n_eligible = 0L, n_repeat_lines = 0L,
                min_items = min_items, transactions = NULL))
  }
  raw_baskets <- split(data$product_detail, data$basket_id)
  baskets <- lapply(raw_baskets, unique)
  sizes <- lengths(baskets)
  eligible <- baskets[sizes >= min_items]
  list(
    n_baskets = length(baskets),
    n_multi = sum(sizes >= 2),
    n_eligible = length(eligible),
    # Lines dropped because the same product was already in the basket
    n_repeat_lines = sum(lengths(raw_baskets)) - sum(sizes),
    min_items = min_items,
    transactions = if (length(eligible) > 0) methods::as(unname(eligible), "transactions") else NULL
  )
}

#' Mine association rules and report how many were generated and kept.
#'
#' Rules are kept when they meet minimum support and confidence, have lift
#' above `min_lift`, and are not redundant (a more general rule with the same
#' or higher confidence exists). Each kept rule gets a one-sided Fisher exact
#' test p-value for positive association, adjusted across the kept rules with
#' Holm's method.
#'
#' @return list(rules = data frame or NULL, n_generated, n_retained)
mine_rules <- function(transactions, support, confidence, min_lift = 1, max_len = 3) {
  empty <- list(rules = NULL, n_generated = 0L, n_retained = 0L)
  if (is.null(transactions) || length(transactions) == 0) return(empty)
  if (!is.finite(support) || support <= 0 || support > 1) stop("support must be in (0, 1]", call. = FALSE)
  if (!is.finite(confidence) || confidence < 0 || confidence > 1) stop("confidence must be in [0, 1]", call. = FALSE)

  rules <- arules::apriori(
    transactions,
    parameter = list(supp = support, conf = confidence, minlen = 2, maxlen = max_len, target = "rules"),
    control = list(verbose = FALSE)
  )
  n_generated <- length(rules)
  if (n_generated == 0) return(empty)

  rules <- rules[!arules::is.redundant(rules)]
  rules <- rules[arules::quality(rules)$lift > min_lift]
  if (length(rules) == 0) return(modifyList(empty, list(n_generated = n_generated)))

  p_raw <- arules::interestMeasure(rules, "fishersExactTest", transactions = transactions,
                                   reuse = FALSE)
  out <- arules::DATAFRAME(rules, separate = TRUE, setStart = "", setEnd = "", itemSep = " + ") %>%
    dplyr::mutate(
      dplyr::across(c(LHS, RHS), as.character),
      # P(consequent) across eligible baskets; a high-confidence rule with a
      # very common consequent is not informative
      rhs_support = confidence / lift,
      p_adjusted = stats::p.adjust(p_raw, method = "holm")
    ) %>%
    dplyr::arrange(dplyr::desc(lift), dplyr::desc(support))
  list(rules = out, n_generated = n_generated, n_retained = nrow(out))
}

#' Support, confidence and lift for a single rule computed directly from the
#' baskets. Used to cross-check arules in the tests and for documentation.
rule_metrics <- function(baskets, lhs, rhs) {
  has_lhs <- vapply(baskets, function(b) all(lhs %in% b), logical(1))
  has_rhs <- vapply(baskets, function(b) all(rhs %in% b), logical(1))
  n <- length(baskets)
  support <- sum(has_lhs & has_rhs) / n
  confidence <- safe_divide(sum(has_lhs & has_rhs), sum(has_lhs))
  list(support = support, confidence = confidence, lift = safe_divide(confidence, sum(has_rhs) / n))
}
