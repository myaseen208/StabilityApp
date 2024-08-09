#' Greatest Common Divisor and Extended Euclidean Algorithm
#'
#' Computes the greatest common divisor (GCD) of two integers using the Extended Euclidean Algorithm.
#' This algorithm not only computes the GCD but also finds coefficients \eqn{s} and \eqn{t} such that:
#' \deqn{g = a \cdot s + b \cdot t}
#' where \eqn{g} is the GCD of \eqn{a}  and \eqn{b}.
#'
#' @param a An integer. The first number for which the GCD is to be computed.
#' @param b An integer. The second number for which the GCD is to be computed.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\eqn{g}}{The greatest common divisor of \eqn{a} and \eqn{b}.}
#'   \item{\eqn{s}}{The coefficient of \eqn{a} in the linear combination that equals \eqn{g}.}
#'   \item{\eqn{t}}{The coefficient of \eqn{b} in the linear combination that equals \eqn{g}.}
#' }
#'
#' @export
#' @import furrr
#' @import fastverse
#' @import Matrix
#' @import primes
#' @import R6
#'
#' @examples
#' # Compute the greatest common divisor and the coefficients for 30 and 12
#' result <- GCDEEA(30, 12)
#' print(result)
#'
#' # Expected output: A list with the GCD, and coefficients s and t
#' # Example output: $g
#' # [1] 6
#' # $s
#' # [1] 1
#' # $t
#' # [1] -2
GCDEEA <- function(a, b) {
  s <- 0
  old_s <- 1
  r <- b
  old_r <- a
  while (r != 0) {
    quotient <- old_r %/% r
    temp <- r
    r <- old_r - quotient * r
    old_r <- temp
    temp <- s
    s <- old_s - quotient * s
    old_s <- temp
  }
  return(list(g = old_r, s = old_s, t = (old_r - old_s * a) %/% b))
}


#' Generate Factors for \eqn{q = 2}
#'
#' This function generates factors for a specific value of \eqn{q}, where \eqn{q = 2}. It calculates factors based on the Galois Field and infinity elements.
#'
#' @param m An integer representing the value used to generate the factors. It must be a multiple of 2.
#'
#' @return A list of factors in the form of Galois Field elements or infinity elements. Each factor is a GFElem object or PGInf object, representing elements in the Galois Field or infinity respectively.
#'
#' @examples
#' # Generate factors for q = 2 with m = 6
#' factors <- Gen2Factors(6)
#' factors
#'
#' @export
Gen2Factors <- function(m) {
  if (m %% 2 != 0) {
    stop("m must be a multiple of 2, got m=", m, ".")
  }
  r <- m - 1
  starter <- c(
    list(c(GFElem$new(0, r), PGInf$new(r))),
    lapply(1:((r-1) %/% 2), function(i) c(GFElem$new(i, r), GFElem$new(-i, r)))
  )
  factors_list <- lapply(0:(r-1), function(g) {
    lapply(starter, function(Ai) {
      lapply(Ai, function(x) Add(x, GFElem$new(g, r)))
    })
  })
  return(unlist(factors_list, recursive = FALSE))
}


#' Generate Factors for \eqn{q = 3}
#'
#' This function generates factors for a given parameter \eqn{m} where \eqn{q = 3}.
#' The factors are generated using elements of the Galois Field GF(m-1) and
#' include the point at infinity. The function performs several checks to ensure
#' the validity of the input and uses parallel processing to compute the factors.
#'
#' @param m An integer representing the parameter for the factors. It must be a
#'          multiple of 6, and \eqn{m-1} must be a prime number.
#'
#' @return A list of factors in the Galois Field GF(m-1), including the point at infinity.
#'         The result is unlisted and contains the generated factors.
#'
#' @details
#' The function checks if \eqn{m} is a multiple of 6 and if \eqn{m-1} is a prime number.
#' It then calculates the factors using elements of the Galois Field GF(m-1) and
#' processes them using parallel computation with `future_map` to speed up the operation.
#' The factors include all elements of the field and are calculated using the
#' iterated function approach. The result is unlisted to return a simple list of factors.
#'
#' @importFrom furrr future_map furrr_options
#' @importFrom primes is_prime
#' @export
#' @examples
#' # Generate factors for q = 3
#' factors <- Gen3Factors(12)
#' factors
Gen3Factors <- function(m) {
  if (m %% 6 != 0) {
    stop("m must be a multiple of 6, got m=", m, ".")
  }
  if (!is_prime(m - 1)) {
    stop("m-1 must be prime, got m=", m, ".")
  }

  r <- m - 1
  F <- lapply(0:(r-1), function(i) GFElem$new(i, r))
  PG <- c(F, list(PGInf$new(r)))

  O <- future_map(PG, function(i) {
    orbit <- Iterated(function(x) Negate(Inverse(Multiply(x, Add(One(x), x)))), i)
    return(unique(take(orbit, r + 1)))
  }, .options = furrr_options(seed = TRUE))

  omega <- F[[which(sapply(F, function(i) Order(i)) == (r - 1))]]

  factors_list <- future_map(F, function(g) {
    lapply(omega^(1:((r-1) %/% 2)), function(lambda) {
      lapply(O, function(Ai) {
        lapply(Ai, function(x) Add(Multiply(lambda, x), g))
      })
    })
  }, .options = furrr_options(seed = TRUE))

  return(unlist(factors_list, recursive = FALSE))
}



#' Calculate the Order of a Galois Field Element
#'
#' This function calculates the order of a Galois Field (GF) element. The order of an element in a finite field is the smallest positive integer \eqn{n} such that the element raised to the power \eqn{n} is equal to 1. For elements in the field GF(p), where \eqn{p} is a prime number, the order is a divisor of \eqn{p-1}.
#'
#' @param x An object of class `GFElem` representing the Galois Field element whose order is to be computed.
#'
#' @return An integer representing the order of the Galois Field element.
#'
#' @examples
#' # Compute the order of a Galois Field element
#' elem <- GFElem$new(3, 7)
#' order <- Order(elem)
#' print(order)
#'
#' # Example with a different element
#' elem2 <- GFElem$new(2, 11)
#' order2 <- Order(elem2)
#' print(order2)
#'
#' # Order of zero element
#' zero_elem <- GFElem$new(0, 5)
#' order_zero <- Order(zero_elem)
#' print(order_zero)
#'
#' @export
Order <- function(x) {
  p <- x$p
  return(length(unique(sapply(1:p, function(i) x^i))))
}


#' Iterated Function Application
#'
#' Applies a function iteratively to an initial element until the result repeats.
#' This function generates a sequence of values starting from an initial value,
#' where each subsequent value is obtained by applying a given function to the previous value.
#' The process stops when a repeated value is encountered.
#'
#' @param f A function to apply iteratively. It should take a single argument and return a result of the same type as the input.
#' @param x An initial value to start the iteration. It should be of the same type as the input required by the function `f`.
#'
#' @return A list containing the sequence of values obtained by applying the function `f` iteratively to `x`.
#' The sequence ends when a value repeats, and the repeated value is included in the result.
#'
#' @export
#'
#' @examples
#' # Define a function to be iterated
#' multiply_by_two <- function(x) x * 2
#'
#' # Apply the function iteratively
#' result <- Iterated(multiply_by_two, 1)
#' print(result)
#'
#' # Define a GFElem element and apply a function to it
#' elem <- GFElem$new(3, 7)
#' iterated <- Iterated(function(x) Multiply(x, elem), elem)
#' iterated
Iterated <- function(f, x) {
  result <- list()
  current <- x
  while (!current %in% result) {
    result <- append(result, list(current))
    current <- f(current)
  }
  return(result)
}


#' Generate a HyperDesign Matrix
#'
#' The `HyperDesign` function generates a design matrix for combinatorial designs.
#' Each column in the matrix represents a combination of \eqn{q} elements chosen
#' from a set of \eqn{m} elements. The resulting matrix is sparse.
#'
#' @param n An integer specifying the number of columns (combinations) in the matrix.
#' @param m An integer specifying the total number of elements to choose from.
#' @param q An integer specifying the number of elements to choose for each combination.
#'
#' @details
#' The function generates all possible combinations of \eqn{q} elements from \eqn{m} elements.
#' It then selects \eqn{n} combinations and creates a matrix where each column represents
#' one of these combinations. The matrix is sparse, with 1s indicating the presence of
#' an element in the combination and 0s otherwise.
#'
#' The function assumes that \eqn{m} is a multiple of 2 and \eqn{q} is a positive integer.
#' If the number of required combinations \eqn{n} exceeds the total number of possible combinations,
#' an error is thrown.
#'
#' @return A sparse matrix of class `CsparseMatrix` from the `Matrix` package, where
#' each column represents a combination of \eqn{q} elements from the set of \eqn{m} elements.
#'
#' @seealso
#' The `Matrix` package for sparse matrix operations.
#'
#' @importFrom Matrix Matrix
#' @export
#'
#' @examples
#' # Generate a HyperDesign matrix with 10 columns, 6 elements, and combinations of 2
#' design1 <- HyperDesign(n = 10, m = 6, q = 2)
#' design1
#'
#' # Generate a HyperDesign matrix with 12 columns, 6 elements, and combinations of 2
#' design2 <- HyperDesign(n = 12, m = 6, q = 2)
#' design2
#'
#' # Generate a HyperDesign matrix with 10 columns, 6 elements, and combinations of 3
#' design3 <- HyperDesign(n = 10, m = 6, q = 3)
#' design3
HyperDesign <- function(n, m, q) {
  if (m %% 2 != 0) stop("m must be a multiple of 2")
  if (q <= 0) stop("q must be a positive integer")

  combs <- combn(m, q, simplify = FALSE)
  num_combs <- length(combs)

  if (n > num_combs) stop("Not enough combinations to fill the matrix")

  design_matrix <- matrix(0, nrow = m, ncol = n)

  for (i in 1:n) {
    comb <- combs[[i]]
    design_matrix[comb, i] <- 1
  }

  sparse_design_matrix <- Matrix(design_matrix, sparse = TRUE)

  return(sparse_design_matrix)
}
