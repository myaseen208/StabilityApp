#' Galois Field Element Class
#'
#' @description
#' The `GFElem` class represents an element in a Galois Field (GF). A Galois Field, also known as a finite field, is a field with a finite number of elements. This class provides methods for initializing a field element, checking if it is zero, and displaying its value.
#'
#' @details
#' The `GFElem` class has two main fields:
#' \itemize{
#'   \item `value`: An integer that represents the value of the Galois field element.
#'   \item `p`: A prime number that represents the characteristic of the Galois field.
#' }
#'
#' The class provides methods to:
#' \itemize{
#'   \item Initialize a Galois Field element with a specified value and characteristic.
#'   \item Check if the element is zero.
#'   \item Display the element in a readable format.
#' }
#'
#' @export
#' @examples
#' # Create a Galois Field element
#' elem <- GFElem$new(3, 7)
#'
#' # Display the Galois Field element
#' elem$show()
#'
#' # Check if the element is zero
#' elem$iszero()
#'
#' # Create a zero element and check
#' zero_elem <- GFElem$new(0, 7)
#' zero_elem$iszero()
#'
#' # Create an element and check if it is zero
#' non_zero_elem <- GFElem$new(5, 7)
#' non_zero_elem$iszero()
#'
#' # Display an element
#' non_zero_elem$show()
GFElem <- R6::R6Class("GFElem",
  public = list(
    #' @field value An integer representing the value of the Galois field element.
    value = NULL,

    #' @field p A prime number representing the characteristic of the Galois field.
    p = NULL,

    #' @description
    #' Initialize a new GFElem object.
    #'
    #' @param value An integer. The value of the Galois field element.
    #' @param p A prime number. The characteristic of the Galois field.
    #'
    #' @return A new `GFElem` object.
    initialize = function(value, p) {
      self$value <- value %% p
      self$p <- p
    },

    #' @description
    #' Check if the element is zero.
    #'
    #' @return TRUE if the element is zero, otherwise FALSE.
    iszero = function() {
      return(self$value == 0)
    },

    #' @description
    #' Display the element.
    show = function() {
      cat("GFElem{", self$p, "}(", self$value, ")\n", sep = "")
    }
  )
)




#' PGInf Class
#'
#' @description
#' A class to represent the infinity element in projective geometry over a Galois field.
#' This class is used to handle the concept of "infinity" in computations involving projective spaces.
#'
#' @export
#'
#' @examples
#' # Create a PGInf element
#' inf_elem <- PGInf$new(7)
#' inf_elem$show()
#' inf_elem$iszero()
#'
#' @details
#' The `PGInf` class provides a representation of the infinity element, often used in projective geometry
#' where an element represents an infinite point or direction. In this implementation, the infinity
#' element is associated with a prime number that represents the characteristic of the underlying Galois field.
#'
#' @section Public Fields:
#' \describe{
#'   \item{\code{p}}{A prime number representing the characteristic of the Galois field.}
#' }
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(p)}}{
#'     Initializes a new \code{PGInf} object.
#'     \describe{
#'       \item{\code{p}}{A prime number representing the characteristic of the Galois field.}
#'     }
#'     @return A new \code{PGInf} object is created.
#'   }
#'   \item{\code{iszero()}}{
#'     Checks if the element is zero.
#'     This method always returns \code{FALSE} because the infinity element is never zero.
#'     @return \code{FALSE}.
#'   }
#'   \item{\code{show()}}{
#'     Displays the infinity element in a readable format.
#'     @return Nothing is returned. This method prints the infinity element to the console.
#'   }
#' }
PGInf <- R6::R6Class("PGInf",
  public = list(
    #' @field p A prime number representing the characteristic of the Galois field.
    p = NULL,

    #' @description
    #' Initialize a new PGInf object.
    #'
    #' @param p A prime number. The characteristic of the Galois field.
    #'
    #' @return A new `PGInf` object.
    initialize = function(p) {
      self$p <- p
    },

    #' @description
    #' Check if the element is zero (always returns FALSE).
    #'
    #' @return FALSE as the infinity element is never zero.
    iszero = function() {
      return(FALSE)
    },

    #' @description
    #' Display the infinity element.
    show = function() {
      cat("PGInf{", self$p, "}()\n", sep = "")
    }
  )
)
