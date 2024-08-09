#' Custom Addition Function
#'
#' Adds two elements from the Galois Field or one element from the Galois Field and one element representing infinity.
#'
#' This function performs addition for elements of the Galois Field (GFElem) or between a Galois Field element and a point at infinity (PGInf).
#' If both elements are of type `GFElem`, their values are added modulo the field's characteristic. If either element is of type `PGInf`, the result is a point at infinity.
#'
#' @param x An object of class `GFElem` or `PGInf`.
#' @param y An object of class `GFElem` or `PGInf`.
#'
#' @return An object of class `GFElem` if both arguments are `GFElem`, or an object of class `PGInf` if either argument is `PGInf`.
#'
#' @examples
#' # Create two Galois Field elements
#' elem1 <- GFElem$new(3, 7)
#' elem2 <- GFElem$new(5, 7)
#'
#' # Add the two Galois Field elements
#' result <- Add(elem1, elem2)
#' result$show() # Should display the result in the format GFElem{7}(1)
#'
#' # Create a Galois Field element and a point at infinity
#' elem3 <- GFElem$new(2, 7)
#' inf <- PGInf$new(7)
#'
#' # Add the Galois Field element and the point at infinity
#' result2 <- Add(elem3, inf)
#' result2$show() # Should display PGInf{7}()
#'
#' @export
Add <- function(x, y) {
  if (inherits(x, "GFElem") && inherits(y, "GFElem")) {
    return(GFElem$new(x$value + y$value, x$p))
  } else if (inherits(x, "PGInf") || inherits(y, "PGInf")) {
    return(PGInf$new(x$p))
  } else {
    stop("Unsupported addition operation")
  }
}




#' Custom Multiplication Function
#'
#' This function multiplies two objects, which can either be elements of a Galois Field (GFElem)
#' or elements representing infinity (PGInf). If either argument is of type `PGInf`, the function
#' handles special cases where the multiplication involves a zero element in the Galois Field.
#'
#' @param x An object of class `GFElem` or `PGInf`. Represents the first operand in the multiplication.
#' @param y An object of class `GFElem` or `PGInf`. Represents the second operand in the multiplication.
#'
#' @return An object of class `GFElem` or `PGInf`, depending on the types of the input arguments.
#' If both inputs are `GFElem`, the result is a new `GFElem` with the product of their values.
#' If either input is `PGInf`, the result is `PGInf` with the same parameter as the inputs.
#'
#' @examples
#' # Multiply two Galois Field elements
#' elem1 <- GFElem$new(3, 7)
#' elem2 <- GFElem$new(5, 7)
#' result <- Multiply(elem1, elem2)
#' result$show()
#'
#' # Multiply a Galois Field element by infinity
#' elem <- GFElem$new(4, 7)
#' inf <- PGInf$new(7)
#' result_inf <- Multiply(elem, inf)
#' result_inf$show()
#'
#' # Multiply two infinity elements
#' inf1 <- PGInf$new(7)
#' inf2 <- PGInf$new(7)
#' result_inf <- Multiply(inf1, inf2)
#' result_inf$show()
#'
#' @export
Multiply <- function(x, y) {
  if (inherits(x, "GFElem") && inherits(y, "GFElem")) {
    return(GFElem$new(x$value * y$value, x$p))
  } else if (inherits(x, "PGInf") || inherits(y, "PGInf")) {
    if (inherits(x, "GFElem") && x$iszero()) {
      return(One(x))
    } else if (inherits(y, "GFElem") && y$iszero()) {
      return(One(y))
    } else {
      return(PGInf$new(x$p))
    }
  } else {
    stop("Unsupported multiplication operation")
  }
}




#' Custom Negation Function
#'
#' Negates a Galois Field element or a projective infinity element.
#' If the input is a Galois Field element, it returns a new Galois Field element with the negated value.
#' If the input is a projective infinity element, it returns a new projective infinity element with the same parameter.
#'
#' @param x An object of class \code{GFElem} or \code{PGInf}. For \code{GFElem}, it should be a Galois Field element. For \code{PGInf}, it should be a projective infinity element.
#'
#' @return For \code{GFElem}, returns a new \code{GFElem} object with the negated value. For \code{PGInf}, returns a new \code{PGInf} object with the same parameter.
#'
#' @examples
#' # Negate a Galois Field element
#' elem <- GFElem$new(3, 7)
#' result <- Negate(elem)
#' result$show()
#'
#' # Negate a projective infinity element
#' inf_elem <- PGInf$new(7)
#' result_inf <- Negate(inf_elem)
#' result_inf$show()
#'
#' @export
Negate <- function(x) {
  if (inherits(x, "GFElem")) {
    return(GFElem$new(-x$value, x$p))
  } else if (inherits(x, "PGInf")) {
    return(PGInf$new(x$p))
  } else {
    stop("Unsupported negation operation")
  }
}



#' Custom Inverse Function
#'
#' Computes the multiplicative inverse of a Galois Field element. If the input is zero, it returns a representation of infinity in the projective space. For elements in the projective space, it returns zero.
#'
#' @param x An object of class `GFElem` or `PGInf`. If `x` is an instance of `GFElem`, it should represent an element of a finite Galois Field. If `x` is an instance of `PGInf`, it represents the point at infinity in the projective space.
#'
#' @return An object of class `GFElem` if `x` is a Galois Field element and its inverse exists; otherwise, an object of class `PGInf` if `x` is zero. For projective space elements, returns zero.
#'
#' @details
#'  \itemize{
#'   \item{If `x` is a `GFElem` (Galois Field element) and is zero, the function returns a `PGInf` object, representing the point at infinity.}
#'   \item{If `x` is a non-zero `GFElem`, the function calculates its inverse using the Extended Euclidean Algorithm (implemented in `GCDEEA`). The inverse is returned as a `GFElem` object.}
#'   \item{If `x` is an instance of `PGInf` (point at infinity), the function returns a zero element in the Galois Field.}
#'   \item{If `x` is neither a `GFElem` nor a `PGInf`, the function stops with an error.}
#'   }
#'
#' @export
#' @examples
#' # Find the inverse of a Galois Field element
#' elem <- GFElem$new(3, 7)
#' result <- Inverse(elem)
#' result$show()
#'
#' # Find the inverse of zero (should return the point at infinity)
#' zero_elem <- GFElem$new(0, 7)
#' result <- Inverse(zero_elem)
#' result$show()
#'
#' # Find the inverse of the point at infinity (should return zero)
#' infinity_point <- PGInf$new(7)
#' result <- Inverse(infinity_point)
#' result$show()
Inverse <- function(x) {
  if (inherits(x, "GFElem")) {
    if (x$iszero()) {
      return(PGInf$new(x$p))
    }
    g <- GCDEEA(x$value, x$p)
    if (g$g != 1) {
      stop("Characteristic not prime in GF(", x$p, ").")
    }
    return(GFElem$new(g$s, x$p))
  } else if (inherits(x, "PGInf")) {
    return(Zero(x))
  } else {
    stop("Unsupported inversion operation")
  }
}


#' Zero Function
#'
#' Creates a zero element in the Galois Field or Projective Geometry.
#'
#' This function returns a zero element corresponding to the input element's field or space.
#' For `GFElem` objects, it returns a `GFElem` with value 0 and the same field order as the input.
#' For `PGInf` objects, it returns a `GFElem` with value 0 and the same field order as the input.
#'
#' @param x An object of class `GFElem` or `PGInf`. The field order (prime) of the input element will be used
#'           to create the zero element.
#'
#' @return An object of class `GFElem` with value 0 and the same field order as the input element.
#'
#' @examples
#' # Create the zero element of a Galois Field
#' elem <- GFElem$new(3, 7)
#' zero_elem <- Zero(elem)
#' zero_elem$show()
#'
#' # Create the zero element of Projective Geometry
#' pg_inf <- PGInf$new(7)
#' zero_pg <- Zero(pg_inf)
#' zero_pg$show()
#'
#' @export
Zero <- function(x) {
  if (inherits(x, "GFElem") || inherits(x, "PGInf")) {
    return(GFElem$new(0, x$p))
  } else {
    stop("Unsupported Zero operation")
  }
}


#' Generate the One Element of a Galois Field
#'
#' This function returns the element `1` in the context of a Galois Field or a projective geometry setting. The returned element is always a `GFElem` object with value `1` and the same characteristic as the input element.
#'
#' @param x An object of class `GFElem` or `PGInf`. The function uses the characteristic `p` of the input element to create a `GFElem` object with value `1`.
#'
#' @return An object of class `GFElem` representing the element `1` in the Galois Field defined by the characteristic `p` of the input element.
#'
#' @examples
#' # Create a GFElem object with value 3 and characteristic 7
#' elem <- GFElem$new(3, 7)
#'
#' # Generate the one element of the same Galois Field
#' one_elem <- One(elem)
#'
#' # Display the one element
#' one_elem$show()
#'
#' # Create a PGInf object with characteristic 7
#' pg_inf <- PGInf$new(7)
#'
#' # Generate the one element for PGInf (should be the same GFElem)
#' one_pg_inf <- One(pg_inf)
#' one_pg_inf$show()
#'
#' @export
One <- function(x) {
  if (inherits(x, "GFElem") || inherits(x, "PGInf")) {
    return(GFElem$new(1, x$p))
  } else {
    stop("Unsupported One operation")
  }
}
