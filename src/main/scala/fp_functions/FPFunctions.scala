package fp_functions

/**
  * This part is about implementing several functions that are very common in functional programming.
  * For the exercises in this part you are _not_ allowed to use library functions.
  * Do not use iteration, write recursive functions instead.
  * Hint: read about the functions in the lecture slides.
  *
  * This part is worth 30 points, 5 points per function.
  */
object FPFunctions {

    /** Q7 (5p)
      * Applies `f` to all elements and returns a new list.
      * @param xs list to map.
      * @param f mapping function.
      * @tparam A type of items in xs.
      * @tparam B result type of mapping function.
      * @return a list of all items in `xs` mapped with `f`.
      */
    def map[A, B](xs: List[A], f: A => B): List[B] = xs match{
        case Nil => Nil

        case _ => f(xs.head) :: map(xs.tail, f)
    }

    /** Q8 (5p)
      * Takes a function that returns a boolean and returns all elements that satisfy it.
      * @param xs the list to filter.
      * @param f the filter function.
      * @tparam A the type of the items in `xs`.
      * @return a list of all items in `xs` that satisfy `f`.
      */
    def filter[A](xs: List[A], f: A => Boolean): List[A] = xs match {
        case Nil => Nil

        case x if f(xs.head) => xs.head :: filter(xs.tail, f) // if f(A) is true => add A to list
        case x => filter(xs.tail, f) // if f(A) is true => add A to list
    }

    /** Q9 (5p)
      * Recursively flattens a list that may contain more lists into 1 list.
      * Example:
      *     List(List(1), List(2, 3), List(4)) -> List(1, 2, 3, 4)
      * @param xs the list to flatten.
      * @return one list containing all items in `xs`.
      */
    def recFlat(xs: List[Any]): List[Any] = xs match {
        case Nil => Nil

        case (h:List[Any])::tail => recFlat(h):::recFlat(tail)
        case (h:Any)::tail => h::recFlat(tail)
    }

    /** Q10 (5p)
      * Takes `f` of 2 arguments and an `init` value and combines the elements by applying `f` on the result of each previous application.
      * @param xs the list to fold.
      * @param f the fold function.
      * @param init the initial value.
      * @tparam A the type of the items in `xs`.
      * @tparam B the result type of the fold function.
      * @return the result of folding `xs` with `f`.
      */
    def foldL[A, B](xs: List[A], f: (B, A) => B, init: B): B = xs match {
        case Nil => init // when xs is empty return accumulator

        case x => foldL(xs.tail, f, f(init, x.head))
    }

    /** Q11 (5p)
      * Reuse `foldL` to define `foldR`.
      * If you do not reuse `foldL`, points will be subtracted.
      *
      * @param xs the list to fold.
      * @param f the fold function.
      * @param init the initial value.
      * @tparam A the type of the items in `xs`.
      * @tparam B the result type of the fold function.
      * @return the result of folding `xs` with `f`.
      */
    def foldR[A, B](xs: List[A], f: (A, B) => B, init: B): B = xs match{
        case Nil => init // when xs is empty return accumulator

        case x => foldR(x.init, f, f(x.last, init)) // TODO: reuse foldL
    }

    /** Q12 (5p)
      * Returns a iterable collection formed by iterating over the corresponding items of `xs` and `ys`.
      * @param xs the first list.
      * @param ys the second list.
      * @tparam A the type of the items in `xs`.
      * @tparam B the type of the items in `ys`.
      * @return a list of tuples of items in `xs` and `ys`.
      */
    def zip[A, B](xs: List[A], ys: List[B]): List[(A, B)] = xs match {
        case Nil => Nil

        case x => (xs.head, ys.head) :: zip(xs.tail, ys.tail)
    }
}
