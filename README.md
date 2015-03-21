# Little Schemer Resources

Study resources for [The Little Schemer](http://mitpress.mit.edu/books/little-schemer).

Contains:
* Scheme file containing function definitions developed during the book (tested under [CHICKEN Scheme](http://www.call-cc.org/))
* The Little Schemer Commandments (below)

## The Little Schemer Commandments

1. The first condition when recursing through a list should be `(null? lat)`
   When recursing with numbers, ask `(zero? n)` and 'else'

2. Use `cons` to build lists

3. When building a list, describe the first typical element and `cons` it onto the recursion

4. Always change at least one argument when recurring; it must be changed to be closer to termination.
   The changing argument must be tested in the termination condition;
   for example, when using `cdr`, test termination with `null?`
   and when using `sub1`, test termination with `zero?`
	
5. When building a value with `+`, always use `0` for the value of the terminating
   line, for adding `0` does not change the value of an addition.
   When building a value with `*`, always use `1` for the value of the terminating
   line, for multiplying by 1 does not change the value of a multiplicaiton.
   When building a value with `cons`, always consider `()` for the value of the
   terminating line


![The Little Schemer](http://lambda.jstolarek.com/wp-content/uploads/2013/01/The_Little_Schemer.jpg)

