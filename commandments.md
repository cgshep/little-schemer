# The Little Schemer Commandments

1. The first condition when recursing through a list should be `(null? lat)`;
   when recursing with numbers, ask `(zero? n)` and 'else'

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