(reverse alias list-reverse)
(list-reverse "expects one list and returns a new list which is the reverse of the input: (a b c) -> (c b a)" prim 1 "wile_list_reverse")
(length alias list-length)
(list-length "expects one list and returns its length" prim 1 "wile_list_length")
(list-length=? "expects one integer and one list and returns #t if the list length is equal to the given number, #f otherwise" prim 2 "wile_list_length_eq")
(list-length>=? "expects one integer and one list and returns #t if the list length is equal to or greater than the given number, #f otherwise" prim 2 "wile_list_length_ge")
(list-length>? "expects one integer and one list and returns #t if the list length is greater than the given number, #f otherwise" prim 2 "wile_list_length_gt")
(list-length<? "expects one integer and one list and returns #t if the list length is less than the given number, #f otherwise" prim 2 "wile_list_length_lt")
(list-length<=? "expects one integer and one list and returns #t if the list length is less than or equal to the given number, #f otherwise" prim 2 "wile_list_length_le")
(list-last "expects one list and returns its last element, or () for a null list" prim 1 "wile_list_last")
(append alias list-append)
(list-append "expects any number of lists and returns a new list which is the concatenation of the inputs" prim -1 "wile_list_append")
(flatten alias list-flatten)
(list-flatten "expects one list of lists of ... nested arbitrarily deeply, returns a new fully flattened list of all the atoms in the input list: (1 2 (3 4 (5 6) (7 (8 (9))))) -> (1 2 3 4 5 6 7 8 9)" prim 1 "wile_list_flatten")
(list-head "expects a list L and an integer N, and returns the first N elements of L as a new list" prim 2 "wile_list_head")
(list-tail "expects a list L and an integer N, and returns all but the first N elements of L" prim 2 "wile_list_tail")
(list-unhead "expects a list L and an integer N, and returns the last N elements of L" prim 2 "wile_list_unhead")
(list-untail "expects a list L and an integer N, and returns all but the last N elements of L as a new list" prim 2 "wile_list_untail")
(list-ref "expects a list L and an integer N, and returns the Nth element of L; the counting is zero-based, ie (list-ref L 0) is the same as (car L)" prim 2 "wile_list_ref")
(filter alias list-filter)
(list-filter "expects one test function of one argument and one list and returns those elements of the input list for which the test function does not return #f" prim 2 "wile_list_filter")
(partition alias list-partition)
(list-partition "expects one test function of one argument and one list and returns a list of two lists, the first one being all those elements of the input for which the test function returns #t and the second one being all those elements of the input for which the test functions returns #f" prim 2 "wile_list_partition")
(list->vector "expects one list of values and returns a vector of those values in the same order as in the input" prim 1 "wile_list2vector")
(list->bytevector "expects one list of characters or small integers and returns a bytevector containing all those bytes" prim 1 "wile_list2bytevector")
(string->char alias string->list)
(string->list "expects one string and returns a list of the individual characters of the string in order from the front" prim 1 "wile_string2list")
(vector->list "expects one vector and returns a list of the entries of the vector in order from the front" prim 1 "wile_vector2list")
(string-append "expects any number of strings and returns a new string which is the concatenation of the inputs" prim -1 "wile_string_append")
(foldr "expects a values-combining function, a right-end value, and a list, and returns the right fold of that function over that list" prim 3 "wile_foldr")
(foldl "expects a values-combining function, a left-end value, and a list, and returns the left fold of that function over that list" prim 3 "wile_foldl")
(expmod "expects three non-negative integer inputs A N M and returns (A^N) modulo M computed in an efficient manner that avoids extremely large numbers" prim 3 "wile_expmod")
(string-join-by "expects one or more strings, and returns the result of using the first string as a separator between the concatenation of all the rest" prim -2 "wile_string_join_by")
(string-split-by "expects a character-testing predicate indicating which characters to drop and a string, and returns a list of sub-strings whose characters did not get dropped" prim 2 "wile_string_split_by")
(string-split-by-whitespace "expects a string and returns a list of the non-whitespace sub-strings" prim 1 "wile_string_split_by_whitespace")
(foldl1 "expects a values-combining function and a non-empty list, and returns the left fold of that function over that list, using the first element of the list as the initial value" prim 2 "wile_foldl1")
(map "expects a procedure of N arguments and N lists all of the same length, where N is at least 1; applies the procedure to each tuple consisting of taking the jth entry from each of the lists, and returns the list of results" prim -3 "wile_map")
(for-each "expects a procedure of N arguments and N lists all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the lists; but does not build any list of results" prim -3 "wile_for_each")
(+ "expects any number of numeric values and returns their sum" prim -1 "wile_add")
(* "expects any number of numeric values and returns their product" prim -1 "wile_multiply")
(- "expects any number of numeric values; if just one, returns its negation; if more than one, subtracts all but the first from the first" prim -1 "wile_subtract")
(/ "expects any number of numeric values; if just one, returns its reciprocal; if more than one, divides the first by all the remaining values" prim -1 "wile_divide")
(min "expects zero or more numeric values, and returns the smallest of them" prim -1 "wile_min")
(max "expects zero or more numeric values, and returns the largest of them" prim -1 "wile_max")
(list-sort "expects one less-than comparison function and one list containing only values that can be compared with that comparison function, and returns a new list that is sorted according to that comparison function. sort algorithm is merge sort, a stable sort" prim 2 "wile_list_sort")
(replicate "expects one general value V and one integer N, and returns a list consisting of N repetitions of V" prim 2 "wile_replicate")
(memp "expects a test function and a list and returns the first sub-list of the input for which the test applied to the car returns #t, or #f if test returns #f for all elements" prim 2 "wile_memp")
(memv "expects a value and a list and returns the first sub-list of the input for which the value is eqv? to the car, or #f if no elements are eqv? to the value" prim 2 "wile_memv")
(assp "expects a predicate and a list of pairs, and returns the first pair for which the predicate applied to its CAR returns non-false, or #f if no such pair exists" prim 2 "wile_assp")
(assv "expects a test value and a list of pairs, and returns the first pair for which the test value is equivalent to its CAR, or #f if no such pair exists" prim 2 "wile_assv")
(list-drop-while "expects a predicate and a list and removes all those elements at the head of the list for which the predicate returns #t" prim 2 "wile_list_drop_while")
(list-take-while "expects a predicate and a list, and returns all those values at the head of the list for which the predicate returns non-false" prim 2 "wile_list_take_while")
(list-remove-dups "expects one list and returns a new list with all adjacent duplicates collapsed into one instance" prim 1 "wile_list_remove_dups")
(string-pad-left "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded with as many C on the left as are required to make the length of the result at least L" prim 3 "wile_string_pad_left")
(string-pad-right "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded with as many C on the right as are required to make the length of the result at least L" prim 3 "wile_string_pad_right")
(string-pad-center "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded symmetrically with as many C as are required to make the length of the result at least L" prim 3 "wile_string_pad_center")
(string-trim-left "expects a drop? predicate and a string, and returns a new string with all the droppable characters on the left side of the string removed" prim 2 "wile_string_trim_left")
(string-trim-right "expects a drop? predicate and a string, and returns a new string with all the droppable characters on the right side of the string removed" prim 2 "wile_string_trim_right")
(string-trim "expects a drop? predicate and a string, and returns a new string with all the droppable characters on both sides of the string removed" prim 2 "wile_string_trim")
(fromto "expects two integers and returns a list of numbers from the first to the second inclusive, counting up if the second integer is larger than the first, and counting down if the second integer is smaller than the first" prim 2 "wile_fromto")
(upfrom "expects two integers, a start and a count, and returns a list of numbers counting up from the start: 4 3 -> (4 5 6)" prim 2 "wile_upfrom")
(any-true? "expects a list of values and returns #t if any value is true" prim 1 "wile_any_true")
(all-true? "expects a list of values and returns #f if any value is false" prim 1 "wile_all_true")
(write-char alias write-string)
(write-string "expects any number of arguments, which must all be strings or characters, except that optionally the first may be an output port. if the first argument is not an output port, the default port is stdout. writes all string/char arguments to the output port" prim -1 "wile_write_string")
(sqlite-meta-tables "expects one sqlite port and returns a list of the names of all tables in the database" prim 1 "wile_sql_meta_tables")
(sqlite-meta-schema "expects one sqlite port and one string which is the name of a table, and returns the schema for that table in the form of an SQL CREATE statement" prim 2 "wile_sql_meta_schema")
(sqlite-dump-table "expects one sqlite port, one string which is the name of a table, and one output port, and dumps the named table into the output port in SQL format" prim 3 "wile_sql_dump_table")
(julian-day "expects three integers which represent year, month, and day respectively and returns the Julian day number corresponding to that date in the proleptic Gregorian calendar" prim 3 "wile_julian_day")
(gregorian-date "expects one integer which represents a Julian day number and returns a 3-list (Y M D) which is the corresponding proleptic Gregorian date" prim 1 "wile_gregorian_date")
(offset-date "expects four integers year month day offset and returns a triple (year' day' month') corresponding to that offset from (year month day)" prim 4 "wile_offset_date")
(delta-dates "expects six integers Y1 M1 D1 Y2 M2 D2 and returns the number of days between those dates in the Gregorian calendar" prim 6 "wile_delta_dates")
(day-of-week "expects either one Julian day number or a Gregorian date in the form of Y M D in numeric format, and returns the numeric weekday of that date, with Sunday being 0 to Saturday being 6" prim -2 "wile_day_of_week")
(is-leap-year? "expects one integer representing a year and returns whether that year is a leap year according to the Gregorian calendar" prim 1 "wile_is_leap_year")
(day-of-year "expects three integers, year month day, and returns the day of the year, from 1 to 365 (or 366)" prim 3 "wile_day_of_year")
(julian-day-of-easter "expects one integer and returns returns the Julian day number on which Easter falls in that year" prim 1 "wile_julian_day_of_easter")
(type-of "expects one argument and returns a symbol describing the type of that argument; may be 'unknown" prim 1 "wile_typeof")
(printf "expects a format string and any number of additional arguments, and writes the formatted output to stdout" prim -2 "wile_printf")
(fprintf "expects an output port, a format string and any number of additional arguments, and writes the formatted output to the specified output port" prim -3 "wile_fprintf")
(sprintf "expects a format string and any number of additional arguments and returns the formatted output as a string" prim -2 "wile_sprintf")
(random-permutation "expects one positive integer N as input and returns a list which is a random permutation of the numbers (1 .. N)" prim 1 "wile_random_permutation")
(vector-sort! "expects one less-than comparison function and one vector containing only values that can be compared with that comparison function, and sorts the vector in-place according to that comparison function. current sort algorithm is Shell sort" prim 2 "wile_vector_sort_inplace")
(vector-map "expects a function of N arguments and N equal-length vectors, and returns a vector of the same length whose entries are the values of the function applied to the corresponding entries of the input vectors" prim -3 "wile_vector_map")
(vector-map! "expects a function of one argument and a vector, and updates the vector in place by applying the function to each element and replacing it with that value" prim 2 "wile_vector_map_inplace")
(vector-for-each "expects a procedure of N arguments and N vectors all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the vectors; but does not build any vector of results" prim -3 "wile_vector_foreach")
(cholesky-decompose "expects an upper or lower half-matrix and does an L*D*L^T decomposition; returns a list containing the D matrix which is in turn a simple list of the diagonal values and the L matrix, stored in the same half-matrix form as the input" prim 1 "wile_cholesky_decompose")
(cholesky-solve "expects an L*D*L^T decomposed matrix as returned by cholesky-decompose and a list R, and solves the matrix equation L*D*L^T X = R for X" prim 2 "wile_cholesky_solve")
(wile-build-info "expects a flag and returns a list of various build configuration items" prim 1 "wile_build_info")
(display-stack-trace "expects a list of stack trace strings and one output file port argument and writes a nice useful stack trace to that file port. returns nothing useful" prim 2 "wile_display_stack_trace")
(stack-trace "expects one output file port argument and writes a nice useful stack trace to that file port. returns nothing useful" prim 1 "wile_stack_trace")
(root-bracket "expects a function of one real variable, a real-valued starting position, and a real-valued scale factor; returns a bracket on a root of the function" prim 3 "wile_root_bracket")
(root-bisect "expects an error-testing function, the function whose root is to be found, and two real values which bracket a root; returns a 2-list containing a root and the function value at that root" prim 4 "wile_root_bisect")
(root-ridders "expects an error-testing function, the function whose root is to be found, and two real values which bracket a root; returns a 2-list containing a root and the function value at that root" prim 4 "wile_root_ridders")
(curry "expects a function and an argument and returns a partially-applied new function" prim 2 "wile_curry")
(compose "expects any number of functions and returns a function which is the composition of all of them" prim -2 "wile_compose")
(list-group-by "expects a comparison function and a list and returns a list of lists of adjacent items which compare equal" prim 2 "wile_list_group_by")
(is-prime? "expects an integer N and an optional second integer K, and returns whether N is prime or not, based on doing K rounds of probabilistic Miller-Rabin testing" prim -2 "wile_mr_primality")
(next-prime "expects one integer N and returns the next prime that is greater than or equal to N, based on Miller-Rabin probabilistic primality test" prim 1 "wile_next_prime")
(wile-standard-environment "returns the wile standard environment without macros for use by the interpreter" prim 0 "wile_std_env_no_macros")
(wile-environment-with-macros "expects one environment list and adds several standard macros to it; uses the standard environment if the environment is given as #f" prim 1 "wile_env_add_macros")
(define-form? "this is part of the wile interpreter; do not use" prim 1 "wile_eval_define_form")
(load-file-path "this is part of the wile interpreter; do not use" prim 2 "wile_eval_load_path")
(load-form? "this is part of the wile interpreter; do not use" prim 1 "wile_eval_load_form")
(begin-form? "this is part of the wile interpreter; do not use" prim 1 "wile_eval_begin_form")
(eval-define "this is part of the wile interpreter; do not use" prim 3 "wile_eval_define")
(eval-begin "this is part of the wile interpreter; do not use" prim 3 "wile_eval_begin")
(apply-lambda "expects one procedure and a list of arguments, applies the procedure to the arguments, and returns the result" prim 2 "wile_eval_apply_lambda")
(apply-interp "expects one procedure and any number of arguments and applies the procedure to the arguments" prim -2 "wile_eval_apply_interp")
(eval "expects one environment list and and expression and returns the result of evaluating the expression in that environment" prim 2 "wile_eval")
