(ns sicp.chapters.chapter01
  (:require
    [sicp.utils :refer [square cube average]]))

(comment
	"Exercise 1.3
	Define a procedure that takes three numbers as arguments and returns the
	sum of the squares of the two larger numbers.")

(defn sum-of-squares-of-2> [x y z]
	(->> (sort > [x y z])
			 (take 2)
			 (map #(Math/pow % 2))
			 (apply +)))

#_(sum-of-squares-of-2> 1 2 3)


;;; ----------------------------------------------------------------------------
;;; 1.1.7 Example: Square Roots by Newton's Method

; The idea is to improve the answer until it is good enough so that
; its square differs from the radicand by less than a predetermined
; tolerance.
(defn good-enough? [guess x]
	(< (Math/abs ^float
			 (- (square guess)
					x))
		 0.001))

(defn sqrt-iter [guess x]
  ; (1) A guess is improved by averaging it with the quotient of the
  ; radicand and the old guess.
	(letfn [(improve [guess x] ; (1)
						(average guess
										 (/ x guess)))]
		(if (good-enough? guess x)
			guess
			(recur (improve guess x)
						 x))))

(defn sqrt
	"Returns the approximation of the square root of `x` using Newton's Method."
	[x]
	(sqrt-iter 1.0 x))

(comment
	(sqrt 9)
	(sqrt (+ 100 37))
	(square (sqrt 1000)))

(comment
	"Exercise 1.8
	Newton's method for cube roots is based on the fact that if y is an
	approximation to the cube root of x, then a better approximation is given
	by the value ... . Use this formula to implement a cube-root procedure
	analogous to the square root procedure.")

(defn cbrt-iter [guess x]
	(letfn [(improve []
						(/ (+ (/ x (square guess))
									(* 2 guess))
							 3))]
		(if (good-enough? guess x)
			guess
			(recur (improve)
						 x))))

(defn cbrt [x]
	(cbrt-iter 1.0 x))


;;; ----------------------------------------------------------------------------
;;; 1.2.1 Linear Recursion and Iteration

; Recursive:
; O(n) time and space
(defn factorial [n]
	(if (= n 1)
		1
		(* n (factorial (dec n)))))
#_(factorial 7)

; Iterative:
; O(n) time, O(1) space
(defn factorial [n]
	(letfn [(fact-iter [product counter max-count]
						(if (> counter max-count)
							product
							(recur (* counter product)
										 (inc counter)
										 max-count)))]
		(fact-iter 1 1 n)))

(comment
	"Exercise 1.10
	Ackermann's function.")

(defn A [x y]
	(cond (zero? y) 0
				(zero? x) (* 2 y)
				(= y 1) 2
				:else (A (dec x)
								 (A x (dec y)))))
(comment
	(A 1 10)
	(A 2 4)
	(A 3 3))

(comment
	"Give concise mathematical definitions for the functions computed by the
	prodedures f, g, and h for positive integers values of n. For example, (k n)
	computes 5n2.")

; 2 * n
(defn f [n] (A 0 n))

; 2 ** n
(defn g [n] (A 1 n))

; h(0) = 0
; h(1) = 2
; h(2) = 2 ** 2
; h(3) = 2 ** 2 ** 2
; h(4) = 2 ** 2 ** 2 ** 2
; h(n) = 2 ** 2 ** ... n times
(defn h [n] (A 2 n))

(defn k [n] (* 5 n n))

;;; ----------------------------------------------------------------------------
;;; 1.2.1 Linear Recursion and Iteration

; Recursive:
; O(golden-ratio^n) time and space
(defn fib [n]
	(cond (zero? n) 0
				(= n 1) 1
				:else (+ (fib (dec n))
								 (fib (- n 2)))))
#_(fib 6)

(comment
	"We can also formulate an iterative process for computing the Fibonacci
	numbers. The idea is to use a pair of integers a and b, initialized to
	Fib(1) = 1 and Fib(0) = 0, and to repeatedly apply the simultaneous
	transformations
	a <- a + b
	b <- a
	It is not hard to show that, after applying this transformation n times,
	a and b will be equal, respectively, to Fib(n+1) and Fib(n).")

(defn fib [n]
	(letfn [(fib-iter [a b count]
						(if (zero? count)
							b
							(recur (+ a b) a (dec count))))]
		(fib-iter 1 0 n)))


;;; ----------------------------------------------------------------------------
;;; Example: Counting change

(comment
	"The number of ways to change amount `a` using `n` kinds of coins equals
	- the number of ways to change amount a using all but the first kind of
	coin, plus
	- the number of ways to change amount `a - d` using all `n` kinds of coins,
	where `d` is the denomination of the first kind of coin.")

(defn first-denomination [kinds-of-coin]
	(case kinds-of-coin
		1 1
		2 5
		3 10
		4 25
		5 50))

(defn cc [amount kinds-of-coin]
	(cond (zero? amount)
				1

				(or (neg? amount)
						(zero? kinds-of-coin))
				0

				:else
				(+ (cc amount
							 (dec kinds-of-coin))
					 (cc (- amount
									(first-denomination kinds-of-coin))
							 kinds-of-coin))))

(defn count-change
	"How many ways can we make change for amount, given half-dollars, quarters,
	 dimes, nickels, and pennies?"
	[amount]
	(cc amount 5))
#_(count-change 100)

(comment
	"Exercise 1.11
	A function f is defined by the rule that f(n) = n if n < 3 and
	f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure that
	computes f by means of a recursive process. Write a procedure that computes
	f by means of an iterative process.")

; Recursive:
(defn f [n]
	(if (< n 3)
		n
		(+ (f (dec n))
			 (* 2 (f (- n 2)))
			 (* 3 (f (- n 3))))))
#_(map f (range 10))

; Iterative:
(defn f [n]
	(letfn [(f-iter [n1 n2 n3 counter]
						(if (< counter 3)
							n1
							(f-iter (+ n1 (* 2 n2) (* 3 n3))
											n1
											n2
											(dec counter))))]
		(if (< n 3)
			n
			(f-iter 2 1 0 n))))

(comment
	"Exercise 1.12
	The following pattern of numbers is called Pascal's triangle."
	[[1]
	 [1 1]
	 [1 2 1]
	 [1 3 3 1]
	 [1 4 6 4 1]]
	"The numbers at the edge of the triangle are all 1, and each number inside
	the triangle is the sum of the two numbers above it. Write a procedure that
	computes elements of Pascal's triangle by means of a recursive process.")

(defn pascal [row col]
	(if (or (= col 1) (= col row))
		1
		(+ (pascal (dec row) (dec col))
			 (pascal (dec row) col))))
#_(pascal 5 4)

(comment
	"Exercise 1.13
	Prove that Fib(n) is the closest integer to (golden-ratio^n)/sqrt(5), where
	golden-ratio = (1 + sqrt(5))/2. Hint: Let Phi = (1 - sqrt(5))/2. Use induction
	and the definition of the Fibonacci numbers to prove that
	Fib(n) = (Theta^n - Phi^n)/sqrt(5).")

(def golden-ratio
	(/ (inc (Math/sqrt 5))
		 2))

(def phi
	(/ (dec (Math/sqrt 5))
		 2))

(defn fib-proof [n]
	(/ (- (Math/pow golden-ratio n)
				(Math/pow phi n))
		 (Math/sqrt 5)))
#_ (fib 10)
#_ (fib-proof 10)

(defn fib-golden-ratio [n]
	(/ (Math/pow golden-ratio n)
		 (Math/sqrt 5)))
#_ (fib-golden-ratio 10)

(comment
	"Exercise 1.15")

(defn p [x]
	(- (* 3 x)
		 (* 4 (cube x))))

(defn sine [angle]
	(if-not (> (Math/abs ^float angle) 0.1)
		angle
		(p (sine (/ angle 3.0)))))
#_ (sine 12.15)


;;; ----------------------------------------------------------------------------
;;; 1.2.4 Exponentiation

; O(n) time and space
(defn expt [b n]
	(if (zero? n)
		1
		(* b (expt b (dec n)))))

; O(n) time, O(1) space
(defn expt [b n]
	(letfn [(iter [b counter product]
						(if (zero? counter)
							product
							(recur b (dec counter) (* product b))))]
		(iter b n 1)))
#_ (expt 2 8)

; b^n = sqr(b^(n/2)) if n is even
; b^n = b * b^n-1 if n is odd
; O(log n) time, O(log n) space
(defn fast-expt [b n]
	(cond (zero? n) 1
				(even? n) (square (fast-expt b (/ n 2)))
				:else (* b (fast-expt b (dec n)))))

(comment
	"Exercise 1.16
	Design a procedure that evolves an iterative exponentiation process that uses
	successive squaring and uses a logarithmic number of steps, as does fast-expt.
	(Hint: Using the observation that (b**(n/2))**2 = (b**2)**(n/2), keep, along
	with the exponent n and the base b, an additional state variable a, and
	define the state transformation in such a way that the product ab**n  is
	unchanged from state to state. At the beginning of the process a is taken to
	be 1, and the answer is given by the value of a at the end of the process.
	In general, the technique of defining an invariant quantity that remains
	unchanged from state to state is a powerful way to think about the design of
	iterative algorithms.")

(defn fast-expt [b n]
	(letfn [(helper [b n product]
						(cond (zero? n) product
									(even? n) (recur (square b) (/ n 2) product)
									:else (recur b (dec n) (* product b))))]
		(helper b n 1)))

#_(fast-expt 2 8)

(comment
	"Exercise 1.17
	The exponentiation algorithms in this section are based on performing
	exponentiation by means of repeated multiplication. In a similar way, one
	can perform integer multiplication by means of repeated addition. The
	following multiplication procedure (in which it is assumed that our language
	can only add, not multiply) is analogous to the expt procedure:"

	(defn multiply [a b]
		(if (zero? b)
			0
			(+ a (multiply a (dec b)))))

	"This algorithm takes a number of steps that is linear in b. Now suppose
	we include, together with addition, operations `double`, which doubles
	an integer, and `halve`, which divided an (even) integer by 2. Using
	these, design a multiplication procedure analogous to fast-expt that uses a
	logarithmic number of steps.")

(defn double [n] (* n 2))

(defn halve [n] (/ n 2))

; O(log n) time, O(1) space
(defn fast-multiply [a b]
	(letfn [(iter [a b acc]
						(cond (zero? b) acc
									(even? b) (recur (double a) (halve b) acc)
									:else (recur a (dec b) (+ acc a))))]
		(iter a b 0)))
#_(map (partial fast-multiply 10) (range 5))

(comment
	"Exercise 1.19
	There is a clever algorithm for computing the Fibonacci numbers in a 
	logarithmic number of steps. Recall the transformation of the state variables 
	a and b in the fib-iter process of section 1.2.2: a <- a + b and b <- a. 
	Call this transformation T, and observe that applying T over and over again 
	n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n).
	In other words, the Fibonacci numbers are produced by applying T^n, the nth 
	power of the transformation T, starting with the pair (1,0). Now consider T 
	to be the special case of p = 0 and q = 1 in a family of transformations 
	Tpq, where Tpq transforms the pair (a,b) according to a <- bq + aq + ap and 
	b <- bp + aq.

	Show that if we apply such a transformation Tpq twice, the effect is the same 
	as using a single transformation Tp'q' of the same form, and compute p' 
	and q' in terms of p and q. This gives us an explicit way to square these
	transformations, and thus we can compute Tn using successive squaring, as in 
	the fast-expt procedure. Put this all together to complete the following 
	procedure, which runs in a logarithmic number of steps)")

(defn fib-iter [a b p q count]
	(cond (zero? count) b
				(even? count) (recur a
														 b
														 (+ (square p) (square q))
														 (+ (square q) (* 2 p q))
														 (/ count 2))
				:else (recur (+ (* b q) (* a q) (* a p))
										 (+ (* b p) (* a q))
										 p
										 q
										 (dec count))))

(defn fib [n]
	(fib-iter 1 0 0 1 n))

(comment
	; Reversing a number using mod
	(loop [n 123456
				 reversed 0]
		(if (<= n 0)
			(recur (int (/ n 10))
						 (-> (* reversed 10)
								 (+ (mod n 10)))))))

;;; ----------------------------------------------------------------------------
;;; 1.2.5 Greatest Common Divisors 

; O(log n) time, O(1) space
(defn gcd 
	"Returns the Greatest Common Divisor of a and b using Euclid's Algorithm."
	[a b]
	(if (zero? b)
		a
		(recur b (rem a b))))

;;; ----------------------------------------------------------------------------
;;; 1.2.6 Example: Testing for Primality
 
(defn divides? [a b]
	(= (rem b a) 0))    

(defn find-divisor [n test-divisor]
	(cond (> (square test-divisor) n) n
				(divides? test-divisor n) test-divisor
				:else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
	(find-divisor n 2))

; Searching for divis 
; O(sqrt(n)) time and space
(defn prime? [n]
	(= n (smallest-divisor n)))


; The Fermat test
; O(log n) time
(defn expmod 
	"Computes the exponential of a number modulo another number."
	[base exp m]
	(cond (zero? exp) 1
				(even? exp) (rem (square 
													 (expmod base (/ exp 2) m))
												 m)
				:else (rem (* base 
											(expmod base (dec exp) m))
									 m)))

(defn fermat-test [n]
	(letfn [(try-it [a]
						(= (expmod a n n) a))]
		(try-it 
			(inc 
				(rand-int (dec n))))))

(defn fast-prime? 
	"Returns true if n is a prime number. Uses the fermat test."
	[n times]
	(cond (zero? times) true
				(fermat-test n) (fast-prime? n (dec times))
				:else false))
(comment
	(map #(list % (fast-prime? % 5) (prime? %))
			 (repeatedly 5 #(rand-int 9999999))))


(comment
	"Exercise 1.21
	Use the samllest-divisor procedure to find the samllest divisor of each of 
	the following numbers:"
	(map smallest-divisor [199 1999 19999])) 

(comment
	"Exercise 1.22")

(defn report-prime [elapsed-time]
	(print " *** ")
	(print elapsed-time))

(defn start-prime-test [n start-time]
	(when (prime? n)
		(report-prime start-time))) 

; todo: runtime
#_
(defn timed-prime-test [n]
	(newline)
	(print n)
	(start-prime-test n runtime))

(defn search-for-primes [range n]
	(let [now (.getTime (java.util.Date.))]
		(loop [primes []
					 range (filter odd? range)]
			(cond (or (= n (count primes))
								(empty? range)) 
						(do (println (- (.getTime (java.util.Date.))
														now))
								primes)
						
						:else
						(let [p (prime? (first range))]
							(recur (if p 
												(conj primes (first range))
												primes)
										 (rest range)))))))


#_(search-for-primes (iterate inc 1000) 3)
#_(search-for-primes (iterate inc 10000) 3)
#_(search-for-primes (iterate inc 100000) 3)
#_(search-for-primes (iterate inc 1000000) 3)


(comment
	"Exercise 1.23
	The smallest-divisor procedure shown at the start of this sectinon does lots
	of needless testing: After it checks to see if the number is divisible by 2
	there is no point in checking to see if it is divisible by any larger even
	numbers. This suggests that the values used for test-divisor should not be
	2, 3, 4, 5, 6, ... but rather 2, 3, 5, 7, 9, ... To implement this change, 
	define a procedure `next` that returns 3 if its innput is equal to 2 and
	otherwise returns its input plus 2. Modify the smallest-divisor procedure 
	to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test
	incorporating this modified version of smallest-divisor, run the test for each 
	of the 12 primes found in exercise 1.22. Since this modification havles the 
	number  of test steps, you should expect it to run about twice as fast. Is 
	this expectation confirmed? If not, what is the observed ratio of the speeds 
	of the two algorithms, and how do you explain the fact that it is different 
	from 2?")

(defn next-step [n]
  (if (= 2 n)
    3
    (+ 2 n)))


;;; ----------------------------------------------------------------------------
;;; 1.3.1 Procedures as Arguments

(defn sum-integers 
  "Returns the sum of the integers from a through b."
  [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))
#_(sum-integers 0 5)

(defn sum-cubes 
	"Returns the sum of the cubes of the integers in the given range."
	[a b]
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (inc a) b))))
#_(sum-cubes 1 5)

(defn pi-sum [a b]
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2)))
			 (pi-sum (+ a 4) b))))
(pi-sum 1 10)

(defn sum
	"Implements the summation of a series (sigma notation)."
	[term a next b]
	(if (> a b)
		0
		(+ (term a)
			 (sum term (next a) next b))))

(defn sum-cubes [a b]
	(sum cube a inc b))
#_(sum-cubes 1 10)

(defn sum-integers [a b]
	(sum identity a inc b))
#_(sum-integers 1 10)

(defn sum-pi [a b]
	(sum #(/ 1.0 (* % (+ % 2)))
			 a
			 #(+ % 4)
			 b))
#_(* 8 (sum-pi 1 1000))

(defn integral [f a b dx]
	(letfn [(add-dx [x] (+ x dx))]
		(* (sum f 
						(+ a (/ dx 2.0)) 
						add-dx 
						b)
			 dx)))
#_(integral cube 0 1 0.01)
#_(integral cube 0 1 0.0001)

; Exercise 1.29.  
;
; Simpson's Rule is a more accurate method of numerical integration than the method
; illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as
;
; S|a,b of f =  h/3[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn]
;
; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). 
; (Increasing n increases the accuracy of the approximation.) 
; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, 
; computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 
; (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

(defn integral-2 [f a b n]
	(let [h (/ (- b a) n)
				next (fn [x] (+ x (* 2 h)))]
		(* (/ h 3)
			 (+ (f a)
					(* 4 (sum f (+ a h) next (- b h)))
					(* 2 (sum f (+ a (* h 2)) next (- b (* 2 h))))
					(f b)))))
#_(integral-2 cube 0 1 2)

(comment
	"Exercise 1.30
	The sum procedure above generates a linear recursion. The procedure can be 
	rewritten so that the sum is performed iteratively. Show how to do this by 
	filling in the missing expressions in the following definition:")

(defn sum [term a next b]
	(letfn [(iter [a result]
						(if (> a b)
							result
							(iter (next a) (+ (term a) result))))]
		(iter a 0)))

(comment
  "Exercise 1.31
	a. The sum procedure is only the simplest of a vast number of similar 
	abstractions that can be captured as higher-order procedures. Write an 
	analogous procedure called `product` that returns the product of the values 
	of a function at points over a given range. Show how to define `factorial` 
	in terms of `product`. Also use `product` to compute approximations to π 
	using the formula
	
	π/4 = 2*4*4*6*6*8*.../3*3*5*5*7*7*...
	
	b. If your product procedure generates a recursive process, write one that 
	generates an iterative process. If it generates an iterative process, write 
	one that generates a recursive process.")

(defn product 
  "Returns the product of the values of a function at points over a given 
  range."
  [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(defn factorial [n]
  (product identity 1 inc n))
#_(factorial 7)

(defn pi [n]
  (letfn [(next [x] (+ x 2))]
    (* 4.0
       (/ (* 2
             (product square 4 next (- n 2))
             n)
          (product square 3 next n)))))
#_(pi 20)

(defn product 
  "Returns the product of the values of a function at points over a given 
  range."
  [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) 
                     (* result (term a)))))]
    (iter a 1)))

(comment
  "Exercise 1.32
	a. Show that `sum` and `product` (exercise 1.31) are both special cases of 
	a still more general notion called `accumulate` that combines a collection 
	of terms, using some general accumulation function:"
  
  (accumulate combiner null-value term a next b)
  
  "`Accumulate` takes as arguments the same term and range specifications as 
	`sum` and `product`, together with a `combiner` procedure (of two arguments) 
	that specifies how the current term is to be combined with the accumulation 
	of the preceding terms and a `null-value` that specifies what base value to 
	use when the terms run out. Write `accumulate` and show how `sum` and 
	`product` can both be defined as simples calls to `accumulate`.
	
	b. If your `accumulate` procedure generates a recursive process, write one 
	that generates an iterative process. If it generates an iterative process, 
	write one that generates a recursive process.")

(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate term (next a) next b))))

(defn accumulate [combiner null-value term a next b]
  (letfn [(iter [a result]
             (if (> a b)
               result
               (recur (next a)
                      (combiner result (term a)))))]
    (iter a null-value)))

(defn sum [term a next b]
  (accumulate + 0 term a next b))
#_(sum identity 1 inc 10)

(defn product [term a next b]
  (accumulate * 1 term a next b))
#_(product identity 1 inc 7)

(comment
  "Exercise 1.33
	You can obtain an even more general version of `accumulate` (exercise 1.32) 
	by introducing the notion of a `filter` on the terms to be combined. That 
	is, combine only those terms derived from values in the range that satisfy 
	a specified condition. The resulting `filtered-accumulate` abstraction takes 
	the same arguments as accumulate, together with an additional predicate of 
	one argument that specifies the filter. Write `filtered-accumulate` as a 
	procedure. Show how to express the following using `filtered-accumulate`:
	
	a. the sum of the squares of the prime numbers in the interval a to b 
	(assuming that you have a `prime?` predicate already written)
	
	b. the product of all the positive integers less than n that are relatively 
	prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).")


(defn filtered-accumulate [combiner pred null-value term a next b]
  (if (> a b)
    null-value
    (combiner 
      (if (pred a) 
        (term a) 
        null-value)
      (filtered-accumulate 
        combiner pred null-value term (next a) next b))))

(defn sum-of-squares-of-primes [a b]
  (filtered-accumulate + prime? 0 square a inc b))
#_(sum-of-squares-of-primes 1 10)

(defn prod-relative-primes [n]
  (letfn [(relative-prime? [x]
            (= 1 (gcd x n)))]
    (filtered-accumulate * relative-prime? 1 identity 1 inc n)))
#_(prod-relative-primes 10)


;;; ----------------------------------------------------------------------------
;;; 1.3.3 Procedures as General Methods

;;; Finding roots of equations by the half-interval method

(defn close-enough? [x y]
  (< (Math/abs ^float
       (- x y))
     0.001))

(defn search
  [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond
          (pos? test-value) (search f neg-point midpoint)
          (neg? test-value) (search f midpoint pos-point)
          :else midpoint)))))

(defn half-interval-method 
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (neg? a-value) (pos? b-value))
      (search f a b)
      
      (and (neg? b-value) (pos? a-value))
      (search f b a)
      
      :else (throw (Exception. 
                     (str "Values are not of opposite sign: " a " " b))))))
; Approximate π between 2 and 4 of sin x = 0.
#_(half-interval-method #(Math/sin %) 2.0 4.0)
; Search the root of the equation x3 - 2*x - 3 = 0, between 1 and 2.
#_(half-interval-method #(- (* % % %) (* 2 %) 3)
                        1.0
                        2.0)


;;; Finding fixed points of functions

(def tolerance 0.00001)

(defn fixed-point
  "Takes a function and an initial guess and produces an approximation to a 
  fixed point of the function. f is applyed repeatedly until we find two 
  successive values whose difference is less than some prescribe tolerance."
  [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2))
               tolerance))
          (try- [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try- first-guess)))

; Approximate the fixed point of the cosine function, starting with 1.
#_(fixed-point #(Math/cos %) 1.0)
; Find a soluction to the equation y = sin y + cos y.
#_(fixed-point #(+ (Math/sin %) (Math/cos %)) 1.0)

; Wrong!
(defn sqrt [x]
  (fixed-point (fn [y] (/ x y))
               1.0))

(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y)))
               1.0))

#_(sqrt 49)

(comment
  "Exercise 1.35
	Show that the golden ratio (section 1.2.2) is a fixed point of the 
	transformation x -> 1 + 1/x, and use this fact to compute the golden ratio 
	by means of the fixed-point procedure.")

(defn golden-ratio []
  (fixed-point (fn [x] (+ 1 (/ 1 x)))
               1.0))
; TODO page 70

(comment
  "Exercise 1.36
	Modify `fixed-point` so that it prints the sequence of approximations it 
	generates, using the newline and display primitives shown in exercise 1.22. 
	Then find a solution so pow(x x) = 1000 by finding a fixed pont of 
	x -> log(1000)/log(x). (Use Scheme's primitive log procedure, which computes 
	natural logarithmics.) Compare the number of steps this takes with and 
	without average damping. (Note that you cannot start fixed-point with a 
	guess of 1, as this would cause division by log(1) = 0.)")

(def log? (atom false))

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2))
               tolerance))
          (try- [guess]
            (let [next (f guess)]
              (when @log? (prn next))
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try- first-guess)))

(defn problem []
  (fixed-point
    (fn [x] (/ (Math/log 1000)
               (Math/log x)))
    2.0))

(comment
  "Exercise 1.37
  a. An infinite continued fraction is an expression of the form
  
  f = n1 / d1 + n2/ d2 + n3/d3 + ...
  
  As an example, one can show that the infinite continued fraction expansion 
  with the Ni, and the Di all equal to 1 produces 1/phi, where phi is the 
  golden ratio (described in section 1.2.2). One way to approximate an 
  infinite continued fraction is to truncate the expansion after a given 
  number of terms. Such a truncation - a so-called k-term finite continued 
  fraction - has the form
  
  n1/ d1 + n2/ ... + ng/ dg
  
  Suppose that n and d are procedures of one argument (the term index i) that 
  return the Ni and Di of the terms of the continued fraction. Define a 
  procedure `cont-frac` such that evaluationg (conf-frac n d k) computes the 
  value of the k-term finite continued fraction. Check your procedure by 
  approximating 1/phi using"
  
  (cont-frac (fn [i] 1.0)
             (fn [i] 1.0)
             k)
  
  "for successive values of k. How large must you make k in order to get an 
  approximation that is accurate to 4 decimal places?
  
  b. If your cont-frac procedure generates a recursive process, write one that 
  generates an iterative process. If it generates an iterative process, write 
  one that generates a recursive process.")

(defn cont-frac [n d k]
  (letfn [(helper [i]
            (if (>= i k)
              (/ (n i) (d i))
              (/ (n i)
                 (+ (d i) 
                    (helper (inc i))))))]
    (helper 1)))

(defn cont-frac [n d k]
  (letfn [(iter [acc i]
            (if (neg? i)
              acc
              (recur (/ (n i)
                        (+ (d i) acc))
                     (dec i))))]
    (iter (/ (n k) (d k))
          (dec k))))

#_(cont-frac (fn [i] 1.0)
             (fn [i] 1.0)
             12)

; TODO page 71

(comment
  "Exercise 1.38
	In 1737, the Swiss mathematician Leonhard Euler published a memoir De 
	Frantionibus Continuis, which included a continued fraction expansion for 
	e - 2, where e is the base of the natural logarithms. In this fraction, 
	The Ni are all 1, and Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,.... 
	write a program that uses your cont-frac procedure from exercise 1.37 to 
	approximate e, based on Euler's expension.")

(defn frantionibus-continuis-di
  []
  (letfn [(iter [i]
            (lazy-seq
              (cons 
                i
                (concat
                  '(1 1)
                  (iter (+ 2 i))))))]
    (cons 1
          (iter 2))))
              
(defn e-2 [n]
  (let [Di (take (inc n) (frantionibus-continuis-di))]
    (cont-frac
      (fn [i] 1.0)
      (fn [i] (nth Di i))
      n)))

#_(e-2 100)

(comment
  "Exercise 1.39
	A continued fraction representation of the tangent function was published in 
	1770 by the German mathematician J.H. Lambert: 
	
  tan x = x/ 1 - x2/ 3 - x2/ 5 - ...
  
	where x is in radians. Define a procedure (tan-cf x k) that computes an 
	approximation to the tangent function based on Lambert's formula. K specifies 
	the number of terms to compute, as in exercise 1.37. ")

(defn tan-cf [x k]
  (letfn [(rec [n i]
            (cond 
              (= 1 i) (/ x (rec n (inc i)))
              (= k i) (/ (square x) n)
              :else (- n 
                       (/ (square x) 
                          (rec (+ 2 n) (inc i))))))]
    (rec 1 1)))

#_(tan-cf 2.0 10)        


;;; ----------------------------------------------------------------------------
;;; 1.3.4 Procedures as Returned Values

(defn average-damp 
  "Given a function f, we consider the function whose value at x is equal to 
  the average of x and f(x)."
  [f]
  (fn [x]
    (average x (f x))))

#_((average-damp square) 10) ; => 55

; Using average-damp, we can reformulate the square-root procedure as follows:
(defn sqrt [x]
  (fixed-point
    (average-damp
      (fn [y] (/ x y)))
    1.0))

(defn cube-root [x]
  (fixed-point
    (average-damp
      (fn [y] (/ x (square y))))
    1.0))

#_(cube-root 8)


; Dg(x) = g(x + dx) - g(x)
;         ---------------
; 							 dx

(def dx 0.00001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) 
          (g x))
       dx)))

#_((deriv cube) 5)

; With the aid of deriv, we can express Newton's method as a fixed point 
; process:
(defn newton-transform [g]
  (fn [x]
    (- x 
       (/ (g x)
          ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point
    (newton-transform g)
    guess))

(defn sqrt [x]
  (newtons-method
    (fn [y] (- (square y) x))
    1.0))

#_(sqrt 100)

(defn fixed-point-of-transform 
  "Takes a fn g that computes some fn, a fn transform that transforms g, and
  an initial guess."
  [g transform guess]
  (fixed-point
    (transform g)
    guess))

(defn sqrt [x]
  (fixed-point-of-transform
    (fn [y] (/ x y))
    average-damp
    1.0))

(defn sqrt [x]
  (fixed-point-of-transform
    (fn [y] (- (square y) x))
    newton-transform
    1.0))

(comment
  "Exercise 1.40
  Define a procedure `cubic` that can be used together with the newtons-method 
  procedure in expressions of the form"
  
  (newtons-method (cubic a b c) 1)
  
  "to approximate zeros of the cubic x3 + ax2 + bx + c.")

(defn cubic [a b c]
  (fn [x]
    (+ (cube x) (* a (square x)) (* b x) c)))

#_(newtons-method
    (cubic 1 2 3)
    1.0)

(comment
  "Exercise 1.41
  Define a procedure `double` that takes a procedure of one argument as argument 
  and returns a procedure that applies the original procedure twice. For 
  example, if inc is a procedure that adds 1 to its argument, then (double inc) 
  should be a procedure that adds 2. What value is returned by
  (((double (double double)) inc) 5)")

(defn double [f]
  (fn [x]
    (f (f x))))

#_((double inc) 1)
#_(((double (double double)) inc) 5) 
    

(comment
  "Exercise 1.42
  Let f and g be two one-argument functions. The composition f after g is 
  defined to be the function x -> f(g(x)). Define a procedure compose that 
  implements composition. For example, if inc is a procedure that adds 1 to 
  its argument,
  ((compose square inc) 6)
   => 49")

(defn compose [& fns]
  (let [fns (reverse fns)]
    (fn [& args]
      (reduce (fn [acc f]
                (f acc))
              (apply (first fns) args)
              (rest fns)))))

(defn compose [& fns]
  (letfn [(iter [fns acc]
            (if (seq fns)
              (recur (rest fns) ((first fns) acc))
              acc))]
    (fn [& args]
      (apply (partial iter (reverse fns)) args))))

((compose square inc) 6)
      

(comment
  "Exercise 1.43
  If f is a numerical function and n is a positive integer, then we can form 
  the nth repeated application of f, which is defined to be the function whose 
  value at x is f(f(...(fx)...)). For example, if f is the function x -> x + 1, 
  then the nth repeated application of f is the function x -> x + n. If if is 
  the operation of squaring a number, then the nth repeated application of f is 
  the function that raises its argument to the 2nth power. Write a procedure 
  that takes as inputs a procedure that computes f and a positive integer n 
  and returns the procedure that computes the nth repeated application of f. 
  Your procedure should be able to be used as follow:"
  
  ((repeated square 2) 5)
  => 625
  
  "Hint: you may find it convenient to use compose from exercise 1.42")

(defn repeated [f n]
  (letfn [(iter [acc n]
            (if (<= n 0)
              acc
              (recur (f acc) (dec n))))]
    (fn [x]
      (iter x n))))

(comment
  "Exercise 1.44
  The idea of smoothing a function is an important concept in signal 
  processing. If f is a function and dx is some small number, then the smoothed 
  version of f is the function whose value at a point x is the average of 
  f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input 
  a procedure that computes the smoothed f. It is sometimes valuable to 
  repeatedly smooth a function (that is, smooth the smoothed function, 
  and so on) to obtain the n-fold smoothed function. Show how to generate 
  the n-fold smoothed function of any given function using smooth and repeated 
  from exercise 1.43.")

(defn smooth 
  ([f]
   (smooth f 1))
  ([f n]
   (repeated
     (fn [x]
       (average 
         (f (- x dx))
         (f x)
         (f (+ x dx))))
     n)))
  
(comment 
  "Exercise 1.45
  We saw in section 1.3.3 that attempting to compute square roots by naively 
  finding a fixed point of y -> x/y does not converge, and that this can be 
  fixed by average damping. The same method works for finding cube roots as 
  fixed points of the average-damped y -> x/y^2. Unfortunately, the process 
  does not work for fourth roots - a single average damp is not enough to 
  make a fixed-point search for y -> x/y^3 converge. On the other hand, if 
  we average damp twice (i.e., use the average damp of the average damp of 
  y -> x/y^3) the fixed-point search does converge. Do some experiments to 
  determine how many average damps are required to compute nth roots as f 
  fixed-point search based upon repeated average damping of y -> x/y^(n-1). 
  Use this to implement a single procedure for computing nth roots using 
  fixed-point, average-damp, and the repeated procedure of exercise 1.43. 
  Assume that any arithmetic operations you need are available as primitives.")  


(defn damps [n]
  (Math/ceil
    (- (/ (Math/log (inc n))
          (Math/log 2))
       1)))

(defn nth-root [x n]
  (fixed-point
    ((repeated average-damp (damps n))
     (fn [y]
       (/ x (Math/pow y (dec n)))))
    1.0))

#_(nth-root 4294967296 32)
    

(comment
  "Exercise 1.46
  Several of the numerical methods described in this chapter are instances of 
  an extremely general computational strategy known as iterative improvement. 
  Iterative improvement says that, to compute something, we start with an 
  initial guess for the answer, test if the guess is good enough, and otherwise 
  improve the guess and continue the process using the improved guess as the 
  new guess. Write a procedure iterative-improve that takes two procedures as 
  arguments: a method for telling whether a guess is good enough and a method 
  for improving a guess. Iterative-improve should return as its value a 
  procedure that takes a guess as argument and keeps improving the guess 
  until it is good enough. Rewrite the sqrt procedure of section 1.1.7. and 
  the fixed-point procedure of section 1.3.3 in terms of iterative-improve.")

(defn iterative-improve [good-enough? improve]
  (letfn [(iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    iter))

(defn sqrt [x]
  ((iterative-improve
     good-enough?
     (fn [guess]
       (average guess (/ x guess))))
   x))

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2))
               0.00001))
          (good-enough? [guess]
            (close-enough? guess (f guess)))])
  ((iterative-improve
     good-enough?
     f)
   first-guess))