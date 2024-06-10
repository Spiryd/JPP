% Merge two sorted lists
merge([], L, L).
merge(L, [], L).
merge([H1|T1], [H2|T2], [H1|T]) :-
    H1 =< H2,
    merge(T1, [H2|T2], T).
merge([H1|T1], [H2|T2], [H2|T]) :-
    H1 > H2,
    merge([H1|T1], T2, T).

% Split a list into two halves
split([], [], []).
split([X], [X], []).
split([H1,H2|T], [H1|T1], [H2|T2]) :-
    split(T, T1, T2).

% Mergesort algorithm
mergesort([], []).
mergesort([X], [X]).
mergesort(List, Sorted) :-
    List = [_,_|_], % Ensure the list has at least two elements
    split(List, L1, L2),
    mergesort(L1, Sorted1),
    mergesort(L2, Sorted2),
    merge(Sorted1, Sorted2, Sorted).

% gcd/3 calculates the greatest common divisor of A and B, and finds X and Y such that A*X + B*Y = G
gcd(A, 0, A, 1, 0).
gcd(A, B, G, X, Y) :-
    B \= 0,
    Q is A // B,
    R is A mod B,
    gcd(B, R, G, X1, Y1),
    X is Y1,
    Y is X1 - Q * Y1.

% de/5 finds X and Y such that A*X + B*Y = gcd(A, B)
de(A, B, X, Y, G) :-
    gcd(A, B, G, X, Y).

% Check if a number is prime
is_prime(2).
is_prime(3).
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, F) :-
    N mod F =:= 0.
has_factor(N, F) :-
    F * F < N,
    F2 is F + 2,
    has_factor(N, F2).

% Find the smallest prime factor
smallest_prime_factor(N, F) :-
    smallest_prime_factor(N, F, 2).

smallest_prime_factor(N, N, _) :- is_prime(N).
smallest_prime_factor(N, F, D) :-
    D * D =< N,
    (N mod D =:= 0 -> F = D ; D1 is D + 1, smallest_prime_factor(N, F, D1)).

% Find the prime factors
prime_factors(1, []).
prime_factors(N, [F|Factors]) :-
    N > 1,
    smallest_prime_factor(N, F),
    N1 is N // F,
    prime_factors(N1, Factors).

% gcd/2 calculates the greatest common divisor of two numbers
gcd(A, 0, A).
gcd(A, B, G) :-
    B \= 0,
    R is A mod B,
    gcd(B, R, G).

% Calculate Euler's Totient function
totient(1, 1).
totient(N, T) :-
    N > 1,
    totient(N, N, 0, T).

totient(_, 0, Acc, Acc).
totient(N, I, Acc, T) :-
    I > 0,
    gcd(N, I, 1),
    NewAcc is Acc + 1,
    NewI is I - 1,
    totient(N, NewI, NewAcc, T).
totient(N, I, Acc, T) :-
    I > 0,
    \+ gcd(N, I, 1),
    NewI is I - 1,
    totient(N, NewI, Acc, T).


% Sieve of Eratosthenes
sieve([], []).
sieve([P|Xs], [P|Ys]) :-
    exclude(multiple_of(P), Xs, Zs),
    sieve(Zs, Ys).

multiple_of(P, X) :-
    X mod P =:= 0.

% Generate a list of numbers from 2 to N
numlist(2, N, List) :- findall(X, between(2, N, X), List).

% Find all primes up to N
primes(N, Primes) :-
    N >= 2,
    numlist(2, N, List),
    sieve(List, Primes).

% Main test predicate
test_all :-
    % Test mergesort
    write('Testing mergesort...'), nl,
    mergesort([4, 2, 5, 3, 1], SortedList),
    write('mergesort([4, 2, 5, 3, 1], SortedList) = '), write(SortedList), nl,
    
    % Test diophantine equation solver
    write('Testing diophantine equation solver...'), nl,
    de(10, 15, X, Y, G),
    write('de(10, 15, X, Y, G) = '), write((X, Y, G)), nl,
    
    % Test prime factors
    write('Testing prime factors...'), nl,
    prime_factors(100, Factors),
    write('prime_factors(100, Factors) = '), write(Factors), nl,
    
    % Test Euler's totient function
    write('Testing Euler\'s totient function...'), nl,
    totient(100, TotientValue),
    write('totient(100, TotientValue) = '), write(TotientValue), nl,
    
    % Test primes up to N
    write('Testing primes up to N...'), nl,
    primes(100, Primes),
    write('primes(100, Primes) = '), write(Primes), nl.
