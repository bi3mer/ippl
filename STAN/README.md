# Project Details

Sample code can be found in [code_samples.rkt](./code_samples.rkt). More examples of constraints can be found in [test_stan_constraints.rkt](./test_stan_constraints.rkt). Additional, examples of constraints can be found in [test_stan_environment.rkt](./test_stan_environment.rkt).

Outside of those examples, every file with `stan_` has a corresponding `test_stan_`. It's been set up so everything is meant to be independent to make it easier to look through the code. If you want to run all tests at once you can use [test_all.rkt](./test_all.rkt). 

## simplifications

To index, Stan uses the common [] operator. This did not work in our case since a vector being indexed could be confused as a matrix by the pattern matcher. As a result <> was used for indexing instead. 

Stan uses <> for constraints. We instead use `((upper = 0) ... )`. We also use a `i ((none)) x` instead of `int x`. The inner `(none)` could likely be removed but this is a side-affect of past work when some declaration of none was required. If we had caught this before the day this was due, likely the no constraint could be changed to be `i () x`. But again, this syntactic improvement was noticed too late to be changed with confidence.

In Stan, an integer can be declared like so `int<lower=0>> a = 1;`. To simplify the language, we changed it to require two lines where first the variable is initialized `i ((lower = 0)) a`. Afterwards, it's value can be modified `a = 1`. You'll also notice that Stan uses semi-colons for it's lines and we do not:

```
(i ((lower = 0)) a)
(a = 1)
```

The semi-colons would not add anything in terms of readability and are removed from the model.

The array was removed since it added an additional layer of complexity and was not relevant to our concerns in terms of simplifications.

Every vector type constraint in Stan was implemented but no matrix type constraint was implemented. Instead a replacement, `onlyones`, was made to show that matrix types could be implemented. The reality, though, was that they were far more difficult to implement and did not get at what we wanted to look at for the project. 

Stan would object to have multiple lower declarations in constraints. We did not implement anything that would force there to only exists one constraint of the same type. It wasn't directly relevant to the problem. If we wanted to enforce this than we could have tested it with an accumulator which contained seen constraints. Then if a constraint had been seen, an error could have been thrown.

Stan implements multiplier and offset in the constraints such that they can be declared in any order. One can also be implemented while another was not. In our case, this added more unnecessary details to the model and was removed. Instead, a constraint of both types was made `(offset = number : multiplier = number)`. If one wanted to only do an offset with no multiplier than they could set the multiplier to 1. Similarly, if they wanted a multiplier without an offset than the offset could be set to 0.