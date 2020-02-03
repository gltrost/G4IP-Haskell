## G4IP-Haskell

### Introduction and Inspiration

This project is an implementation Roy Dyckhoff’s contraction-free sequent calculus g4ip, which allows for a proof-search method for the constructive propositional sequent calculus. This project was inspired by a Standard-ML homework assignment at Carnegie Mellon, but has been rewritten in Haskell here.  

## Important Files 

* G4IP-Haskell/src/G4IP.hs : Defines the g4ip-theorem prover 
* G4IP-Haskell/src/Expression.hs : Defines the data-structures used for g4ip
* G4ip/test/Spec.hs : Holds tests for the project



### Syntax

The syntax of the logic is:
* Booleans: `TRUE`/`FALSE`
* Proposition: `Atom "[some string]"`
* Implication: `-->` 
* Conjunction: `&`
* Disjunction: `\/`
* Negation: `neg`
* Bi-implication: `<->`

An example proposition would be `((Atom "a" --> TRUE) & (Atom "b" \/ FALSE))`. 

### Running G4IP

While in `G4IP-Haskell/src`, run `ghci G4IP.hs`, then type `g4ip [your-proposition]`.
For example: 

``` g4ip ((Atom "p" --> Atom "q") & (Atom "q" --> Atom "r") --> (Atom "p" --> Atom "r")) ```
will evaluate to `True`. 

### Testing 

Tests can be added in `test/Spec.hs`. This project was built with stack, so to run tests, simply type `stack test` at the root directory. 

### Next Steps

The next step is to build a visual interface for g4ip that will allow the user to see the proof completely written out if a proof exists. 
