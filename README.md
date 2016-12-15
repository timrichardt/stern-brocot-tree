# Lazy arithmetics on the Stern-Brocot tree
![In this figure, the first three levels of the Stern-Brocot tree are drawn.](https://raw.githubusercontent.com/timrichardt/stern-brocot-tree/master/resources/first_three_levels_of_SSB.png)

The figure above shows the first three levels of the signed Stern-Brocot
tree **SSB**. Each path in in the right binary tree represents a
positive rational number while each in the left a negative rational
number. Irrational numbers can be represented as infinite lazy sequences on
the trees [3].

The **SSB** is related to representation of numbers as continued
fractions and in 1978 Bill Gosper presented an algorithm for arithmetics
on continued fractions [1]. In 2007, Milad Niqui published a work that
generalizes Bill Gospers algorithm onto sequences of **SSB** [3]. The
Clojure implementation here, with some exceptions, closely resembles
Milad Niqui's work.

## Usage
Assuming, you have installed `boot`, run

```
boot pom jar install
```

inside the repository. `stern-brocot-tree` should then be installed into
your local Maven and can be included into project dependencies.

```
[stern-brocot-tree "0.0.1"]
```

Inside a project, reuquire the following functions.

```
> (require '(stern-brocot [tree :refer [Q->SSB SSB->Q R L fmt]]))
> (require '(stern-brocot [arithmetic :refer [add sub mul div]]))
```

`Q->SSB` converts a rational number to it's **SSB** representation and
`SSB->Q` is it's inverse. `fmt` prints a human readble representation of
the sequence.

```
> (Q->SSB -2/3)
(-1 #function[stern-brocot.tree/L] #function[stern-brocot.tree/R])

> (fmt *1)
-LR

> (SSB->Q [-1 L R])
-2/3
```

Irrational numbers are infinite sequences of **SSB**. For example, the
square root of two can be represented through `(1 ~R ~@(cycle [L L R
R]))`. That means we start in the positive tree and descend right, and
then continue infinitely descending left, left, right, right. The
further we we climb down the tree, the more precise our value of the
root.

```
> (->> `(1 ~R ~@(cycle [L L R R]))
       (take 42)
       fmt)
RLLRRLLRRLLRRLLRRLLRRLLRRLLRRLLRRLLRRLLRR

> (->> `(1 ~R ~@(cycle [L L R R]))
       (take 42)
       SSB->Q)
77227930/54608393

> (double *1)
1.414213562373095

> (Math/sqrt 2)
1.4142135623730951
```

The arithmetic operations are `add`, `sub`, `mul` and `div` and behave
like the ordinary arithmetic operations. The example shows how to add
one to the square root of two.

```
> (->> (add (Q->SSB 1) `(1 ~R ~@(cycle [L L R R])))
       (take 42)
       SSB->Q
       double)
2.414213562373095
```

## References
1. R.W. Gosper (1978), Continued fraction arithmetic, Unpublished draft
   paper, text available at  http://www.tweedledum.com/rwg/cfup.htm (cited
   Dec 15 2016)
2. A. Ya. Khinchin, Continued Fractions, Dover Publications, Mineoa, NY (1961)
3. M. Niqui (2007). Exact arithmetic on the Stern–Brocot tree. Journal
   of Discrete Algorithms, 5(2),
   356-379.
   [doi:10.1016/j.jda.2005.03.007](http://dx.doi.org/10.1016/j.jda.2005.03.007)
4. M. Niqui, Y. Bertot, QArith - A rational arithmetic library for Coq,
   https://github.com/coq-contribs/qarith-stern-brocot (cited Dec 15
   2016)
5. R.E. Graham, D.E. Knuth, O. Patashnik, Concrete Mathematics. A
   Foundation for Computer Science 2nd edition, Addison-Wesley, Reading,
   MA (1994)
