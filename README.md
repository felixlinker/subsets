# subsets

This package provides two modules that allow to efficiently iterate over all subsets of any list.
Here is a quick code example:

```
>>> data T = A | B | C deriving (Show)
>>> subsetsAsc [A, B, C] 0 3
[[], [A], [B], [C], [A, B], [A, C], [B, C], [A, B, C]]
>>> subsetsDesc [A, B, C] 3 3
[[A,B,C],[B,C],[A,C],[A,B],[C],[B],[A],[]]
```

We use the idea of the Banker's sequence (described in [this paper](https://www.researchgate.net/publication/2526315_Efficiently_Enumerating_the_Subsets_of_a_Set)) to achieve this.
This approach has the upsides:
* No class constraints
* Subsets are generated without recursion thus being very memory efficient; you will need a constant amount of memory throughout your iteration
* Subsets are generated in sorted order concerning their subset-order which in some scenarios can save a lot of time
