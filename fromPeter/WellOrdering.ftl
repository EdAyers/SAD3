# [prove off]
#[/prove]

[object/-s]
Signature. An object is a notion.

Let x, y, z, u, v stand for objects.
Let X, Y, Z stand for sets.

Let X is nonempty stand for X has an element.

# Signature. x is a jelement of X is an atom.

Let x \in X stand for x is an element of X.
Definition. A subset of Y is a set X such that every element of X is an element of Y.

[pair/-s]
Signature. A pair is a notion.
Let p, q stand for pairs.
Signature. [x,y] is a pair.
Signature. fst p is an object.
Signature. snd p is an object.

[relation/-s]
Signature. A relation is a notion.
Let r, s stand for relations. 
Signature. x -r> y is an atom.
# Definition. A relation is a set S such that all elements of S are pairs.
#Let x r y denote that [x, y] is an element of r.
Axiom. Let M be a set. Every element of M is an object.
Definition 82. r connects X iff for all u, v \in X
     u -r> v or v -r> u or u = v.

Definition 83. r is transitive in X iff for all u, v, w \in X
    if u -r> v and v -r> w then u -r> w.

Definition 84. r is asymmetric in X iff for all u, v \in X
    if u -r> v then not v -r> u.

Definition 86. a first member of X with r is an element z of X such that z \in X and for all y \in X it is wrong that y -r> z.

# Definition 86. z is a first member of X iff z \in X and for all y \in X
#     it is wrong that y -r> z.

[wellorder/-s]
Definition 87. r wellorders X iff r connects X and every nonempty subset of X has a first member with r.

# if Y \sub X and Y \neq \empty then there is an first member of Y with r)).

# Theorem 88. If r well-orders X then r is transitive in X and r is asymetric in X.
# Proof.
#     Let u \in X, v \in X, u -r> v and v -r> u.
#      then {u,v} \sub 
# qed.


