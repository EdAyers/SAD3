# [prove off]
#[/prove]

#Definition. An object is a notion.

Let x, y, z stand for objects.
Let X, Y, Z stand for sets.
Let x \in X stand for x is an element of X.

[pair/-s]
Signature. A pair is a notion.
Let p, q, r stand for pairs.
Signature. [x,y] is a pair.
Signature. fst p is an object.
Signature. snd p is an object.

[relation/-s]
Signature. A relation is a notion.
Let r, s stand for relations. 
Signature. x ~ r ~ y is an atom.
# Definition. A relation is a set S such that all elements of S are pairs.
#Let x r y denote that [x, y] is an element of r.

Definition 82. r connects X iff for all u, v \in X
     u ~ r ~ v or v ~ r ~ u or u = v.

Definition 83. r is transitive in X iff for all u, v, w \in X
    if u ~ r ~ v and v ~ r ~ w then u ~ r ~ w.

Definition 84. r is asymmetric in X iff for all u, v \in X
    if u ~ r ~ v then not v ~ r ~ u.

Definition 86. z is r first member of X iff z \in X and for all y \in X
    it is wrong that y ~ r ~ z.

Definition 87. r well-orders X iff r connects X and if Y \sub X and Y \neq \empty then there is an r first member of Y 

Theorem 88. If r well-orders X then r is transitive in X and r is asymetric in X.
Proof.
    Let u \in X, v \in X, u ~ r ~ v and v ~ r ~ u.
     then {u,v} \sub 
qed.


