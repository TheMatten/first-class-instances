# first-class-instances

(WIP!)

Remember when you had some neat way of building instance out of another one,
when some type had multiple interesting instances of same class, or when it was
possible to elegantly derive some instance for your type on paper, but compiler
just wouldn't allow you to use these things because of overlapping and stuff?
Well, now you can!:

TODO: examples

`first-class-instances` allow you to treat typeclass instances as what they
actually are - ordinary values. And Magic Inside™ turns them back and forth
between ordinary constraints when needed. Build instances from the others,
locally from available functions or even dynamically at runtime! Or derive them
tusing ordinary Haskell code and then promote into global ones using TH (in
development).

This library is still experimental, so please do not use it for serious things,
but I welcome you to try it out and give some feedback!