# ad-hoc-poly

[![Travis](https://img.shields.io/travis/com/coord-e/ad-hoc-poly)](https://travis-ci.com/coord-e/ad-hoc-poly)

`ad-hoc-poly` is an implementation of ad-hoc polymorphism and type classes.

## Try

`ad-hoc-poly` works as a transpiler to OCaml. There are two source languages named `mlx1` and `mlx2`.

To see how this works, save the code below as `sample.mlx2`:

```ocaml
class<T> Num {
  add :: T -> T -> T,
  mul :: T -> T -> T,
  neg :: T -> T,
} in
impl Num<Int> {
  add = add_int,
  mul = mul_int,
  neg = neg_int,
} in
impl Num<Float> {
  add = add_float,
  mul = mul_float,
  neg = neg_float,
} in
let square = \x. mul x x in
let x = add (square 3) (int_of_float (square 2.7)) in
print_int x
```

And perform translation with:

```shell
stack run mlx2 -- sample.mlx2
```

You can find more examples under [test/data](test/data). Also, you can configure the type environment by editing [env.yaml](env.yaml). Enjoy!

## Two languages

### `mlx1`

`mlx1` is a language with overloadings. A type-directed translation pass resolves overloadings and compiles `mlx1` into well-typed OCaml code.

### `mlx2`

`mlx2` is a language with type classes. `mlx2` is desugared to `mlx1`.

## FAQ

### Q. I don't want to enter non-ascii symbols such as `∀` or `λ`

A. You can use `forall` for `∀` and `\` for `λ` and `Λ`.

### Q. Is this implementation proven to be correct?

A. No.

### Q. Why is mlx2 syntax designed to look like Rust's traits, but not Haskell's type classes?

A. To separate two compilation phases.

To perform the dictionary conversion, it is needed to identify each class declaration.

`class` and `instance` declarations take type expressions to represent constraints and instantiations in Haskell-like syntax. Thus, a type evaluation is required to identify the class declarations. I don't want this because the translation from `mlx1` to OCaml also requires type evaluation.

`class` and `impl` declarations in `mlx2` syntax take class names as a simple identifier and I can implement dictionary conversion easily because of it.

### Q. Is it necessary to introduce type-level lambda like this implementation to implement ad-hoc polymorphism?

A. No.

Haskell is one of the major conterexamples.

### Q. Is it feasible to implement this in one-pass?

A. Yes.

I've implemented this as two-pass compilation just for simplicity.

### Q. Does this implementation support higher-kinded types (HKT)?

A. No.

AFAIK, higher order unification is required to introduce HKT in a system with type lambdas. To keep simplicity, I didn't implemented that.
