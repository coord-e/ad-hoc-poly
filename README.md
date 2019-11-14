# ad-hoc-poly

Implementation of ad-hoc polymorphism.

## Try

`ad-hoc-poly` works as a transpiler from the project-specific language (named `mlx1` and `mlx2`) to OCaml.

Save the code below as `sample.mlx2`:

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
let square = λx. mul x x in
let x = add (square 3) (int_of_float (square 2.7)) in
print_int x
```

And perform translation with:

```shell
stack run mlx2 -- sample.mlx2
```

You'll see translated well-formed OCaml code which is printed to stdout.

## Two languages

### `mlx1`

`mlx1` is a language with overloadings. A type-directed translation pass resolves overloadings and compiles `mlx1` to well-typed OCaml code.

### `mlx2`

`mlx2` is a language with type classes. `mlx2` is desugared to `mlx1`.

## FAQ

### Q. I don't want to enter non-ascii symbols such as `∀` or `λ`

A. You can use `forall` for `∀` and `\` for `λ` and `Λ`.

### Q. Is this implementation proven to be correct?

A. No.

### Q. Why is mlx2 syntax designed to look like Rust's traits, but not Haskell's type classes?

A: To Separate two compilation phases.

...

### Q. Is it necessary to introduce type-level lambda like this implementation to implement ad-hoc polymorphism?

A. No.

...

### Q. Is it feasible to implement this in one-pass?

A. Yes.

...

### Q. Does this implementation support higher-kinded types (HKT)?

A. No.

...
