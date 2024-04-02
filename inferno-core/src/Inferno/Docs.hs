{-# LANGUAGE QuasiQuotes #-}

module Inferno.Docs where

import Text.RawString.QQ

docs :: String
docs = [r|
# Inferno: Documentation

Inferno is a functional scripting language.

## Types

A core feature of the Inferno language is its type system. Types in a programming language are a way of labeling all values with the kind of “thing” they represent. For instance, within the Inferno language, there are values which are numeric, textual or represent time/timezones (amonst many others). Types can provide useful information to the user and help prevent many common errors when a script contains a mistake (like adding a boolean to an integer, or passing the wrong parameter to a function). These are some of the types present in Inferno:

- `int`: Integer numbers. These are numbers without decimals.
- `double`: Double-precision floating numbers. These are numbers with decimals.
- `bool`: Truth values. This type has only two members: `#true` and `#false`.
- `epochTime`: Points in time.
- `word16`: Unsigned 16-bit integeres.
- `word32`: Unsigned 32-bit integeres.
- `word64`: Unsigned 64-bit integeres.

Besides these “basic” types, there are several more complex types which can be built up from the types above:

- `array of *`: Where `*` can be `int`, `double` or potentially another array or other complex type
- `series of *`: This type represents the values of a control/virtual parameter. See the `TachDB` module below for functions that help extract values from a series.
- `option of *`: A type such as `option of int` has values that look like `Some 4` or `None` and represents a values which could potentially be undefined. This is useful to indicate a value is missing for a certain time in a time series, for example.
- `* -> *` (function type): A value can also be a function. For instance, a function that takes a `double` and returns an `int` has type `double -> int`.
- `(*, ..., *)` (tuple type): We can bundle several different types into a tuple, useful if we want to return multiple value from a function, for example a function with the type `double -> (int, text)` takes a `double` value and returns both an `int` and a `text` value. The unit/empty tuple `()` can be used to represent no (useful) value is being returned.
- Record types: A record is like a tuple where each field has a name. For instance `{ht = 1.51; wt = 62.4}` is a record value containing two fields `ht` and `wt` . We say such a record has type `{ht: double; wt: double}`.

There is no need to tell the system what type any of the literals and variables are. It can deduce this information automatically and will only warn the user in case of a mistake.

## Syntax

### Literals

Literals are values that you write directly in the script, like `0.8` or `120`.  
There are three kinds of literals:

- Numeric literal (with decimals): `0.80`, `100.0`, etc. They have type `double`.
- Numeric literal (without decimals): `120`, `1234`, etc. They can have type `int` or `double`, depending on context.
- Hexadecimal literal: `0xA9F`, `0x8D01`, etc. They have type `word64`.
- Text/string literal: `"Hello"` They have type `text`.
- Interpolated string: `` `Hello ${23} ${"world"}!` `` They have type `text`.

### Variables

Variables are names that represent values, like `foo` or `bar`. They always start with a letter and are followed by zero or more letters or numbers or underscores. Examples of valid variables are: `x`, `y`, `MyVariable`, `ThisIsAVariable`, `this_2_is_a_variable`, `Year2018`.

### Optionals

Certain functions in the standard library are not guaranteed to return a value. This is indicated by the optional type, which has two values:

- If a value is present, it is written using the `Some` keyword. For example the expression `Some 120` denotes an `option of int`, whereas `Some "Hello world!"` has type `option of text`.
- If a value is missing, we indicate this using the `None` keyword.

### Tuples

Values, variables, and other constructs can be grouped together into tuples, for example, if a function returns multiple values. Tuples are formed by appending values in brackets, e.g. `(0.80, "Hello world!")`, which has the type `(int, text)` . We can also indicate the return of a “unit” value by the unit/empty tuple `()`, not to be confused with the optional value `None`.

### Enums

An enum is a user defined set of names/labels which can be stored inside a control parameter instead of a simple `double` value. Using an enum may be useful if the parameter value represents a discrete state of a system rather than a continuous value, such as pressure or flow rate. For example, the `bool` type is an enum with the labels `#true` and `#false`. Another example of an enum might be a `plungerState` enum, with `#rising`, `#falling` or `#disabled` states. Using enums has multiple advantages described further below, not least of which is better readability of scripts.

### Functions

Like we mentioned earlier (see the Types section), a variable can contain a function. To apply an argument to a function, simply put the argument you want to pass tothe function right after the function variable. For example, if we have a function called `f` of type `double -> bool`, we can apply an argument `3.14` to `f`, simply by writing `f 3.14`. The result will have type `bool`.

You can create your own functions to re-use parts of your code. For example:
```
let square = fun x -> x * x in
(square 3.4, square 4.5)
```

### Let assignments

A let statement can be used to introduce temporary/intermediate values within a script:

```
let x = <code 1> in
<code 2>
```

Let statements can optionally be *annotated* with a type, if you want to tell Inferno what the type of an expression should be. For example, the literal `4` can be either an `int` or a `double`, so you can tell Inferno you mean `int` by:
```
let x : int = 4 in
<code that uses x as an int>
```
### Conditionals

Inferno includes the familiar `if...then...else...` construct. The syntax goes as  
follows:

```
if <boolean expression>
then
    <code 1>
else
    <code 2>
```

Unlike most programming languages, `if` statements in Inferno are expressions. This means we can assign an `if` expression to a variable or pass it to a function:

```
let x = if 2 > 3 then 5 else 6 in
x + 1
```

### Case statements

The `match` statement is like a more powerful `if` statement, which allows us to pattern match not only on `bool`eans but also other enums as well as optional types. The syntax for a `match` is as follows:

```
match <expression> with {
  | <pattern 1> -> 
    <code 1>
  ...
  | <pattern n> ->
    <code n>
}
```

Like the `if` statement, a `match` is also an expression, assignable to a variable, which means all the branches must return a value of the same type. A `match` statement can be used instead of an `if` statement:

```
let x =
  match 2 > 3 with {
    | #true -> 5
    | #false -> 6
  }
in
x + 1
```

It can also be used to inspect the value of an optional type:

```
match latestValue pid1 with {
  | Some val -> val * 2
  | None -> 0
}
```

In this instance, we analyze the result of calling `latestValue pid1`, which may rerturn a `None` value. The branches of a `match` must always be total, namely, we must handle all the cases for any given type, i.e. we must have a `#true` and a `#false` branch for `bool`, a `Some x` and `None` for `option of *`, etc. If we only care about one specific case and want to lump all the other cases into a “catch all” pattern, we can use the wildcard `_` pattern:

```
match someNumber with {
  | 0 -> 1
  | 1 -> 0
  | _ -> 42
}
```

It is also possible to match on multiple values at once with the use of tuples:

```
match (latest pid1, someNumber) with {
  | (Some v, 0) -> v
  | (Some v, 1) -> (-v)
  | (Some _, _) -> 42
  | (None, 0) -> 1
  | (None, 1) -> 0
  |  _         -> 84
}
```

### Assertions

Assertions in scripts can be used to check invariants/assumptions made about the data being processed, e.g.:

```
assert latestTempReading > 0 in
<code>
```

If the assertion is violated during the execution of the script an error is logged.

### Arrays

Arrays can be useful in scripts when pulling data from several virtual/control parameters and aggregating the results. Small arrays can be expressed as array literals:
```
[1.1, 34.5, 1231.53]
```

More complex array can be created using the array builder notation which allow for modifying and filtering arrays. The syntax of array builders is the following:

```
[ <expr> | x1 <- <array 1> , ... xn <- <array n>, (if <boolean condition>) ]
```

The array builder notation allows for filtering of elements

```
[ n | n <- someNumberArray, if n > 10 ]
```

applying a transformation to each element of an array

```
[ 2 * n | n <- someNumberArray ]
```

or combining several arrays by taking their product

```
[ (n, m) | n <- someNumberArray, m <- someOtherArray ]
```

See the `Array` module below for builtin functions that manipulate arrays.

### Records

A record is like a tuple, but it names its fields so that it is easier to distinguish between them or refer to them. For example:
```
let r1 = {ht = 1.51; wt = 62.4} in
let bmi = fun r -> r.wt / (r.ht * r.ht) in
bmi r1
```
In the example above, we create a record `r1` with two fields `ht` and `wt` to represent the height and weight of a person. The `bmi` function is given an arbitrary record `r` and extracts the two fields `ht` and `wt` using the record field access syntax e.g. `r.ht`.

The example above has the following types:

```
r1 : {ht: double; wt: double}
bmi : forall 'a. {ht: double; wt: double; 'a} -> double
```

Here, variable `r1` (which is assigned a record value) has type `{ht: double; wt: double}`, and the type of the function `bmi` uses a type variable `'a` to denote the fact that it accepts as argument any record that has at least the fields `ht` and `wt`.

This allows us to reuse functions operating on records, even when more fields are added. For example, the following code is also correct:
```
let r1 = {ht = 1.51; wt = 62.4} in
let bmi = fun r -> r.wt / (r.ht * r.ht) in
let x1 = bmi r1 in
let r12 = {ht = 1.51; wt = 62.4; name="Zaphod"} in
let x2 = bmi r2 in
...
```

### Predefined operators and precedence

| Precedence | Left          | None        | Right |
| ---------- | ------------- | ----------- | ----- |
| 11         |               |             | **    |
| 10         | *, /, %, .&.  |             |       |
| 9          | +, -, `.XOR.` |             |       |
| 8          | .\|.          |             |       |
| 7          |               | <,>, >=, <= |       |
| 6          |               | ==, !=      |       |
| 5          |               |             | &&    |
| 4          |               |             | XOR   |
| 3          |               |             |       |

Operator precedence and associativity.

## Type Classes

Some functions and operators operate on more than one type. For example, the division operator `/` can work on both `int`s and `double`s. Inferno uses the concept of *type classes* to represent such functions. Internally, `/` is defined as follows:
```
define division on int int int;
define division on int double double;
define division on double int double;
define division on double double double;

(/) : forall 'a 'b 'c. {requires division on 'a 'b 'c} => 'a -> 'b -> 'c := ...
```
Here, `division` is a type class with three type parameters `'a`, `'b`, and `'c`. The first few lines define the possible combinations of three types for which `division` is defined. The type of `/` then says that for any types `'a`, `'b`, and `'c` for which `division` is defined, `/` can be a function with type `'a -> 'b -> 'c`.

For example, if we divide an `int` by an `int`, this says that the result will also be an `int`. Similarly, if we divide a `double` by an `int`, the result will be a `double`.

An auto-generated list of all type classes and the list of types for which they are defined is at the bottom of this document.

## Modules

Modules are a way of organizing reusable functions/scripts into units which another script can depend on and import. The standard library provides several modules, which are detailed below. If a module `Foo` has a function `bar`, then you can use it in your script by writing `Foo.bar`.

If you find you are using functions from a module repeatedly in a script, you can also "open" the module and make its functions available to use without a prefix:
```
open Foo in
<your script here that can say just bar instead of Foo.bar>
```
Alternatively, you can rename a module using a `let module` statement:
```
let module F = Foo in
<your script here that can say F.bar instead of Foo.bar>
```

|]