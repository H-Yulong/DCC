## Defunctionalization with dependent types

We provide a portable standalone implementation of the defunctionalization translation of §3, 
written in OCaml and compiled to run in a web browser using `js_of_ocaml`. 
The implementation performs type checking of CC (§3.1) and DCC (§3.2) terms, 
abstract defunctionalization (§3.3) and backwards translation from DCC to CC (§3.5), 
allowing the interested reader to experiment with the effects of the translation on real
examples. We include several ready-made examples, including dependent composition, 
dependent pairs and finite sets.

## Getting started guide

Open `index.html` in a web browser. Then, you can use the following steps
to familiarize yourself with the implementation's functionalities.

1. Click on the *Identity application* example to load it into the source-language part (left column)of the webpage.
This example is about the polymorphic identity function `λA:U0.λx:A.x` in CC,
applied to the `Unit` type then to the unit term `()`.
2. Click "Check context" to check if the source-language context 
(in this case, the empty context) is well-formed. 
The output textbox should turn green and show "Context OK".
3. Click "Infer type" to automatically infer the type of the source-language term `(\A:U0.\x:A.x) Unit ()`. 
The output textbox should show `Unit`.
4. Click "Type check" to see if the inferred type is correct. 
The output textbox should turn green, meaning that it type-checks.
5. Click "Normalize" to normalize the term and the result should be `()`. Then, click "Back" to resume.
6. Click "Transform to DCC" to perform the defunctionalization transformation.
The target-language part (right column) should show the resulting label context,
type context, term, and type in target language.
7. Use "Type check" and "Normalize" to verify that the transformation preserves
types and reduction behaviors. 
The transformed term should type check and reduce to `()`, since the transformation is
type-preserving (Theorem 3.16) and correct (Corollary 3.13).
8. Use "Transform to CC" to perform the backward transformation
(You may want to delete the contents in the source-language term and type textbox first).
In this case, the result term should be `((λA:U0. λx:A. x) Unit) ()`
and the result type should be `Unit`.

Apart from the examples, you can also write your own terms in the source and the target language, 
then normalize/type-check/transform them, and witness type-preservation and correctness of the transformation.
For instance, start with the fully-dependent composition function in Section 3.1.
```
\A:U0. \B:(A -> U0). \C:(Pi x:A. (B x) -> U0).
\f:(Pi x:A. Pi y:B x. C x y).
\g:(Pi x:A. B x).
\x:A. f x (g x)
```
If anything fails, the output box should turn red and show an error message.
You can find a documentation on the syntax of both languages at the bottom of the webpage or in the next section.

## Step-by-Step Instructions

### Syntax

*Universes* &emsp;
Universes are `U0`, `U1`, `U2`, etc.

*Unit type* &emsp;
We have a built-in unit type `Unit` and the unit value `()`.

*Variables* &emsp; 
We use named variables.
Names may contain lower- and upper-case letters, 
numbers, underscores, and quotation marks, and must begin with a letter.
A variable binds to the cloest binder that gives the same name.
For example, the variable `x` in `\x:U0. \x:U1. x` binds to the inner lambda, 
so, `x` has type `U1` and the inferred type of this expression is `Pi x:U0. Pi x:U1. U1`.
The same convention also applies to label names.

*Lambda and Pi* &emsp;
Lambda abstractions and Pi types are written as `\x:A.M` and `Pi x:A.B`,
where `x` is the bound variable and `A`,`M`,`B` are term expressions.
Note that a space is needed after `Pi` for the implementation to parse it correctly.

*Applications* &emsp;
Applications are `M N` in CC and `M @ N` in DCC, where `M` and `N` are CC/DCC terms.
Parenthesis are needed in cases like `g x (f x)`.

*Type contexts* &emsp;
Type contexts have the form of `x1:A1, x2:A2, ... , xn:An`,
where `x1, ... , xn` are variables and `A1, ... , An` are expressions.
Note that type contexts are telescopes, i.e. `x1, ... , xi` is bounded in `Ai+1`.

*Labels and label contexts* &emsp;
A label context is a telescope of label definitions, separated with commas.
Each definition has the form of `L({x1:A1, ... , xn:An}, x:A --> M:B)`, where
`L` is the label name, 
`{x1:A1, ... , xn:An}` is a type context (of the free variables in the closure),
`x:A` is the bound variable of the closed function, and
`M:B` is the closed function's body and return type.
Note that `x1, ... , xn` and `x` are bound in `M`; 
`A1 , ... , An`, `M`, `A`, and `B` can refer to previous labels.

*Syntactic sugar* &emsp;
We support writing `A -> B` for non-dependent functions, 
which is interpreted as `Pi _:A.B`.
We allow a trailing comma at the end of non-empty lists of expressions,
like `x1:A1, ..., xn:An,` in contexts or `L0{x, y, z,}` in labels. 
We also have an experimental let-binding syntax `let x=M in N`
for illustration purpose, and it is *not* a part of the formal syntax.
The let-binding is de-sugared as `N[M/x]` (substitute x by M in N), 
so, it could accept ill-typed programs like `let x=nonsense in U0`.

*Unicode* &emsp;
We support the following unicode alternatives.

| Plain | Unicode |
|---|---|
| \ | λ |
|Pi | Π |
|-> | → |
|-->| ⟶ |

### Example: New functions in the type

In §3.3 we provided an example of functions appearing in the type of an expression,
even if the context and the expression itself do not contain that function. Namely,
```
    Γ ≜ ·, 𝐴: (𝑁𝑎𝑡 → 𝑁𝑎𝑡) → 𝑈0, 𝑎 : Π𝑓 : (𝑁𝑎𝑡 → 𝑁𝑎𝑡).𝐴 (𝜆𝑛 :𝑁𝑎𝑡 .1 + (𝑓 𝑛))
    𝑀 ≜ 𝑎 (𝜆𝑥 :𝑁𝑎𝑡 .1 + 𝑥)
    𝑁 ≜ 𝐴 (𝜆𝑛 :𝑁𝑎𝑡 .2 + 𝑛)
```
We have Γ ⊢ 𝑀 : 𝑁, because the inferred type of 𝑀 is (according to rule *D-Ty-Apply*)

    (𝐴 (𝜆𝑛 :𝑁𝑎𝑡 .1 + (𝑓 𝑛)))[(𝜆𝑥 :𝑁𝑎𝑡 .1 + 𝑥)/𝑓]

which is 

    𝐴 (𝜆𝑛 :𝑁𝑎𝑡 .1 + ((𝜆𝑥 :𝑁𝑎𝑡 .1 + 𝑥) 𝑛))

after the substitution, and it reduces to

    𝐴 (𝜆𝑛 :𝑁𝑎𝑡 .2 + 𝑛).

So, the type of 𝑀 contains a new function that does not exist in the
context Γ or the term 𝑀. We can replicate this example in our implementation
(It is also available under the name "New functions in type" on the webpage).

1. The implementation does not have built-in natural numbers, but we can provide
   an abstract signature of the natural number type, zero, and suc in 
   the source-language context.
```
N:U0, z:N, suc:N -> N,
```

2. Now, we input our example. We have 
```
A:Pi f:N -> N. U0,
a:Pi f:N -> N. A (\n:N.suc (f n))
```
in the context and 
```
a (\x:N. suc x)
```
as the term.

3. Clicking the source-language "Infer type" would give us the inferred and normalized
   type of our term, `A (λn:N. suc (suc n))`.

4. Click "Transform to DCC" to defunctionalize the example. There should be 4 functions
in the target language's label context.
```
L0({N:U0, suc:Π_:N.N, f:Π_:N.N}, n:N ⟶ suc @ (f @ n):N),
L1({N:U0, suc:Π_:N.N}, x:N ⟶ suc @ x:N),
L3({N:U0, suc:Π_:N.N}, n:N ⟶ suc @ (L1{N, suc} @ n):N),
L2({N:U0, suc:Π_:N.N}, n:N ⟶ suc @ (suc @ n):N)
```
The four labels have the following correspondence.

| Label | Corresponding lambda | From |
| ----- | -------------------- | ---- |
| `L0` | `\n:N.suc (f n)` | Context |
| `L1` | `\x:N. suc x` | Term |
| `L3` | `\n:N. suc ((\x:N. suc x) n)` | Inferred, un-normalized type |
| `L2` | `\n:N. suc (suc n)` | Inferred, normalized type |

5. The source-language type expression is transformed to `A @ L2{N, suc}`, and we can
click "Type check" to verify that the transformed term has this type, i.e. 
the transformation is type-preserving in this case. Since `L3` comes from the
inferred but not normalized type of the expression, the term should also type check
with `A @ L3{N, suc}`.

6. Note that the new function appeared in the type derivation is essentially
`\n:N.suc (f n)` with the free variable `f` substituted by `\x:N. suc x`, 
which corresponds to `L0{N, suc, L1{N, suc}}` in DCC. 
Indeed, the transformed term type checks with `A @ L0{N, suc, L1{N, suc}}`.
This implies that, `L2{N, suc}`, `L3{N, suc}`, and `L0{N, suc, L1{N, suc}}` 
are all equivalent in this particular DCC-context.

### Example: Weakenings

The target context enjoys the *weakening* property -
a well-typed expression is still well-typed in an extended type or label context.
We illustrate this property with the following example (Lemma 3.1, 3.2).

1. Click "Identity" to load the polymorphic identity function example. 
Defunctionalize it with the "Transform to DCC" button. The resulting label context should be
```
L1({A:U0}, x:A ⟶ x:A),
L0({}, A:U0 ⟶ L1{A}:(Πx:A.A))
```
and the type context is empty.
The transformed term is `L0{}` and its type is `ΠA:U0.Πx:A.A`.
We can verify that it type checks by clicking "Type check" on the target side.

2. Now, we weaken the target-language type context by adding variables like
```
A:U0, f:A -> A,
B:A -> U0, g: Pi x:A. B @ x
```
Similarily, we can weaken the label context with (well-formed) new labels like
```
M({}, A:U0 --> A : U0),
N({A:U0, f:A -> A}, x:A --> f @ x : A)
```
and it should still type check.

3. We say that a label context `D1` is a *subset* of another label context `D2`
if `D2` contains all label definitions that `D1` contains. This gives a stronger 
weakening property: a well-typed expression is still well-typed in a larger label context.
For example, our term is still well-typed in the following context.
```
M({}, A:U0 --> A : U0),
L1({A:U0}, x:A --> x:A),
N({A:U0, f:A -> A}, x:A --> f @ x : A),
L0({}, A:U0 --> L1{A}: A -> A)
```

### Build instructions

The implementation of our defunctionalizatin transformation is written in OCaml
and compiled to JavaScript with the `js_of_ocaml` package. We have included the
compiled JavaScript file `jsmain.js` in the repository, along with the corresponding
OCaml source code.

If you have installed OCaml, you can rebuild/modify the implementation with the following
commands.

- To compile/recompile `jsmain.js`, run `make all`. 
  Please make sure you have installed OCaml packages `menhir` and `js_of_ocaml` 
  (you can install them from `opam`).

- To clean the make files, run `make clean`.

- To rebuild the dependency graph, run `make depend`.
