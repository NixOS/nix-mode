let
  foo = 42;
  bar = let foo = 42; in
        let bar = 42; in
        42;
  baz = 42;
in
let foo = 42; in
let foo = 42;
    bar = 42;
in
let foo = let
      bar = 42;
    in
      42;
    bar = 42;
in
let
  foo = let
    bar = 42;
  in
    42;
  bar = 42;
in
let foo = 42;
in
{
  foo = let
    bar = 42;
  in
    42;

  foo = let bar = 42;
        in 42;

  foo = let bar = 42; in
        42;

  foo =
    let
      foo = 42;
      bar = 42;
    in
      42;
}
