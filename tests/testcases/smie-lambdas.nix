{ foo, bar }:
{ foo, bar } @ baz:
foo: bar:

{
  test1 =
    { f = x:
        42 };

  test2 = x:
    42;

  test3 = map (x:
    42) [ 1 2 3 ];

  test4 =
    { f = x: y: z:
        42 };

  test2 = x: y: z:
    42;

  test3 = map (x: y: z:
    42) [ 1 2 3 ];
}

