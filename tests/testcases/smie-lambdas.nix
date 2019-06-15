{ foo, bar }:
{ foo, bar } @ baz:
foo: bar:

{ test0 = x:
    42;

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

  test5 = x: y: z:
    42;

  test6 = map (x: y: z:
    42) [ 1 2 3 ];

  test7 = map
    (x:
      42);

  test7 = map
    (x: y: z:
      42);

  test8 =
    x:
    y:
    z:
    42;
}
