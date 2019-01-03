let
  x = [
    (let y = 1; in y)
    { foo = 1; }
    [ 1 2 3 ]
    x
  ];
in x
