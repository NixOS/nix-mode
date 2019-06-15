{
  test1 =
    { foo = {
        bar = 42;
      };
    };

  test2 =
    {
      foo = {
        bar = 42;
      };
    };

  test3 = {
    foo = { bar = 42;
            baz = 42;
          };
  };

  test4 = [
    {
      foo = 42;
    }
  ];

  test5 = [
    { foo = 42;
      bar = 42;
    }
  ];

  test6 =
    [ { foo = 42;
        bar = 42;
      }
    ];

  test7 =
    [
      {
        foo = 42;
        bar = 42;
      }
    ];

  test8 = (foo:
    42
  );
}
