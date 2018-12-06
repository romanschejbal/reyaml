open Jest;
open Expect;
open ReYaml;

test("Boolean", () =>
  parse("true") |> expect |> toEqual(Yaml.Bool(true))
);

test("Boolean", () =>
  parse("false") |> expect |> toEqual(Yaml.Bool(false))
);

test("Float", () =>
  parse("1") |> expect |> toEqual(Yaml.Float(1.))
);

test("String", () =>
  parse("tenis") |> expect |> toEqual(Yaml.String("tenis"))
);

test("Array", () =>
  parse("- tenis") |> expect |> toEqual(Yaml.Array([String("tenis")]))
);

test("Array", () =>
  parse("- tenis\n- aloha")
  |> expect
  |> toEqual(Yaml.Array([String("tenis"), String("aloha")]))
);

test("Object", () =>
  parse("tenis: 1")
  |> expect
  |> toEqual(Yaml.Object([("tenis", Yaml.Float(1.))]))
);

test("More complex object", () =>
  parse("first: 1\nsecond:\n - 1\n - str\nthird:\n here: 2\n there: true")
  |> expect
  |> toEqual(
       Yaml.(
         Object([
           ("first", Float(1.)),
           ("second", Array([Float(1.), String("str")])),
           (
             "third",
             Object([("here", Float(2.)), ("there", Bool(true))]),
           ),
         ])
       ),
     )
);

test("Stringify Float", () =>
  stringify(Yaml.Float(10.)) |> expect |> toEqual("10\n")
);

test("Stringify String", () =>
  stringify(Yaml.String("Ahoj")) |> expect |> toEqual("Ahoj\n")
);

test("Stringify Array", () =>
  stringify(Yaml.Array([Float(1.), Float(2.)]))
  |> expect
  |> toEqual("- 1\n- 2\n")
);

test("Stringify Object", () =>
  stringify(
    Yaml.(
      Object([
        ("first", Float(1.)),
        ("second", Array([Float(1.), String("str")])),
        ("third", Object([("here", Float(2.))])),
      ])
    ),
  )
  |> expect
  |> toEqual("first: 1\nsecond:\n  - 1\n  - str\nthird:\n  here: 2\n")
);

test("Comments", () =>
  parse("<<: more\n# whaat\nsrani: 1\n")
  |> stringify
  |> expect
  |> toEqual("<<: more\nsrani: 1\n")
);

test("Empty yaml", () =>
  parse("") |> stringify |> expect |> toEqual("")
);
test("Only one comment yaml", () =>
  parse("# comment") |> expect |> toEqual(Yaml.Null)
);