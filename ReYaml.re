let source = Sys.argv[1];
let env = Array.length(Sys.argv) > 2 ? Sys.argv[2] : "";

let loadFile = filename => {
  let ic = open_in(filename);
  let len = in_channel_length(ic);
  let str = really_input_string(ic, len);
  close_in(ic);
  str;
};

let loadYamls = filename =>
  loadFile(filename)
  |> Str.split(Str.regexp_string("---"))
  |> List.map(Yaml.of_string_exn);

let loadYaml = filename => List.hd(loadYamls(filename));

let rec parse = (currentFilename, value) => {
  let parseObject = obj =>
    List.fold_right(
      (next, l) =>
        switch (next) {
        | ("<<", `String(filename)) =>
          let included = loadYaml(filename) |> parse(filename);
          switch (included) {
          | `O(o) => List.append(o, l)
          /* List.append(List.map(((key, v)) => (key, parse(v)), o), l) */
          | _ =>
            failwith(
              "Error when parsing "
              ++ currentFilename
              ++ ", trying to import "
              ++ filename
              ++ " - only objects are allowed as imports.",
            )
          };
        | (key, v) => [(key, parse(currentFilename, v)), ...l]
        },
      obj,
      [],
    );

  switch (value) {
  | `O(o) => `O(parseObject(o))
  | `A(l) => `A(List.map(parse(currentFilename), l))
  | x => x
  };
};

let output =
  loadYamls(source)
  |> List.map(parse(source))
  |> List.map(Yaml.to_string_exn)
  |> String.concat("---\n");

print_endline(output);