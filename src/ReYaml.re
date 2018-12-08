[@bs.module "yargs"] external yargs: Js.Dict.t('a) = "argv";
[@bs.module "fs"]
external readFileSync:
  (~name: string, [@bs.string] [ | `utf8 | `ascii]) => string =
  "";

let getFilename = yargs =>
  Js.Dict.unsafeGet(yargs, "_")
  |> (
    fun
    | [||] => failwith("Filename not provided")
    | [|file|] => file
    | _ => failwith("Too many filenames provided")
  );

let import = filename => {
  let included = readFileSync(~name=filename, `utf8) |> Yaml.parse;
  switch (included) {
  | `Object(_) => included
  | _ => failwith("Only objects are allowed as imports")
  };
};

let replaceVariables = (vars, str) => {
  let vars = Js.Dict.entries(vars);
  Array.fold_left(
    (final, entry) =>
      switch (entry) {
      | ("_", _) => final
      | (key, value) => Js.String.replace("$" ++ key, value, final)
      },
    str,
    vars,
  );
};

let rec process = (~vars=Js.Dict.empty(), yaml) => {
  switch (yaml) {
  | `Object(list) =>
    `Object(
      List.fold_right(
        (next, final) =>
          switch (next) {
          | ("<|", `String(filename)) =>
            let imported =
              replaceVariables(vars, filename)
              |> import
              |> process(~vars)
              |> (
                fun
                | `Object(list) => list
                | _ => []
              );
            List.append(imported, final);
          | (key, value) => [(key, process(~vars, value)), ...final]
          },
        list,
        [],
      ),
    )
  | `Array(values) => `Array(List.map(process(~vars), values))
  | `String(s) => `String(replaceVariables(vars, s))
  | x => x
  };
};

let start = () => {
  let file = readFileSync(~name=getFilename(yargs), `utf8);
  let split = Js.String.split(Obj.magic([%bs.re "/^---\\n/gm"]), file);
  let yamls = Array.map(Yaml.parse, split);
  let processed = Array.map(process(~vars=yargs), yamls);
  Array.fold_left(
    (final, yaml) =>
      Yaml.stringify(yaml)
      |> (
        str =>
          switch (final) {
          | "" => str
          | _ => final ++ "---\n" ++ str
          }
      ),
    "",
    processed,
  )
  |> Js.log;
};