[@bs.module "yargs"] external yargs: Js.Dict.t('a) = "argv";
[@bs.module "fs"]
external readFileSync:
  (~name: string, [@bs.string] [ | `utf8 | `ascii]) => string =
  "";

let getFilenames = yargs =>
  Js.Dict.unsafeGet(yargs, "_")
  |> (
    fun
    | [||] => failwith("Filename not provided")
    | files => files
  );

let import = filename => {
  let included = readFileSync(~name=filename, `utf8) |> Yaml.parse;
  switch (included) {
  | `Null => included
  | `Object(_) => included
  | _ =>
    failwith(
      "Error when importing: "
      ++ filename
      ++ " - only objects are allowed as imports",
    )
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

let (+/) = Node.Path.join2;

[@bs.val] [@bs.scope "process"] external chdir: string => unit = "chdir";

let start = () => {
  let originalCwd = Node.Process.cwd();
  let filenames = getFilenames(yargs);
  Array.mapi(
    (i, filename) => {
      if (i > 0) {
        Js.log("---");
      };
      Js.log("# generated from: " ++ filename);
      let newCwd = Node.Path.dirname(originalCwd +/ filename);
      chdir(newCwd);
      let filename = Node.Path.basename(filename);

      let file = readFileSync(~name=filename, `utf8);
      let split = Js.String.split(Obj.magic([%bs.re "/^---\\n/gm"]), file);
      let yamls = Array.map(Yaml.parse, split);
      let processed =
        Array.map(process(~vars=yargs), yamls)
        |> Array.to_list
        |> List.filter(
             fun
             | `Null => false
             | `Object([]) => false
             | _ => true,
           )
        |> Array.of_list;
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
    },
    filenames,
  );
};