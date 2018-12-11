let readFile = filename => {
  let ic = open_in(filename);
  let len = in_channel_length(ic);
  let str = really_input_string(ic, len);
  close_in(ic);
  str;
};

let readYamls = str =>
  Str.split(Str.regexp("^---$"), str)
  |> List.map(str =>
       Yaml.of_string(str)
       |> (
         fun
         | Ok(r) => r
         | Error(e) => Rresult.R.failwith_error_msg(Error(e))
       )
     );

let getFilenamesFromArgs = args =>
  Array.sub(args, 1, Array.length(args) - 1)
  |> (
    fun
    | [||] => failwith("Filename not provided")
    | files => files
  );

let importOne = filename => {
  let included = readFile(filename) |> readYamls |> List.hd;
  switch (included) {
  | `Null => included
  | `O(_) => included
  | _ =>
    failwith(
      "Error when importing: "
      ++ filename
      ++ " - only objects are allowed as imports",
    )
  };
};

let replaceVariables = (vars, str) =>
  Array.fold_left(
    (final, entry) =>
      switch (entry) {
      | [key, value] =>
        Str.global_replace(Str.regexp_string("$" ++ key), value, final)
      | _ => final
      },
    str,
    vars,
  );

let rec process = (~vars=[||], yaml) =>
  switch (yaml) {
  | `O(list) =>
    `O(
      List.fold_right(
        (next, final) =>
          switch (next) {
          | ("<|", `String(filename)) =>
            let imported =
              replaceVariables(vars, filename)
              |> importOne
              |> process(~vars)
              |> (
                fun
                | `O(list) => list
                | _ => []
              );
            List.append(imported, final);
          | (key, value) => [(key, process(~vars, value)), ...final]
          },
        list,
        [],
      ),
    )
  | `A(values) => `A(List.map(process(~vars), values))
  | `String(s) =>
    let replaced = replaceVariables(vars, s);
    switch (float_of_string(replaced)) {
    | number when replaced !== s => `Float(number)
    | exception _ => `String(replaced)
    | _ => yaml
    };
  | x => x
  };

let (+/) = Filename.concat;

let start = () => {
  let args = Sys.argv;
  let vars =
    Unix.environment() |> Array.map(Str.split(Str.regexp_string("=")));
  let originalCwd = Sys.getcwd();
  let filenames = getFilenamesFromArgs(args);
  Array.mapi(
    (i, filename) => {
      if (i > 0) {
        print_endline("---");
      };
      print_endline("# generated from: " ++ filename);
      let newCwd = Filename.dirname(originalCwd +/ filename);
      Sys.chdir(newCwd);
      let filename = Filename.basename(filename);
      let yamls = readFile(filename) |> readYamls;
      let processed =
        List.map(process(~vars), yamls)
        |> List.filter(
             fun
             | `Null => false
             | `O([]) => false
             | _ => true,
           )
        |> Array.of_list;
      Array.fold_left(
        (final, yaml) =>
          Yaml.to_string_exn(yaml)
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
      |> print_endline;
    },
    filenames,
  );
};

start();