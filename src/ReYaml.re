module Yaml = {
  type value =
    | Null
    | Bool(bool)
    | Float(float)
    | String(string)
    | Array(list(value))
    | Object(list((string, value)));
};

[@bs.module "yaml"] external yamlParse: string => Js.Json.t = "parse";
[@bs.module "yaml"] external yamlStringify: Js.Json.t => string = "stringify";

let parse = str => {
  let json = yamlParse(str);

  let rec p = json => {
    switch (Js.typeof(json)) {
    | "boolean" => Yaml.Bool(Obj.magic(json): bool)
    | "number" => Yaml.Float(Obj.magic(json): float)
    | "string" => Yaml.String(Obj.magic(json): string)
    | _ when Js.Array.isArray(json) =>
      let arr: array(Js.Json.t) = Obj.magic(json);
      let parsedArr = Array.map(p, arr) |> Array.to_list;
      Yaml.Array(parsedArr);
    | "object" =>
      if (Js.isNullable(Obj.magic(json))) {
        Yaml.Null;
      } else {
        let dict: Js.Dict.t(Js.Json.t) = Obj.magic(json);
        let entries = Js.Dict.entries(dict);
        let list =
          Array.map(
            ((key, value)) => {
              let keyString: string = Obj.magic(key);
              (keyString, p(value));
            },
            entries,
          )
          |> Array.to_list;
        Yaml.Object(list);
      }
    | _ => failwith("This should never happen")
    };
  };
  p(json);
};

let stringify = yaml => {
  let rec s = yaml =>
    switch (yaml) {
    | Yaml.Bool(value) => Js.Json.boolean(value)
    | Yaml.Float(number) => Js.Json.number(number)
    | Yaml.String(str) => Js.Json.string(str)
    | Yaml.Array(arr) => List.map(s, arr) |> Array.of_list |> Js.Json.array
    | Yaml.Object(list) =>
      Js.Dict.fromList(List.map(((key, value)) => (key, s(value)), list))
      |> Js.Json.object_
    | Yaml.Null => Js.Json.null
    };

  s(yaml) |> yamlStringify;
};