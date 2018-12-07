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
    switch (Js.Json.classify(json)) {
    | Js.Json.JSONFalse => Yaml.Bool(false)
    | Js.Json.JSONTrue => Yaml.Bool(true)
    | Js.Json.JSONNull => Yaml.Null
    | Js.Json.JSONNumber(n) => Yaml.Float(n)
    | Js.Json.JSONString(s) => Yaml.String(s)
    | Js.Json.JSONArray(arr) =>
      Yaml.Array(Array.map(p, arr) |> Array.to_list)
    | Js.Json.JSONObject(o) =>
      Yaml.Object(
        Js.Dict.entries(o)
        |> Array.map(((key, value)) => (key, p(value)))
        |> Array.to_list,
      )
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