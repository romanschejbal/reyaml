open Jest;
open Expect;

test("It works", () =>
  ReYaml.readFileSync(~name="./sample.yml", `utf8)
  |> Yaml.parse
  |> ReYaml.process(
       ~vars=Js.Dict.fromList([("env", "dev"), ("name", "karel")]),
     )
  |> Yaml.stringify
  |> expect
  |> toEqual(ReYaml.readFileSync(~name="./sample.out.yml", `utf8))
);