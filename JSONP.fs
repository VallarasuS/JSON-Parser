
module JSONParser

open System

type JValue =
    | JString of string
    | JNumber of float
    | JObject of Map<string, JValue>
    | JArray of JValue list
    | JBool of bool
    | JNull