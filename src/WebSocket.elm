port module WebSocket exposing (close, listen, receive)


port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg


port close : () -> Cmd msg
