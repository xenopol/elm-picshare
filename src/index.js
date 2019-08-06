import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("elm")
});

let socket = null;

const listen = url => {
  if (!socket) {
    socket = new WebSocket(url);
    socket.onmessage = event => {
      app.ports.receive.send(event.data);
    };
  }
};

const close = () => {
  if (socket) {
    socket.close();
    socket = null;
  }
};

app.ports.listen.subscribe(listen);
app.ports.close.subscribe(close);
