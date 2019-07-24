import { Elm } from './Main.elm'


const app = Elm.Main.init({
    node: document.getElementById('elm')
})

const listen = url => {
    const socket = new WebSocket(url)

    socket.onmessage = event => {
        app.ports.receive.send(event.data)
    }
}

app.ports.listen.subscribe(listen)