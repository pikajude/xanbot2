const socket = new WebSocket('ws://localhost:8080');

socket.addEventListener('open', (ev) => {
    console.log(ev);
});

socket.addEventListener('message', (ev) => {
    console.log(ev.data);
})
