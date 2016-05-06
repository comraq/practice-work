import { Server as WebsocketServer } from "ws";

const server = new WebsocketServer({ port: 3210 });
new Promise((resolve, reject) => {
  server.on("connection", socket => {
    socket.on("message", resolve);
  });
})
.then(msg => {
  server.clients.forEach(other => {
    if (other === socket)
      return;

    other.send(msg);
  });
});
