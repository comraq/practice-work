import SimplePeer from "simple-peer";
import SimpleWebsocket from "simple-websocket";
import { EventEmitter } from "events";

const emitter = new EventEmitter();

export default () => {
  let socket = new SimpleWebsocket("ws://localhost:3210");
  socket.on("close", () => console.log("Socket Closed!"));
  socket.on("error", err => {
    console.log("Socket Error!")
    console.log(err);
  });
  socket.on("connect", () => {
    let rtc = new SimplePeer({
      initiator: true,
      trickle: false
    });
    rtc.on("signal", data => socket.send(data));

    rtc.on("connect", () => {
      emitter.emit("connected");

      // Socket Signaler is no longer necessary
      socket.destroy();
    });
    
    rtc.on("data", msg => emitter.emit("message", msg));

    socket.on("data", data => rtc.signal(data));
  });

  return {
    onReady: callback => emitter.addListener("connected", callback),

    send: msg => rtc.send(msg),

    onMessage: callback => emitter.addListener("message", callback)
  };
}
