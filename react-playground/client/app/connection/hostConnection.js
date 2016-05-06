import SimplePeer from "simple-peer";
import SimpleWebsocket from "simple-websocket";
import { EventEmitter } from "events";

const peers = [];
const emitter = new EventEmitter();

export default () => {
  let socket = new SimpleWebsocket("ws://localhost:3210");
  socket.on("close", () => console.log("Socket Closed!"));
  socket.on("error", (err) => {
    console.log("Socket Error!")
    console.log(err);
  });
  socket.on("connect", () => console.log("Socket Connected!"));

  socket.on("data", (data) => {
    let rtc = new SimplePeer({
      initiator: false,
      trickle: false
    });

    rtc.signal(data);
    rtc.on("signal", data => socket.send(data));
    rtc.on("connect", () => peers.push(rtc));
 
    rtc.on("data", msg => {
      emitter.emit("message", msg);
 
      //as host, we need to broadcast the data to the other peers
      peers.forEach(p => {
        if(p === rtc)
          return;
 
        p.send(msg);
      });
    });
  });

  return {
    onReady: callback => callback(),

    send: msg => {
      peers.forEach(p => p.send(msg));
    },

    onMessage: callback => emitter.addListener("message", callback)
  };
}
