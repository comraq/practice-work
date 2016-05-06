import { EventEmitter } from "events";

import getClientConnection from "./clientConnection";
import getHostConnection from "./hostConnection";

const emitter = new EventEmitter();
let connection = null;

const setupConnection = conn => {
  conn.onReady(() => {
    connection = conn;
    emitter.emit("status");
  });

  conn.onMessage(msg => emitter.emit("message", msg));
}

// Exports

const isConnected = () => connection !== null;

const sendMessage = msg => connection.send(msg);

const onMessage = callback => emitter.addListener("message", callback);

const onStatusChange = callback => emitter.addListener("status", callback);

const offMessage = callback => emitter.removeListener("message", callback);

const offStatusChange = callback =>
                          emitter.removeListener("status", callback);

const host = () => {
  getHostConnection()
    .then(setupConnection)
    .catch(console.log.bind(console));
};

const join = () => {
  getClientConnection()
    .then(setupConnection)
    .catch(console.log.bind(console));
};

export { isConnected, sendMessage, onMessage, onStatusChange, offMessage,
         offStatusChange, host, join };
