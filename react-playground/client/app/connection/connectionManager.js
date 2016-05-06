import { EventEmitter } from "events";

import ClientConnection from "./clientConnection";
import HostConnection from "./hostConnection";

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

const host = () => setupConnection(HostConnection());

const join = () => setupConnection(ClientConnection());

export { isConnected, sendMessage, onMessage, onStatusChange, offMessage,
         offStatusChange, host, join };
