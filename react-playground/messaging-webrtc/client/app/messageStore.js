import { EventEmitter } from "events";

const emitter = new EventEmitter();

const event = "update";
const messages = [];

const getMessages = () => messages.slice(0);

const subscribe = listener => emitter.addListener(event, listener);

const unsubscribe = listener => emitter.removeListener(event, listener);

const newMessage = message => {
  messages.push(message);
  emitter.emit(event);
};

export { getMessages, subscribe, unsubscribe, newMessage };
