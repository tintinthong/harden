import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

var storageKey = "store";
var flags = localStorage.getItem(storageKey);

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});

console.log(JSON.stringify(flags, null, 2));
console.log(JSON.stringify(Elm, null, 2));
console.log(JSON.stringify(app, null, 2));

app.ports.storeCache.subscribe(function (val) {
  if (val === null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(val));
  }

  // Report that the new session was stored successfully.
  setTimeout(function () {
    app.ports.onStoreChange.send(val);
  }, 0);
});

// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener(
  "storage",
  function (event) {
    if (event.storageArea === localStorage && event.key === storageKey) {
      app.ports.onStoreChange.send(event.newValue);
    }
  },
  false
);
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
