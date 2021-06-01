import Elm from '../.elm-spa/defaults/Main.elm'
import { v4 as uuidv4 } from 'uuid';

let uuid;
try {
    const storage = window.sessionStorage;
    uuid = storage.getItem("myUuid");
    if (!uuid) {
        uuid = uuidv4();
        storage.setItem("myUuid", uuid);
    }
}
catch (e) {
    uuid = uuidv4();
}

var app = Elm.Main.init({flags: uuid});

let pusher;
let channel;
app.ports.connect.subscribe((authParams) => {
  pusher = new Pusher("b470a1b48d9579a05f7f", {
    authEndpoint: "/.netlify/functions/auth-pusher",
    auth: { params: authParams },
    cluster: "us2",
  });
  channel = pusher.subscribe("presence-main");
  bind_subscription_succeeded("presence-main");
  bind_subscription_error("presence-main");
});

// Pusher (https://pusher.com/docs/channels/using_channels/events#binding-with-optional-this-context)
// says you can pass a `context` optional argument, as in:
//     channel.bind(handler, context)
// and inside handler, `this.field` will resolve to `context.field`
// I haven't been able to make this work, possibly because the event is generated on a different
// path (resulting from `pusher_internal:subscription_succeeded`)?
//
// So we capture the channel name via a closure instead.
function bind_subscription_succeeded(channelName) {
  channel = pusher.channel(channelName);
  channel.bind("pusher:subscription_succeeded", (members) => {
    let memberList = [];
    members.each((member) => {
      memberList.push({ uid: member.id, data: member.info });
    });
    const result = {
      channel: channelName,
      event: "pusher:subscription_succeeded",
      me: { uid: members.me.id, data: members.me.info },
      members: memberList,
    };
    log("subscription succeeded", result);
    app.ports.pusher.send(result);
  });
}

function bind_subscription_error(channelName) {
channel = pusher.channel(channelName);
channel.bind("pusher:subscription_error", (err) => {
    const result = {
    channel: channelName,
    event: "pusher:subscription_error",
    data: err,
    };
    log("subscription error", result);
    app.ports.pusher.send(result);
});
}

function log(tag, obj) {
  const msg = `"${tag}" ${JSON.stringify(obj, null, 2)}`;
  console.log(msg);
}
