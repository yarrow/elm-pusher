const querystring = require("querystring");
exports.handler = async function (event, context) {
  const query = querystring.parse(event.body);
  log("Input", query);
  log("Event", event);
  log("Context", context);
  if (query.password !== process.env.PASSWORD) {
    return {
      statusCode: 401,
      headers: {
        "content-type": "text/plain; charset=UTF-8",
      },
      body: "Password didn't match",
    };
  }

  const Pusher = require("pusher");
  const pusher = new Pusher({
    appId: process.env.PUSHER_APP_ID,
    key: process.env.PUSHER_KEY,
    secret: process.env.PUSHER_SECRET,
    cluster: process.env.PUSHER_CLUSTER,
    useTLS: false,
  });

  const presenceData = {
    user_id: query.socket_id,
    user_info: { name: query.name },
  };
  const auth = pusher.authenticate(
    query.socket_id,
    query.channel_name,
    presenceData
  );
  return {
    statusCode: 200,
    headers: {
      "content-type": "application/json; charset=UTF-8",
      "access-control-allow-origin": "*",
      "access-control-expose-headers":
        "content-encoding,date,server,content-length",
    },
    body: JSON.stringify(auth),
  };
};
function log(tag, obj) {
  msg = `"${tag}:" ${JSON.stringify(obj, null, 2)}`;
  console.log(msg);
}
