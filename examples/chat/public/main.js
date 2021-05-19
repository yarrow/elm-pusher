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

Elm.Main.init({flags: uuid});
