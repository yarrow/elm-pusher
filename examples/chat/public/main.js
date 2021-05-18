import { v4 as uuidv4 } from 'uuid';

const uuid = uuidv4();

Elm.Main.init({flags: uuid});

